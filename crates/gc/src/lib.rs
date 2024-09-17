//! # Garbage Collector
//!
//! ## Structure
//!
//! The heap can be upto 4GB, the size addressable with a u32.
//! We allocate the maximum size using `mmap`, this gives us a
//! pointer to a 4GB section of memory, but due to virtual memory
//! it will only actually be used when accessed for the first time.
//!
//! The start of the heap contains all the page descriptors containing
//! all the metadata for each page. The later pages are used for the
//! actual data. Pages are 4KB in size. This means that the metadata is
//! not interspersed with the data, but can easily be accessed by calculating
//! the correct index.
//!
//! The first couple of page descriptors are used as roots for the free
//! lists, so are not actually used for allocation. Page descriptors are not
//! initialised upfront, when the heap is created a page descriptor is created
//! for an initial ream and pages are split from it. The initial ream runs out
//! a new ream is created and added to the free list.
//!
//! ## Allocations
//!
//! Allocations can either be small (blocks) or large (reams). The small
//! allocations are less than a page and rounded up to the next power of 2.
//! They are allocated onto a page which only contains that specific size.
//! Large allocations are larger than a page and rounded up to the page boundary.
//!
//! We keep multiple free lists to deal with different types of pages. The free ream
//! list contains empty reams, the free page list contains empty pages, and there is a
//! free list for each of the block sizes, which are partially filled with blocks. We
//! also keep a list of full pages/ reams so they are easy to access.
//!
//! ### Garbage Collection
//!
//! The actual garbage collection algorithm is mark and sweep. First all
//! pages are marked as unused, by marking their `gc_bits` in
//! [`Heap::start_gc()`]. Then objects are traced by the interpreter and
//! marked as used, setting the relevant `gc_bits` for the page (using
//! [`Heap::mark()`]). Then the free lists are updated by [`Heap::finish_gc()`]
//! to ensure that they are correct (the sweep phase). We only consider
//! pages which are in a free, or full list to avoid processing pages which
//! are not in use.
//!
//! We do not yet compact consecutive empty pages back into reams.

#![no_std]
#![feature(strict_provenance)]

use core::{fmt, marker::PhantomData, num::NonZero, ops, panic};

mod pages;
use pages::{BlockClass, PageDescriptor, PageDescriptorRef, PageList};

/// The max size 4GB; Addressable with a u32
const HEAP_SIZE: usize = 4 * 1024 * 1024 * 1024;
const _ASSERT_SIZE: () = assert!(HEAP_SIZE == (u32::MAX as usize) + 1);

/// The length of a page - 4KB
const PAGE_SIZE: usize = 4 * 1024;

/// The number of pages in the heap
const PAGE_COUNT: usize = HEAP_SIZE / (PAGE_SIZE + PageDescriptor::SIZE);

/// The number of pages required to hold the page descriptors
const PD_PAGES: usize = bytes_to_pages(PAGE_COUNT * PageDescriptor::SIZE);

/// The number of bytes the page descriptor section takes up
const PD_SECTION_SIZE: usize = PD_PAGES * PAGE_SIZE;

/// The largest allocation we can allocate
const MAX_REAM_SIZE: usize = (u16::MAX as usize + 1) * PAGE_SIZE;

const fn bytes_to_pages(n: usize) -> usize {
  if n % PAGE_SIZE != 0 {
    n / PAGE_SIZE + 1
  } else {
    n / PAGE_SIZE
  }
}

/// A heap.
///
/// Manages a garbage-collected heap
pub struct Heap {
  raw: RawMemory,

  free_ream_list: PageList,
  free_page_list: PageList,
  size_class_lists: [PageList; BlockClass::SMALL_CLASS_COUNT],

  full_list: PageList,
}
impl Heap {
  /// Creates a new heap.
  ///
  /// Requests a 4GB region of memory from `mmap`, and initialises the
  /// garbage collector data structures.
  #[must_use]
  pub fn new() -> Option<Self> {
    let mut raw = RawMemory::new()?;
    let base = raw.base();
    let first_ream = raw.materialize_new_ream();

    let mut page_count: u16 = 1; // we start at 1, as virtual pointer can't be 0
    let mut new_page = || {
      let page = PageList::new(page_count, base);
      page_count += 1;
      page
    };

    let mut allocator = Self {
      raw,

      free_ream_list: new_page(),
      free_page_list: new_page(),
      size_class_lists: [
        new_page(),
        new_page(),
        new_page(),
        new_page(),
        new_page(),
        new_page(),
        new_page(),
      ],

      full_list: new_page(),
    };

    let (_, actual_first) = first_ream.split(page_count)?;
    allocator.free_ream_list.push(actual_first?);

    Some(allocator)
  }

  /// Allocates a block of at least `bytes` bytes.
  ///
  /// Returns a virtual pointer to the allocated memory.
  ///
  /// # Panics
  /// The function panics if the allocation request is larger than
  /// the largest allocation size ([`MAX_REAM_SIZE`]).
  pub fn alloc(&mut self, bytes: usize) -> Gc<u8> {
    if bytes <= PAGE_SIZE {
      let class = BlockClass::get(bytes);
      self.alloc_small(class)
    } else if bytes <= MAX_REAM_SIZE {
      let ream = self.alloc_ream(bytes);
      self.full_list.push(ream);
      ream.data_pointer()
    } else {
      panic!("unreasonable allocation request: {bytes} bytes");
    }
  }

  /// Allocates a ream with enough pages to hold `bytes` bytes.
  fn alloc_ream(&mut self, bytes: usize) -> PageDescriptorRef {
    if self.free_ream_list.is_empty() {
      self.free_ream_list.push(self.raw.materialize_new_ream());
    }

    let page_count = bytes_to_pages(bytes).try_into().unwrap();
    for page in self.free_ream_list.iter() {
      let Some((ream, rest)) = page.split(page_count) else {
        continue;
      };

      if let Some(rest) = rest {
        self.free_ream_list.push(rest);
      }

      return ream;
    }

    panic!("Ran out of pages");
  }

  /// Allocates a block of size `class`
  fn alloc_small(&mut self, class: BlockClass) -> Gc<u8> {
    let mut page = match self.size_class_lists[class as usize - 6].first() {
      Some(first) => first,
      None => self.get_new_page(class),
    };

    let idx = page.take_next_block();

    if page.is_full() {
      self.full_list.push(page);
    }

    page.data_pointer().add(idx * class.block_size())
  }

  fn get_new_page(&mut self, class: BlockClass) -> PageDescriptorRef {
    let mut page = match self.free_page_list.first() {
      Some(first) => first,
      None => self.alloc_ream(1),
    };

    page.set_class(class);
    page.set_empty();
    self.size_class_lists[class as usize - 6].push(page);
    page
  }

  /// Begins a garbage collection, clearing the `gc_bits` of all used pages.
  ///
  /// # Safety
  ///
  /// This function leaves the heap in a broken state, and [`Heap::mark()`] and
  ///  [`Heap::finish_gc()`] need to be used to return it to a usable state. Using
  /// alloc before garbage collection is finished is forbidden.
  pub fn start_gc(&mut self) {
    for mut page in self.full_list.iter() {
      page.set_empty();
    }

    for size in &self.size_class_lists {
      for mut page in size.iter() {
        page.set_empty();
      }
    }
  }

  /// Marks a pointer as used, as part of garbage collection
  pub fn mark(&mut self, ptr: Gc<u8>) {
    PageDescriptorRef::from_index(ptr.page_index(), self.raw.base()).mark(ptr.block_index());
  }

  /// Finishes a garbage collection
  ///
  /// Returns all unused pages to the appropriate free lists.
  pub fn finish_gc(&mut self) {
    for page in self.full_list.iter() {
      if page.is_empty() {
        if page.pages() > 0 {
          self.free_ream_list.push(page);
        } else {
          self.free_page_list.push(page);
        }
      } else if page.is_full() {
        // Do nothing, as page is already in full_list
      } else {
        debug_assert!(!page.is_ream()); // Ream cannot be partially full
        self.size_class_lists[page.class() as usize - 6].push(page);
      }
    }

    for size_list in &self.size_class_lists {
      for page in size_list.iter() {
        if page.is_empty() {
          self.free_page_list.push(page);
        }
      }
    }
  }
}
impl<T> ops::Index<Gc<T>> for Heap {
  type Output = T;

  fn index(&self, index: Gc<T>) -> &Self::Output {
    unsafe { &*self.raw.base().add(index.addr()).cast() }
  }
}
impl<T> ops::IndexMut<Gc<T>> for Heap {
  fn index_mut(&mut self, index: Gc<T>) -> &mut Self::Output {
    unsafe { &mut *self.raw.base().add(index.addr()).cast() }
  }
}
impl fmt::Debug for Heap {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Heap({:x?})", self.raw.base())
  }
}
struct RawMemory {
  mmap: mmap_rs::MmapMut,
  used_pages: usize,
}
impl RawMemory {
  fn new() -> Option<Self> {
    Some(Self {
      mmap: mmap_rs::MmapOptions::new(HEAP_SIZE).ok()?.map_mut().ok()?,
      used_pages: 0,
    })
  }

  /// The base address of the heap
  fn base(&self) -> *mut u8 {
    self.mmap.as_ptr().cast_mut()
  }

  /// Creates a new maximum-size ream by creating pages for it.
  fn materialize_new_ream(&mut self) -> PageDescriptorRef {
    assert!(self.used_pages < PAGE_COUNT, "Ran out of pages");

    let pages = self.used_pages;
    self.used_pages += u16::MAX as usize;

    let mut ream = PageDescriptorRef::from_index(pages, self.base());
    ream.set_ream(u16::MAX);
    ream
  }
}

/// A virtual pointer to a memory allocation on the garbage collected heap.
#[must_use]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Gc<T> {
  value: NonZero<u32>,
  _type: PhantomData<T>,
}
impl<T> Gc<T> {
  /// Creates a new pointer to a value
  ///
  /// SAFETY:
  /// The value must be to a valid allocation in the data section of the heap
  #[allow(clippy::missing_panics_doc, reason = "u32 < usize")]
  pub fn new(value: NonZero<u32>) -> Self {
    // check that the pointer is to the data section
    debug_assert!(value.get() > PD_SECTION_SIZE.try_into().unwrap());

    Self {
      value,
      _type: PhantomData,
    }
  }

  /// The address of the pointer
  #[must_use]
  #[allow(clippy::missing_panics_doc, reason = "u32 < usize")]
  pub fn addr(self) -> usize {
    self.value.get().try_into().unwrap()
  }
  /// Casts the pointer to a different type.
  pub fn cast<U>(self) -> Gc<U> {
    Gc {
      value: self.value,
      _type: PhantomData,
    }
  }

  /// The index of the block in the page this pointer is to
  fn block_index(self) -> usize {
    self.addr() % PAGE_SIZE
  }
  /// The index of the page that this pointer is in
  fn page_index(self) -> usize {
    (self.addr() - PD_SECTION_SIZE) / PAGE_SIZE
  }

  /// Adds an offset to the pointer
  fn add(self, offset: usize) -> Self {
    Gc {
      value: self.value.checked_add(offset.try_into().unwrap()).unwrap(),
      _type: PhantomData,
    }
  }
}
impl<T> fmt::Debug for Gc<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "Gc({:x?})", self.value.get())
  }
}

#[cfg(test)]
#[cfg_attr(miri, ignore)]
mod test;
