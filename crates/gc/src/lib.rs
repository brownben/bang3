//! # Garbage Collector
//!
//! ## Structure
//!
//! The heap can be upto 4GB, the size addressable with a u32.
//! We allocate the maximum size of heap this gives us a
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

use std::{alloc, fmt, marker::PhantomData, mem, num::NonZero, ops, panic, slice};

mod display;
mod pages;
use pages::{BlockClass, PageDescriptor, PageDescriptorRef, PageList};

/// The max size 4GB; Addressable with a u32
const HEAP_SIZE: usize = 4 * 1024 * 1024 * 1024;
const _ASSERT_SIZE: () = assert!(HEAP_SIZE == (u32::MAX as usize) + 1);

/// The length of a page - 4KB
pub const PAGE_SIZE: usize = 4 * 1024;

/// The number of pages in the heap
pub const PAGE_COUNT: usize = HEAP_SIZE / (PAGE_SIZE + PageDescriptor::SIZE);

/// The number of pages required to hold the page descriptors
const PD_PAGES: usize = bytes_to_pages(PAGE_COUNT * PageDescriptor::SIZE);

/// The number of bytes the page descriptor section takes up
const PD_SECTION_SIZE: usize = PD_PAGES * PAGE_SIZE;

/// The largest allocation we can allocate
pub const MAX_REAM_SIZE: usize = (u16::MAX as usize + 1) * PAGE_SIZE;

const fn bytes_to_pages(n: usize) -> usize {
  if n % PAGE_SIZE != 0 {
    n / PAGE_SIZE + 1
  } else {
    n / PAGE_SIZE
  }
}

/// The size of the heap to provision
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum HeapSize {
  /// The standard full 4GB heap
  #[default]
  Standard,
  /// A small heap, with a maximum size of a ream (256MB)
  /// Primariliy for testing, and creating many heaps at once
  Small,
}
impl HeapSize {
  fn bytes(self) -> usize {
    match self {
      HeapSize::Standard => HEAP_SIZE,
      HeapSize::Small => MAX_REAM_SIZE,
    }
  }
}

/// A heap.
///
/// Manages a garbage-collected heap
#[derive(Debug)]
pub struct Heap {
  raw: RawMemory,

  free_ream_list: PageList,
  free_page_list: PageList,
  size_class_lists: [PageList; BlockClass::SMALL_CLASS_COUNT],

  full_list: PageList,
  full_list_len: usize,

  is_collecting: bool,
}
impl Heap {
  /// Creates a new heap.
  ///
  /// Requests a 4GB region of memory, and initialises the
  /// garbage collector data structures.
  #[must_use]
  pub fn new(size: HeapSize) -> Option<Self> {
    let mut raw = RawMemory::new(size.bytes())?;
    let base = raw.base;
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
      full_list_len: 0,

      is_collecting: false,
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
  fn allocate_bytes(&mut self, bytes: usize) -> Gc<u8> {
    debug_assert!(!self.is_collecting);

    if bytes <= PAGE_SIZE {
      let class = BlockClass::get(bytes);
      self.alloc_small(class)
    } else if bytes <= MAX_REAM_SIZE {
      let ream = self.alloc_ream(bytes);
      self.full_list.push(ream);
      self.full_list_len += 1;
      ream.data_pointer()
    } else {
      panic!("Allocation Too Large: can't allocate {bytes} bytes");
    }
  }

  /// Allocates a type to the heap, and returns a virtual pointer to the allocated memory.
  ///
  /// # Panics
  /// The function panics if the allocation request is larger than
  /// the largest allocation size ([`MAX_REAM_SIZE`]).
  pub fn allocate<T: Sized>(&mut self, value: T) -> Gc<T> {
    debug_assert!(!self.is_collecting);

    let allocation_size = mem::size_of::<T>();
    let pointer = self.allocate_bytes(allocation_size).cast();
    self[pointer] = value;
    pointer
  }

  /// Allocates a list of type T to the heap,
  /// and returns a virtual pointer to the header of the list.
  ///
  /// # Panics
  /// The function panics if the allocation request is larger than
  /// the largest allocation size ([`MAX_REAM_SIZE`]).
  pub fn allocate_list_header<T: Sized>(&mut self, length: usize) -> GcList<T> {
    debug_assert!(!self.is_collecting);

    let pointer: Gc<usize> = self
      .allocate_bytes(mem::size_of::<usize>() + length * mem::size_of::<T>())
      .cast();
    self[pointer] = length;

    GcList::from(pointer)
  }

  /// Allocates a list of type T to the heap,
  /// and returns a virtual pointer to the header of the list.
  ///
  /// # Panics
  /// The function panics if the allocation request is larger than
  /// the largest allocation size ([`MAX_REAM_SIZE`]).
  pub fn allocate_list<T: Sized>(
    &mut self,
    iterator: impl Iterator<Item = T>,
    length: usize,
  ) -> GcList<T> {
    debug_assert!(!self.is_collecting);

    let list: GcList<T> = self.allocate_list_header(length);
    let buffer = self.get_list_buffer_mut(list);

    for (i, item) in iterator.enumerate() {
      debug_assert!(i < length);
      buffer[i] = item;
    }

    list
  }

  /// Get the pointer to a buffer of a list allocated with [`Heap::allocate_list`].
  #[must_use]
  pub fn get_list_buffer_ptr<T>(&self, pointer: GcList<T>) -> *const T {
    unsafe {
      pointer
        .get_pointer(self.raw.base)
        .add(mem::size_of::<usize>())
        .cast()
    }
  }

  /// Get the buffer of a list allocated with [`Heap::allocate_list`].
  #[must_use]
  pub fn get_list_buffer<T>(&self, pointer: GcList<T>) -> &[T] {
    let length = self[*pointer];
    let buffer_ptr = self.get_list_buffer_ptr(pointer);

    unsafe { slice::from_raw_parts(buffer_ptr, length) }
  }

  /// Get the buffer of a list allocated with [`Heap::allocate_list`].
  #[must_use]
  pub fn get_list_buffer_mut<T>(&mut self, pointer: GcList<T>) -> &mut [T] {
    let length = self[*pointer];
    let buffer_ptr = self.get_list_buffer_ptr(pointer).cast_mut();

    unsafe { slice::from_raw_parts_mut(buffer_ptr, length) }
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

    panic!("Out of Memory: could not allocate {bytes} bytes");
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
      self.full_list_len += 1;
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

  fn get_page(&self, index: usize) -> PageDescriptorRef {
    debug_assert!(index < self.raw.page_count);

    PageDescriptorRef::from_index(index, self.raw.base)
  }

  /// Begins a garbage collection, clearing the `gc_bits` of all used pages.
  ///
  /// # Safety
  ///
  /// This function leaves the heap in a broken state, and [`Heap::mark()`] and
  ///  [`Heap::finish_gc()`] need to be used to return it to a usable state. Using
  /// alloc before garbage collection is finished is forbidden.
  pub fn start_gc(&mut self) {
    self.is_collecting = true;

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
  pub fn mark<T>(&self, ptr: Gc<T>) {
    debug_assert!(self.is_collecting);

    if ptr.is_null() {
      return;
    }

    self.get_page(ptr.page_index()).mark(ptr.block_index());
  }

  /// Marks a pointer as freed, to free memory in the heap.
  ///
  /// If it is in a full page, the page will not be returned to the free list (until a
  /// full garbage collection is performed). But it can free space in small allocation pages.
  pub fn mark_free<T>(&self, ptr: Gc<T>) {
    if ptr.is_null() {
      return;
    }

    self.get_page(ptr.page_index()).mark_free(ptr.block_index());
  }

  /// Finishes a garbage collection
  ///
  /// Returns all unused pages to the appropriate free lists.
  pub fn finish_gc(&mut self) {
    debug_assert!(self.is_collecting);

    for page in self.full_list.iter() {
      if page.is_empty() {
        if page.is_ream() {
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

    self.full_list_len = self.full_list.iter().count();
    self.is_collecting = false;
  }

  /// Returns the number of full pages on the heap.
  ///
  /// Is incorrect during garbage collection
  #[must_use]
  pub fn full_page_count(&self) -> usize {
    self.full_list_len
  }
}
impl<T> ops::Index<Gc<T>> for Heap {
  type Output = T;

  fn index(&self, index: Gc<T>) -> &Self::Output {
    debug_assert!(!index.is_null());

    let page = self.get_page(index.page_index());
    let is_block_allocated = page.is_block_allocated(index.block_index());
    debug_assert!(self.is_collecting || is_block_allocated, "use after free");

    unsafe { &*index.get_pointer(self.raw.base).cast() }
  }
}
impl<T> ops::IndexMut<Gc<T>> for Heap {
  fn index_mut(&mut self, index: Gc<T>) -> &mut Self::Output {
    debug_assert!(!index.is_null());

    let page = self.get_page(index.page_index());
    let is_block_allocated = page.is_block_allocated(index.block_index());
    debug_assert!(self.is_collecting || is_block_allocated, "use after free");

    unsafe { &mut *index.get_pointer(self.raw.base).cast() }
  }
}

#[derive(Debug)]
struct RawMemory {
  /// The base address of the heap
  base: *mut u8,

  heap_size: usize,
  used_pages: usize,
  page_count: usize,
}
impl RawMemory {
  #[expect(clippy::unnecessary_wraps, reason = "may be fallible in the future")]
  fn new(heap_size: usize) -> Option<Self> {
    let page_count = heap_size / (PAGE_SIZE + PageDescriptor::SIZE);

    Some(Self {
      base: unsafe {
        let layout = alloc::Layout::from_size_align(heap_size, 8).unwrap();
        alloc::alloc(layout)
      },

      heap_size,
      used_pages: 0,
      page_count,
    })
  }

  /// Creates a new maximum-size ream by creating pages for it.
  fn materialize_new_ream(&mut self) -> PageDescriptorRef {
    assert!(
      self.used_pages < self.page_count,
      "Out of Memory: Heap is full"
    );

    let pages = self.used_pages;
    self.used_pages += u16::MAX as usize;

    let mut ream = PageDescriptorRef::from_index(pages, self.base);
    ream.set_ream(u16::MAX, true);
    ream
  }
}
impl Drop for RawMemory {
  fn drop(&mut self) {
    unsafe {
      let layout = alloc::Layout::from_size_align(self.heap_size, 8).unwrap();
      alloc::dealloc(self.base, layout);
    }
  }
}

/// A virtual pointer to a memory allocation on the garbage collected heap.
#[must_use]
pub struct Gc<T> {
  value: NonZero<u32>,
  _type: PhantomData<T>,
}
impl Gc<u8> {
  /// A null pointer, to pointing to no value.
  ///
  /// Is ignored by the Garbage Collector
  pub const NULL: Gc<u8> = Self {
    value: NonZero::new(u32::MAX - 7).unwrap(),
    _type: PhantomData,
  };
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
  /// The address of the pointer
  #[must_use]
  pub const fn addr_const(self) -> usize {
    self.value.get() as usize
  }

  /// Casts the pointer to a different type.
  pub fn cast<U>(self) -> Gc<U> {
    Gc {
      value: self.value,
      _type: PhantomData,
    }
  }

  /// Does the pointer point to [`Self::NULL`]?
  #[must_use]
  pub fn is_null(&self) -> bool {
    self == &Gc::NULL
  }

  fn get_pointer(self, heap_base: *mut u8) -> *mut u8 {
    unsafe { heap_base.add(self.addr()).cast() }
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
impl<T> Clone for Gc<T> {
  fn clone(&self) -> Self {
    *self
  }
}
impl<T> Copy for Gc<T> {}
impl<T, U> PartialEq<Gc<U>> for Gc<T> {
  fn eq(&self, other: &Gc<U>) -> bool {
    self.value == other.value
  }
}
impl<T> Eq for Gc<T> {}
impl<T> fmt::Debug for Gc<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.is_null() {
      write!(f, "Gc(NULL)")
    } else {
      write!(f, "Gc({:x?})", self.value.get())
    }
  }
}

/// A virtual pointer to a list on the heap
///
/// It consists of a header, which stores the size of the list, followed directly
/// by a series of values all of the same type.
#[derive(PartialEq, Eq)]
#[must_use]
#[repr(transparent)]
pub struct GcList<T: Sized> {
  header: Gc<usize>,
  _inner: PhantomData<T>,
}
impl<T> GcList<T> {
  /// Gets the number of elements which can fit in this allocation for the list
  #[must_use]
  pub fn capacity(&self, heap: &Heap) -> usize {
    let page = heap.get_page(self.page_index());
    let block_size = page.class().block_size();

    (block_size - mem::size_of::<usize>()) / core::mem::size_of::<T>()
  }
}
impl<T> Clone for GcList<T> {
  fn clone(&self) -> Self {
    *self
  }
}
impl<T> Copy for GcList<T> {}
impl<T> fmt::Debug for GcList<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "GcList({:x?})", self.value.get())
  }
}
impl<T> ops::Deref for GcList<T> {
  type Target = Gc<usize>;
  fn deref(&self) -> &Self::Target {
    &self.header
  }
}
impl<T> From<Gc<usize>> for GcList<T> {
  fn from(value: Gc<usize>) -> Self {
    Self {
      header: value,
      _inner: PhantomData,
    }
  }
}

#[cfg(test)]
mod test;
