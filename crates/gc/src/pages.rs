use crate::{Gc, PAGE_COUNT, PAGE_SIZE, PD_SECTION_SIZE};
use core::{cmp::Ordering, fmt, iter, mem, num::NonZero};

#[derive(Debug)]
pub struct PageDescriptor {
  // free list info
  prev: Option<PdPointer>,
  next: Option<PdPointer>,

  // page type
  class: BlockClass,
  /// The number of pages after this one in the current ream
  len: u16,

  // gc info
  gc_bits: u64,

  // currently unused
  _padding: [u8; 5],
}
impl PageDescriptor {
  pub const SIZE: usize = mem::size_of::<Self>();
  const _ASSERT_SIZE: () = assert!(Self::SIZE == 24);
}

/// The size of the blocks for a given page
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BlockClass {
  Ream = 0,
  B64 = 6,
  B128 = 7,
  B256 = 8,
  B512 = 9,
  B1024 = 10,
  B2048 = 11,
  /// A full page
  B4096 = 12,
}
impl BlockClass {
  pub const SMALL_CLASS_COUNT: usize = 7;
  pub const SMALL_CLASSES: [Self; Self::SMALL_CLASS_COUNT] = [
    Self::B64,
    Self::B128,
    Self::B256,
    Self::B512,
    Self::B1024,
    Self::B2048,
    Self::B4096,
  ];

  /// Get the block size which can contain at least `bytes` bytes.
  ///
  /// Assumes that `bytes` is smaller than a full page.
  pub fn get(bytes: usize) -> Self {
    let next_class = bytes.max(64).next_power_of_two();
    let class_index = next_class.trailing_zeros().saturating_sub(6);
    Self::SMALL_CLASSES[class_index as usize]
  }

  /// The number of bytes in a block of this class.
  pub fn block_size(self) -> usize {
    1 << (self as usize)
  }

  /// The number of blocks in a page of this class.
  pub fn block_count(self) -> usize {
    if self == Self::Ream {
      return 1;
    }

    4096 / self.block_size()
  }
}

/// A reference to a page descriptor
///
/// With the allocation base to allow actual data to be accessed.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PageDescriptorRef {
  pd: *mut PageDescriptor,
  alloc_base: *mut u8,
}
impl PageDescriptorRef {
  fn new(pd: PdPointer, alloc_base: *mut u8) -> Self {
    Self {
      pd: unsafe { alloc_base.add(pd.addr()).cast() },
      alloc_base,
    }
  }
  pub fn from_index(index: usize, alloc_base: *mut u8) -> Self {
    debug_assert!(index < PAGE_COUNT);

    #[allow(clippy::cast_ptr_alignment)]
    Self {
      pd: unsafe { alloc_base.cast::<PageDescriptor>().add(index) },
      alloc_base,
    }
  }

  /// What is the index of this page?
  pub fn index(&self) -> usize {
    (self.pd.addr() - self.alloc_base.addr()) / PageDescriptor::SIZE
  }
  /// Gets a virtual pointer to this page descriptor.
  pub fn as_ptr(self) -> PdPointer {
    let ptr = self.index() * PageDescriptor::SIZE;
    PdPointer::new(unsafe { NonZero::new_unchecked(ptr.try_into().unwrap_unchecked()) })
  }
  /// Get the page descriptor at index offset from this page.
  pub fn offset(self, offset: isize) -> Self {
    Self {
      pd: unsafe { self.pd.offset(offset) },
      alloc_base: self.alloc_base,
    }
  }

  /// Gets a virtual pointer to the data of this page.
  pub fn data_pointer(&self) -> Gc<u8> {
    let address = PD_SECTION_SIZE + self.index() * PAGE_SIZE;
    Gc::new(unsafe { NonZero::new_unchecked(address.try_into().unwrap_unchecked()) })
  }

  /// Gets the page descriptor after this one in a list
  pub fn next(&self) -> Option<Self> {
    let next = self.as_ref().next?;
    Some(Self::new(next, self.alloc_base))
  }
  /// Gets the page descriptor before this one in a list
  pub fn prev(&self) -> Option<Self> {
    let prev = self.as_ref().prev?;
    Some(Self::new(prev, self.alloc_base))
  }
  /// Remove this page descriptor from the list it is in
  pub fn unlink(mut self) -> Self {
    let mut prev = self.prev().expect("page to be linked");

    if let Some(mut next) = self.next() {
      next.as_mut().prev = self.prev().map(Self::as_ptr);
    }
    prev.as_mut().next = self.next().map(Self::as_ptr);

    let page = self.as_mut();
    page.prev = None;
    page.next = None;

    self
  }

  /// Is this page descriptor a ream?
  pub fn is_ream(&self) -> bool {
    self.as_ref().class == BlockClass::Ream
  }
  /// Get the number of pages in this ream
  pub fn pages(&self) -> u16 {
    debug_assert!(self.is_ream());

    self.as_ref().len + 1
  }
  /// Set the page to be a ream of size `size` pages
  pub fn set_ream(&mut self, size: u16, unlink: bool) {
    let ream = self.as_mut();
    ream.class = BlockClass::Ream;
    ream.len = size - 1;
    if unlink {
      ream.prev = None;
      ream.next = None;
      ream.gc_bits = 1;
    }
  }
  /// Split a ream into two, with the first one of size `prefix_size` pages
  pub fn split(mut self, prefix_size: u16) -> Option<(Self, Option<Self>)> {
    debug_assert!(self.is_ream());

    match self.pages().cmp(&prefix_size) {
      Ordering::Less => None,
      Ordering::Equal => Some((self, None)),
      Ordering::Greater => {
        let mut suffix = self.offset(isize::try_from(prefix_size).unwrap());
        suffix.set_ream(self.pages() - prefix_size, true);

        self.set_ream(prefix_size, false);
        Some((self, Some(suffix)))
      }
    }
  }

  /// Get the type of page this is
  pub fn class(&self) -> BlockClass {
    self.as_ref().class
  }
  /// Set the block size for this page
  pub fn set_class(&mut self, class: BlockClass) {
    self.as_mut().class = class;
    self.as_mut().len = 0;
  }

  /// Is the page empty?
  pub fn is_empty(&self) -> bool {
    self.as_ref().gc_bits == 0
  }
  /// Is the page full?
  pub fn is_full(&self) -> bool {
    let blocks = self.as_ref().class.block_count();
    let mask = if blocks == 64 {
      u64::MAX
    } else {
      (1u64 << blocks) - 1
    };

    self.as_ref().gc_bits == mask
  }
  /// Set the page to be empty, ready for marking
  pub fn set_empty(&mut self) {
    self.as_mut().gc_bits = 0;
  }
  /// Mark a block in the page as used
  pub fn mark(&mut self, block_index: usize) {
    let block_index = block_index / self.as_ref().class.block_size();
    let gc_bits = &mut self.as_mut().gc_bits;
    *gc_bits |= 1u64 << block_index;
  }
  /// Mark a block in the page as used
  pub fn mark_free(&mut self, block_index: usize) {
    let block_index = block_index / self.as_ref().class.block_size();
    let gc_bits = &mut self.as_mut().gc_bits;
    *gc_bits &= !(1u64 << block_index);
  }
  /// Get the next free block in the page
  pub fn take_next_block(&mut self) -> usize {
    let gc_bits = &mut self.as_mut().gc_bits;
    let idx = gc_bits.trailing_ones().try_into().unwrap();
    *gc_bits |= 1u64 << idx;
    idx
  }
  /// Is the given block allocated?
  pub fn is_block_allocated(&self, block_index: usize) -> bool {
    if self.as_ref().class == BlockClass::Ream {
      return self.as_ref().gc_bits > 0;
    }

    let block_index = block_index / self.as_ref().class.block_size();
    let gc_bits = self.as_ref().gc_bits;
    let block_mask = 1u64 << block_index;

    (gc_bits & block_mask) == block_mask
  }
  /// Count the number of allocated blocks in the page
  pub fn num_allocated_blocks(&self) -> u32 {
    self.as_ref().gc_bits.count_ones()
  }
}
impl AsRef<PageDescriptor> for PageDescriptorRef {
  fn as_ref(&self) -> &PageDescriptor {
    unsafe { &*self.pd }
  }
}
impl AsMut<PageDescriptor> for PageDescriptorRef {
  fn as_mut(&mut self) -> &mut PageDescriptor {
    unsafe { &mut *self.pd }
  }
}
impl fmt::Debug for PageDescriptorRef {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.as_ref().fmt(f)
  }
}

/// An intrusively linked list of page descriptors, for use in free lists
#[derive(Debug)]
pub struct PageList {
  /// The root of the list, a page which is only used to be the root of the list
  root: PageDescriptorRef,
}
impl PageList {
  pub fn new(root_index: u16, allocator_base: *mut u8) -> Self {
    let mut root = PageDescriptorRef::new(PdPointer::from_index(root_index.into()), allocator_base);

    root.as_mut().next = None;
    root.as_mut().prev = None;

    Self { root }
  }

  /// Add a page to the list
  pub fn push(&mut self, mut pd: PageDescriptorRef) {
    debug_assert!(self.root != pd, "can't link root to it's self");

    if pd.prev().is_some() {
      pd.unlink();
    }

    if let Some(mut first) = self.root.next() {
      pd.as_mut().next = Some(first.as_ptr());
      first.as_mut().prev = Some(pd.as_ptr());
    }
    self.root.as_mut().next = Some(pd.as_ptr());
    pd.as_mut().prev = Some(self.root.as_ptr());
  }

  /// Get the first page in the list
  pub fn first(&self) -> Option<PageDescriptorRef> {
    self.root.next()
  }

  /// Is the list empty?
  pub fn is_empty(&self) -> bool {
    self.root.next().is_none()
  }

  /// Iterate over the pages in the list
  pub fn iter(&self) -> impl Iterator<Item = PageDescriptorRef> + use<> {
    // we get the next page before returning the current page,
    // so if the current page is added to a different list,
    // the iteration can continue through the correct list

    let mut next = self.root.next();
    iter::from_fn(move || {
      let current = next;
      next = current.and_then(|page| page.next());
      current
    })
  }
}

/// A virtual pointer to a page descriptor.
///
/// Used in free lists to minimise the space needed to for the free lists.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PdPointer {
  value: NonZero<u32>,
}
impl PdPointer {
  fn new(value: NonZero<u32>) -> Self {
    // check that the pointer is to a page descriptor and is aligned
    debug_assert!(value.get() < PD_SECTION_SIZE.try_into().unwrap());
    debug_assert!((value.get()).is_multiple_of(u32::try_from(PageDescriptor::SIZE).unwrap()));

    Self { value }
  }

  fn from_index(index: usize) -> Self {
    debug_assert!(index < PAGE_COUNT);

    let address = index * PageDescriptor::SIZE;
    Self::new(unsafe { NonZero::new_unchecked(address.try_into().unwrap_unchecked()) })
  }

  fn addr(self) -> usize {
    self.value.get().try_into().unwrap()
  }
}
impl fmt::Debug for PdPointer {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "PdPointer({:x})", self.addr())
  }
}
