#![allow(unused)]

use std::alloc;
use std::mem;

#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BlockMeta {
  Free,
  Allocated,
}

#[derive(Clone, Copy, Debug)]
struct Block(*mut u32);
impl Block {
  const HEADER_SIZE: u32 = 2;

  const OVERHEAD: u32 = Self::HEADER_SIZE * 4;

  fn size_ptr(&self) -> *mut u32 {
    self.0
  }
  fn meta_ptr(&self) -> *mut BlockMeta {
    unsafe { self.0.add(1).cast() }
  }
  fn data_ptr(&self) -> *mut u8 {
    unsafe { self.0.add(2).cast() }
  }

  /// The size of the data portion of the block
  fn size(&self) -> u32 {
    unsafe { *self.size_ptr() }
  }
  /// Is the block free?
  fn meta(&self) -> BlockMeta {
    unsafe { *self.meta_ptr() }
  }
  /// The next block in memory
  unsafe fn next_block(&self) -> Block {
    let size = usize::try_from(self.size()).unwrap();
    Block(unsafe { self.0.add(2).add(size / mem::size_of::<u32>()).cast() })
  }

  fn split_block(&mut self, size: u32) -> Block {
    let existing_size = self.size();

    unsafe {
      *self.size_ptr() = size;
      *self.meta_ptr() = BlockMeta::Allocated;
    }

    let new_block = unsafe { self.next_block() };
    unsafe {
      *new_block.size_ptr() = existing_size - size - Block::OVERHEAD;
      *new_block.meta_ptr() = BlockMeta::Free;
    }

    new_block
  }
}

#[derive(Debug)]
pub struct Heap {
  memory: *mut u8,
  size: u32,
  free: Option<Block>,
}
impl Heap {
  pub fn new(size: u32) -> Self {
    // heap size is a sensible size
    debug_assert!(size > 0);
    debug_assert!(size < u32::MAX - Block::OVERHEAD);

    // align to 4 bytes for header and footers
    let size = size + size % 8u32;

    let layout = alloc::Layout::array::<u64>(usize::try_from(size).unwrap() / 8).unwrap();
    let memory = unsafe { alloc::alloc(layout) };

    let initial_block = Block(memory.cast());
    unsafe {
      *initial_block.size_ptr() = size - Block::OVERHEAD;
      *initial_block.meta_ptr() = BlockMeta::Free;
    }

    Self {
      memory,
      size,
      free: Some(initial_block),
    }
  }

  pub fn allocate_raw(&mut self, size: u32) -> *mut u8 {
    debug_assert!(self.free.is_some());
    debug_assert!(self.free.unwrap().meta() == BlockMeta::Free);

    // align to 4 bytes for header and footers
    let size = size + size % 8u32;

    let mut existing_block = self.free.unwrap();

    if existing_block.size() < size {
      panic!("Out of memory")
    } else if existing_block.size() < (size + Block::OVERHEAD * 2) {
      unsafe { *existing_block.meta_ptr() = BlockMeta::Allocated };
      self.free = None;
      existing_block.data_ptr()
    } else {
      let new_block = existing_block.split_block(size);
      self.free = Some(new_block);
      existing_block.data_ptr()
    }
  }

  pub fn allocate<T>(&mut self, value: T) -> *mut T {
    debug_assert!(mem::size_of::<T>() <= u32::MAX as usize);

    let ptr = self
      .allocate_raw(u32::try_from(mem::size_of::<T>()).unwrap())
      .cast::<T>();

    unsafe { ptr.write(value) };
    ptr
  }
}
impl Drop for Heap {
  fn drop(&mut self) {
    let size = usize::try_from(self.size).unwrap();
    let layout = alloc::Layout::array::<u64>(size / 8).unwrap();
    unsafe { alloc::dealloc(self.memory, layout) }
  }
}
impl Default for Heap {
  fn default() -> Self {
    Self::new(4096)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn new_heap() {
    let heap = Heap::new(1024);
    assert_eq!(heap.free.unwrap().size(), 1024 - Block::OVERHEAD);
    assert_eq!(heap.free.unwrap().meta(), BlockMeta::Free);

    let heap = Heap::new(64);
    assert_eq!(heap.free.unwrap().size(), 64 - Block::OVERHEAD);
    assert_eq!(heap.free.unwrap().meta(), BlockMeta::Free);
  }

  #[test]
  fn allocate_blocks() {
    let mut heap = Heap::new(64);

    // Initial heap has a singular free block
    let view = unsafe { &*heap.memory.cast::<[u32; 16]>() };
    assert_eq!(view[0], 64 - Block::OVERHEAD);
    assert_eq!(view[1], BlockMeta::Free as u32);

    // A new block is allocated, by splitting the free block
    heap.allocate_raw(8);
    let view = unsafe { &*heap.memory.cast::<[u32; 16]>() };
    // 1st allocation
    assert_eq!(view[0], 8);
    assert_eq!(view[1], BlockMeta::Allocated as u32);
    // Remaining free block
    assert_eq!(view[4], 40);
    assert_eq!(view[5], BlockMeta::Free as u32);

    // Another block is allocated, by splitting the free block
    heap.allocate_raw(8);
    let view = unsafe { &*heap.memory.cast::<[u32; 16]>() };
    // 1st allocation
    assert_eq!(view[0], 8);
    assert_eq!(view[1], BlockMeta::Allocated as u32);
    // 2nd allocation
    assert_eq!(view[4], 8);
    assert_eq!(view[5], BlockMeta::Allocated as u32);
    // Remaining free block
    assert_eq!(view[8], 24);
    assert_eq!(view[9], BlockMeta::Free as u32);

    // Another block is allocated, the free block is used
    heap.allocate_raw(16);
    let view = unsafe { &*heap.memory.cast::<[u32; 16]>() };
    // 1st allocation
    assert_eq!(view[0], 8);
    assert_eq!(view[1], BlockMeta::Allocated as u32);
    // 2nd allocation
    assert_eq!(view[4], 8);
    assert_eq!(view[5], BlockMeta::Allocated as u32);
    // 3rd allocation
    assert_eq!(view[8], 24);
    assert_eq!(view[9], BlockMeta::Allocated as u32);
  }
}
