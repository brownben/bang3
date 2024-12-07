use super::{Heap, HeapSize, PAGE_SIZE};
use crate::Gc;

#[test]
fn allocate_small_size() {
  let mut allocator = Heap::new(HeapSize::Standard).unwrap();
  let a = allocator.allocate_bytes(10);
  let b = allocator.allocate_bytes(10);
  let c = allocator.allocate_bytes(10);
  let d = allocator.allocate_bytes(75);
  let e = allocator.allocate_bytes(257);
  let f = allocator.allocate_bytes(83);

  // objects of the same size are allocated contiguously
  assert_eq!(a.addr() + 64, b.addr());
  assert_eq!(b.addr() + 64, c.addr());

  // object of a different size are allocated on a different page
  assert_eq!(a.addr() + PAGE_SIZE, d.addr());
  assert_eq!(d.addr() + 128, f.addr());
  assert_eq!(d.addr() + PAGE_SIZE, e.addr());
}

#[test]
fn allocate_reams() {
  let mut allocator = Heap::new(HeapSize::Standard).unwrap();
  let a = allocator.allocate_bytes(5021);
  let b = allocator.allocate_bytes(10);
  let c = allocator.allocate_bytes(10);
  let d = allocator.allocate_bytes(5021);

  assert_eq!(a.addr() + PAGE_SIZE * 2, b.addr());
  assert_eq!(b.addr() + 64, c.addr());
  assert_eq!(b.addr() + PAGE_SIZE, d.addr());
}

#[test]
fn allocate_sweep_allocate_uses_same_address() {
  let mut allocator = Heap::new(HeapSize::Standard).unwrap();
  let a = allocator.allocate_bytes(60);
  let b = allocator.allocate_bytes(60);
  let c = allocator.allocate_bytes(60);

  allocator.start_gc();
  allocator.mark(b);
  allocator.mark(c);
  allocator.finish_gc();

  let d = allocator.allocate_bytes(60);
  assert_eq!(a.addr(), d.addr());
}

#[test]
fn allocate_then_free_everything() {
  let mut allocator = Heap::new(HeapSize::Standard).unwrap();
  let _ = allocator.allocate_bytes(10);
  let _ = allocator.allocate_bytes(10);
  let _ = allocator.allocate_bytes(10);
  let _ = allocator.allocate_bytes(75);
  let _ = allocator.allocate_bytes(257);
  let _ = allocator.allocate_bytes(83);

  allocator.start_gc();
  allocator.finish_gc();
}

#[test]
fn allocate_then_free_nothing() {
  let mut allocator = Heap::new(HeapSize::Standard).unwrap();
  let a = allocator.allocate_bytes(10);
  let b = allocator.allocate_bytes(10);
  let c = allocator.allocate_bytes(10);
  let d = allocator.allocate_bytes(75);
  let e = allocator.allocate_bytes(257);
  let f = allocator.allocate_bytes(83);

  allocator.start_gc();
  allocator.mark(a);
  allocator.mark(b);
  allocator.mark(c);
  allocator.mark(d);
  allocator.mark(e);
  allocator.mark(f);
  allocator.finish_gc();
}

#[test]
fn small_heap() {
  let mut allocator = Heap::new(HeapSize::Small).unwrap();
  let _ = allocator.allocate_bytes(10);
  let b = allocator.allocate_bytes(10);
  let c = allocator.allocate_bytes(10);
  let _ = allocator.allocate_bytes(75);
  let e = allocator.allocate_bytes(257);
  let _ = allocator.allocate_bytes(83);

  allocator.start_gc();
  allocator.mark(b);
  allocator.mark(c);
  allocator.mark(e);
  allocator.finish_gc();
}

#[test]
fn fill_pages_with_small_objects_then_gc_all() {
  let mut allocator = Heap::new(HeapSize::Small).unwrap();
  for _ in 0..64 {
    let _ = allocator.allocate_bytes(60);
  }

  allocator.start_gc();
  allocator.finish_gc();
}

#[test]
fn fill_pages_with_small_objects() {
  let mut allocator = Heap::new(HeapSize::Small).unwrap();
  for _ in 0..28 {
    let _ = allocator.allocate_bytes(60);
  }
  let a = allocator.allocate_bytes(60);
  let b = allocator.allocate_bytes(60);
  for _ in 0..6 {
    let _ = allocator.allocate_bytes(60);
  }
  let c = allocator.allocate_bytes(60);
  for _ in 0..3 {
    let _ = allocator.allocate_bytes(60);
  }
  let d = allocator.allocate_bytes(60);
  for _ in 0..45 {
    let _ = allocator.allocate_bytes(60);
  }

  assert_eq!(allocator.size_class_lists[0].iter().count(), 1);
  assert_eq!(allocator.full_list.iter().count(), 1);

  allocator.start_gc();
  allocator.mark(a);
  allocator.mark(b);
  allocator.mark(c);
  allocator.mark(d);
  allocator.finish_gc();

  assert_eq!(allocator.size_class_lists[0].iter().count(), 1);
  assert_eq!(allocator.full_list.iter().count(), 0);

  for _ in 0..6 {
    let _ = allocator.allocate_bytes(60);
  }
  allocator.start_gc();
  allocator.mark(a);
  allocator.mark(b);
  allocator.mark(c);
  allocator.mark(d);
  allocator.finish_gc();

  allocator.start_gc();
  allocator.finish_gc();
}

#[test]
fn fill_page_then_reuse_diff_size() {
  let mut allocator = Heap::new(HeapSize::Small).unwrap();
  for _ in 0..70 {
    let _ = allocator.allocate_bytes(60);
  }
  assert_eq!(allocator.size_class_lists[0].iter().count(), 1);
  assert_eq!(allocator.full_list.iter().count(), 1);

  allocator.start_gc();
  allocator.finish_gc();
  assert_eq!(allocator.full_list.iter().count(), 0);

  for _ in 0..32 {
    let _ = allocator.allocate_bytes(128);
  }
  assert_eq!(allocator.full_list.iter().count(), 1);

  allocator.start_gc();
  allocator.finish_gc();
  assert_eq!(allocator.full_list.iter().count(), 0);
}

#[test]
fn allocate_ream() {
  let mut allocator = Heap::new(HeapSize::Small).unwrap();
  let ream = allocator.allocate_bytes(PAGE_SIZE + 1);

  allocator.start_gc();
  allocator.mark(ream);
  allocator.finish_gc();

  allocator.start_gc();
  allocator.finish_gc();
}

#[test]
fn allocate_ream_then_reuse_split() {
  let mut allocator = Heap::new(HeapSize::Small).unwrap();
  let _ream = allocator.allocate_bytes(PAGE_SIZE + PAGE_SIZE + 1);

  allocator.start_gc();
  allocator.finish_gc();

  let _ream = allocator.allocate_bytes(PAGE_SIZE + 1);

  allocator.start_gc();
  allocator.finish_gc();
}

#[test]
fn allocate_ream_then_reuse() {
  let mut allocator = Heap::new(HeapSize::Small).unwrap();
  let _ream = allocator.allocate_bytes(PAGE_SIZE + 1);

  assert_eq!(allocator.full_list.iter().count(), 1);
  assert_eq!(allocator.free_ream_list.iter().count(), 1);
  allocator.start_gc();
  allocator.finish_gc();
  assert_eq!(allocator.full_list.iter().count(), 0);
  assert_eq!(allocator.free_ream_list.iter().count(), 2);

  let _ream = allocator.allocate_bytes(PAGE_SIZE + 1);

  assert_eq!(allocator.full_list.iter().count(), 1);
  assert_eq!(allocator.free_ream_list.iter().count(), 1);
  allocator.start_gc();
  allocator.finish_gc();
}

#[test]
fn allocate_ream_then_reuse_too_small() {
  let mut allocator = Heap::new(HeapSize::Small).unwrap();
  let _ream = allocator.allocate_bytes(PAGE_SIZE + 1);

  allocator.start_gc();
  allocator.finish_gc();
  assert_eq!(allocator.free_ream_list.iter().count(), 2);

  let _ream = allocator.allocate_bytes(PAGE_SIZE * 3);

  assert_eq!(allocator.full_list.iter().count(), 1);
  assert_eq!(allocator.free_ream_list.iter().count(), 2);
}

#[test]
fn mark_null() {
  let mut allocator = Heap::new(HeapSize::Small).unwrap();

  allocator.start_gc();
  allocator.mark(Gc::NULL);
  allocator.finish_gc();
}

#[test]
fn mark_as_free() {
  let mut allocator = Heap::new(HeapSize::Standard).unwrap();
  let a = allocator.allocate_bytes(10);
  let b = allocator.allocate_bytes(10);
  let c = allocator.allocate_bytes(10);

  let small_object_page_list = &mut allocator.size_class_lists[0];
  let first_page = small_object_page_list.first().unwrap();

  assert_eq!(first_page.num_allocated_blocks(), 3);

  allocator.mark_free(b);
  assert_eq!(first_page.num_allocated_blocks(), 2);

  allocator.mark_free(a);
  assert_eq!(first_page.num_allocated_blocks(), 1);

  allocator.mark_free(c);
  assert_eq!(first_page.num_allocated_blocks(), 0);
}
