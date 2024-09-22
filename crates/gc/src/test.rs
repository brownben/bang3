use super::{Heap, HeapSize, PAGE_SIZE};

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
