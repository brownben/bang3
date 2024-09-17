use super::{Heap, PAGE_SIZE};

#[test]
fn allocate_small_size() {
  let mut allocator = Heap::new().unwrap();
  let a = allocator.alloc(10);
  let b = allocator.alloc(10);
  let c = allocator.alloc(10);
  let d = allocator.alloc(75);
  let e = allocator.alloc(257);
  let f = allocator.alloc(83);

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
  let mut allocator = Heap::new().unwrap();
  let a = allocator.alloc(5021);
  let b = allocator.alloc(10);
  let c = allocator.alloc(10);
  let d = allocator.alloc(5021);

  assert_eq!(a.addr() + PAGE_SIZE * 2, b.addr());
  assert_eq!(b.addr() + 64, c.addr());
  assert_eq!(b.addr() + PAGE_SIZE, d.addr());
}

#[test]
fn allocate_sweep_allocate_uses_same_address() {
  let mut allocator = Heap::new().unwrap();
  let a = allocator.alloc(60);
  let b = allocator.alloc(60);
  let c = allocator.alloc(60);

  allocator.start_gc();
  allocator.mark(b);
  allocator.mark(c);
  allocator.finish_gc();

  let d = allocator.alloc(60);
  assert_eq!(a.addr(), d.addr());
}

#[test]
fn allocate_then_free_everything() {
  let mut allocator = Heap::new().unwrap();
  let _ = allocator.alloc(10);
  let _ = allocator.alloc(10);
  let _ = allocator.alloc(10);
  let _ = allocator.alloc(75);
  let _ = allocator.alloc(257);
  let _ = allocator.alloc(83);

  allocator.start_gc();
  allocator.finish_gc();
}

#[test]
fn allocate_then_free_nothing() {
  let mut allocator = Heap::new().unwrap();
  let a = allocator.alloc(10);
  let b = allocator.alloc(10);
  let c = allocator.alloc(10);
  let d = allocator.alloc(75);
  let e = allocator.alloc(257);
  let f = allocator.alloc(83);

  allocator.start_gc();
  allocator.mark(a);
  allocator.mark(b);
  allocator.mark(c);
  allocator.mark(d);
  allocator.mark(e);
  allocator.mark(f);
  allocator.finish_gc();
}
