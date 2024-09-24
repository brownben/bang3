use crate::{pages::BlockClass, Heap};
use core::fmt;

impl fmt::Display for Heap {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "  ╭─[Heap]")?;
    writeln!(f, "  ├─ Base Pointer: {:x?}", self.raw.base())?;
    writeln!(f, "  ├─ Is Collecting? {}", self.is_collecting)?;
    writeln!(f, "  │ ")?;

    let free_page_list_len = self.free_page_list.iter().count();
    let free_ream_list_len = self.free_ream_list.iter().count();
    let full_list_len = self.full_list.iter().count();
    writeln!(f, "  ├─ Free Page List (length: {free_page_list_len})",)?;
    writeln!(f, "  ├─ Free Ream List (length: {free_ream_list_len})",)?;
    writeln!(f, "  ├─ Full Page List (length: {full_list_len})")?;
    writeln!(f, "  │ ")?;

    for (size_id, list) in self.size_class_lists.iter().enumerate() {
      let size = BlockClass::SMALL_CLASSES[size_id];
      let length = list.iter().count();
      let blocks: u32 = list.iter().map(|page| page.num_allocated_blocks()).sum();

      writeln!(
        f,
        "  ├─ {size:?} Page List (length: {length}, allocated blocks: {blocks})",
      )?;
    }

    writeln!(f, "──╯")
  }
}
