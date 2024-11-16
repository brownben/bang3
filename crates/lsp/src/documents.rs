use bang_syntax::{parse, AST};
pub use lsp_types::Uri as FileIdentfier;
use std::collections::HashMap;

#[derive(Default)]
pub struct DocumentIndex {
  files: HashMap<FileIdentfier, Document>,
}
impl DocumentIndex {
  pub fn open(&mut self, id: FileIdentfier, source: String) {
    self.files.insert(id.clone(), Document::new(id, source));
  }
  pub fn get(&mut self, id: &FileIdentfier) -> &mut Document {
    self.files.get_mut(id).expect("file to be open")
  }
  pub fn close(&mut self, id: &FileIdentfier) {
    self.files.remove(id);
  }
}

pub struct Document {
  pub id: FileIdentfier,
  pub ast: AST,
}
impl Document {
  fn new(id: FileIdentfier, source: String) -> Self {
    Self {
      id,
      ast: parse(source),
    }
  }

  pub fn update(&mut self, new_source: String) {
    self.ast = parse(new_source);
  }
}
