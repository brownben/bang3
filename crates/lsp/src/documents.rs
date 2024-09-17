use bang_parser::LineIndex;
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
  pub source: String,
  pub line_index: LineIndex,
}
impl Document {
  fn new(id: FileIdentfier, source: String) -> Self {
    let line_index = LineIndex::from_source(&source);

    Self {
      id,
      source,
      line_index,
    }
  }

  pub fn update(&mut self, new_source: String) {
    self.source = new_source;
    self.line_index = LineIndex::from_source(&self.source);
  }
}
