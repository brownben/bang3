use bang_syntax::{AST, parse, parse_into};
use bang_typechecker::TypeChecker;
pub use lsp_types::Uri as FileIdentfier;
use std::{cell::OnceCell, collections::HashMap};

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
  typechecker: OnceCell<TypeChecker>,
}
impl Document {
  fn new(id: FileIdentfier, source: String) -> Self {
    Self {
      id,
      ast: parse(source),
      typechecker: OnceCell::new(),
    }
  }

  pub fn update(&mut self, new_source: String) {
    parse_into(new_source, &mut self.ast);
    self.typechecker = OnceCell::new();
  }

  pub fn typechecker(&self) -> &TypeChecker {
    self
      .typechecker
      .get_or_init(|| TypeChecker::check(&self.ast))
  }
}
