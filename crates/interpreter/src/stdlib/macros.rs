macro_rules! module {
  ($module_name:ident, $module_items_name:ident, $module_docs_name:ident, {
    $(
      const $(#[doc = $constant_doc_comment:literal])*
      $constant_name:ident : $constant_type:ident = $constant:expr;
    )*
    $(
      $(#[doc = $doc_comment:literal])*
      fn $function_name:ident() = $function:expr;
    )*
  }) => {
    #[allow(unused_variables)]
    #[doc = concat!("The `", stringify!($module_name), "` module of Bang's Standard Library")]
    pub fn $module_name(vm: &mut VM, item: &str) -> super::ImportResult {
      use crate::object::{NativeFunction, NATIVE_FUNCTION_TYPE_ID};

      match item {
        $(stringify!($constant_name) => module!(const $constant_type, vm, $constant),)*
        $(
          stringify!($function_name) => {
            let native_function = NativeFunction::new(stringify!($function_name), $function);
            let allocated_function = vm.heap.allocate(native_function);
            let value = Value::from_object(allocated_function, NATIVE_FUNCTION_TYPE_ID);
            super::ImportResult::Value(value)
          },
        )*
        _ => super::ImportResult::ItemNotFound,
      }
    }

    #[doc = concat!("All the items in the `", stringify!($module_name), "` module")]
    pub const $module_items_name: [&str; module!(count $($constant_name)*) + module!(count $($function_name)*)] = [
      $(stringify!($constant_name),)*
      $(stringify!($function_name),)*
    ];

    #[doc = concat!("Get doc comments for the items in the `", stringify!($module_name), "` module")]
    #[must_use] pub fn $module_docs_name(item: &str) -> Option<&'static str> {
      match item {
        $(
          stringify!($constant_name) => Some(concat!($($constant_doc_comment, "\n"),*)),
        )*
        $(
          stringify!($function_name) => Some(concat!($($doc_comment, "\n"),*)),
        )*
        _ => None,
      }
    }
  };

  // Wrap Constants
  (const String, $vm:expr, $value:expr) => {
    $vm.allocate_string($value).into()
  };
  (const $_type:ident, $vm:expr, $value:expr) => {
    $value.into()
  };

  // Count
  // From The Little Book of Rust Macros
  // https://veykril.github.io/tlborm/decl-macros/building-blocks/counting.html
  (count ) => { 0 };
  (count $odd:tt $($a:tt $b:tt)*) => { (module!(count $($a)*) << 1) | 1 };
  (count $($a:tt $even:tt)*) => { module!(count $($a)*) << 1 };
}
pub(crate) use module;
