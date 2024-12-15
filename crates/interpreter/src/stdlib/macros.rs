macro_rules! module {
  (
    $module_name:ident,
    $module_struct_name:ident,
  {
    $(
      const $(#[doc = $constant_doc_comment:literal])*
      $constant_name:ident : $constant_type:ident = $constant:expr;
    )*
    $(
      $(#[doc = $doc_comment:literal])*
      #[type$function_type:tt]
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
            let full_function_name = concat!(
              stringify!($module_name), "::", stringify!($function_name)
            );
            let native_function = NativeFunction::new(full_function_name, $function);
            let allocated_function = vm.heap.allocate(native_function);
            let value = Value::from_object(allocated_function, NATIVE_FUNCTION_TYPE_ID);
            super::ImportResult::Value(value)
          },
        )*
        _ => super::ImportResult::ItemNotFound,
      }
    }

    #[doc = concat!("The `", stringify!($module_name), "` module of Bang's Standard Library")]
    pub struct $module_struct_name;
    impl StdlibModule for $module_struct_name {
      fn name(&self) -> &'static str {
        stringify!($module_name)
      }

      fn items(&self) -> &'static [&'static str] {
        &[
          $(stringify!($constant_name),)*
          $(stringify!($function_name),)*
        ]
      }
      fn docs(&self, item: &str) -> Option<&'static str> {
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

      fn type_of(&self, item: &str) -> Option<&'static str> {
        match item {
          $(
            stringify!($constant_name) => Some(module!(const type $constant_type)),
          )*
          $(
            stringify!($function_name) => Some(stringify!($function_type)),
          )*
          _ => None,
        }
      }
    }
  };

  // Wrap Constants
  (const type String) => {
    "string"
  };
  (const type $type:ident) => {
    stringify!($type)
  };
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
