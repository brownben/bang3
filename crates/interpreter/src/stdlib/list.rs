use super::macros::{module, prelude::*};

module!(list, LIST_ITEMS, list_docs, {
  /// The number of elements in a list
  ///
  /// ## Example
  /// ```bang
  /// [2, 3, 5, 7, 11] >> list::length // 5
  /// [2] >> list::length // 1
  /// ```
  fn length(List) -> usize = |list: &[_]| list.len();
  /// Is the list empty?
  ///
  /// ## Example
  /// ```bang
  /// [] >> list::isEmpty // true
  /// [3, 4] >> list::isEmpty // false
  /// ```
  fn isEmpty(List) -> bool = |list: &[_]| list.is_empty();

  /// Is the given item in the list?
  ///
  /// ## Example
  /// ```bang
  /// [1, 3, 5] >> list::contains(3) // true
  /// ['a', 'big', 'dog'] >> list::contains('hello') // false
  /// ```
  fn contains(Any, List) -> bool = |vm: &VM, search, list: &[Value]| {
    list.iter().any(|item| vm.equals(*item, search))
  };
});
