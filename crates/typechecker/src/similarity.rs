#![allow(clippy::cast_precision_loss)]

/// Find if there is a similar name to the one given
pub fn similarly_named<'a>(
  name: &str,
  options: impl IntoIterator<Item = &'a str>,
) -> Option<String> {
  options
    .into_iter()
    .map(|option| (option, jaro_winkler_similarity(name, option)))
    .filter(|(_, similarity)| *similarity > 0.7)
    .max_by(|(_, a), (_, b)| a.total_cmp(b))
    .map(|(name, _)| name.to_owned())
}

fn jaro_similarity(a: &str, b: &str) -> f64 {
  if a.is_empty() && b.is_empty() {
    return 1.0;
  }
  if a.is_empty() || b.is_empty() {
    return 0.0;
  }

  let window = (usize::max(a.len(), b.len()) / 2).saturating_sub(1);

  let mut matches = 0.0;
  let mut a_matches = vec![false; a.len()];
  let mut b_matches = vec![false; b.len()];

  for (i, char_a) in a.bytes().enumerate() {
    let start = i.saturating_sub(window);

    for (j, char_b) in b.bytes().enumerate().skip(start).take(window * 2 + 1) {
      if char_a == char_b && !b_matches[j] {
        a_matches[i] = true;
        b_matches[j] = true;
        matches += 1.0;
        break;
      }
    }
  }

  if matches == 0.0 {
    return 0.0;
  }

  let mut transpositions = 0.0;
  let mut b_iter = b_matches.into_iter().zip(b.bytes());
  for (a_matches, char_a) in a_matches.into_iter().zip(a.bytes()) {
    if a_matches {
      for (b_matches, char_b) in b_iter.by_ref() {
        if !b_matches {
          continue;
        }

        if char_a != char_b {
          transpositions += 1.0;
        }
        break;
      }
    }
  }
  transpositions /= 2.0;

  (1.0 / 3.0)
    * (matches / a.len() as f64 + matches / b.len() as f64 + (matches - transpositions) / matches)
}

fn jaro_winkler_similarity(a: &str, b: &str) -> f64 {
  let similarity = jaro_similarity(a, b);
  let prefix_length = a
    .chars()
    .take(4)
    .zip(b.chars())
    .take_while(|(a, b)| a == b)
    .count();

  (0.1 * prefix_length as f64).mul_add(1.0 - similarity, similarity)
}

#[cfg(test)]
mod test {
  use super::{jaro_similarity, jaro_winkler_similarity, similarly_named};

  macro_rules! assert_similar {
    ($x:expr, $y:expr) => {
      assert!(
        ($x - $y).abs() < 0.01,
        "expected {} to be similar to {}",
        $x,
        $y,
      )
    };
  }

  #[test]
  fn empty_strings() {
    assert_similar!(jaro_winkler_similarity("", ""), 1.0);
    assert_similar!(jaro_winkler_similarity("hello", ""), 0.0);
    assert_similar!(jaro_winkler_similarity("", "hello"), 0.0);
  }

  #[test]
  fn completely_different_strings() {
    assert_similar!(jaro_winkler_similarity("abs", "poo"), 0.0);
    assert_similar!(jaro_winkler_similarity("rude", "laky"), 0.0);
    assert_similar!(jaro_winkler_similarity("a", "b"), 0.0);
    assert_similar!(jaro_winkler_similarity("b", "a"), 0.0);
  }

  #[test]
  fn identical_strings() {
    assert_similar!(jaro_winkler_similarity("Hello World", "Hello World"), 1.0);
    assert_similar!(jaro_winkler_similarity("a", "a"), 1.0);
    assert_similar!(jaro_winkler_similarity("Lazy", "Lazy"), 1.0);
  }

  #[test]
  fn wikipedia_example() {
    // https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance
    assert_similar!(jaro_similarity("FAREMVIEL", "FARMVILLE"), 0.88);
    assert_similar!(jaro_winkler_similarity("FAREMVIEL", "FARMVILLE"), 0.92);
  }

  #[test]
  fn strsim_tests() {
    // Tests from `strsim` crate: https://github.com/rapidfuzz/strsim-rs/tree/main (MIT License)

    assert_similar!(0.81, jaro_winkler_similarity("dixon", "dicksonx"));
    assert_similar!(0.81, jaro_winkler_similarity("dicksonx", "dixon"));
    assert_similar!(0.84, jaro_winkler_similarity("dwayne", "duane"));
    assert_similar!(0.96, jaro_winkler_similarity("martha", "marhta"));
    assert_similar!(0.60, jaro_winkler_similarity("a jke", "jane a k"));
    assert_similar!(0.86, jaro_winkler_similarity("Thorkel", "Thorgier"));
    assert_similar!(0.73, jaro_winkler_similarity("Dinsdale", "D"));
    assert_similar!(
      0.99,
      jaro_winkler_similarity("thequickbrownfoxjumpedoverx", "thequickbrownfoxjumpedovery")
    );
    assert_similar!(
      0.88,
      jaro_winkler_similarity("cheese burger", "cheese fries")
    );
  }

  #[test]
  fn makes_choice() {
    assert_eq!(similarly_named("bar", ["baz"]), Some("baz".to_owned()));
    assert_eq!(similarly_named("bar", ["daz"]), None);
    assert_eq!(similarly_named("bar", ["dazzle"]), None);

    assert_eq!(
      similarly_named("math", ["maths", "string"]),
      Some("maths".to_owned())
    );
    assert_eq!(
      similarly_named("stringy", ["maths", "string"]),
      Some("string".to_owned())
    );

    assert_eq!(
      similarly_named("hello", ["hell", "helly", "hallo"]),
      Some("hell".to_owned())
    );
  }
}
