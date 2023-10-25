use std::fmt::Display;

/// Convert a seq iterator to a separated string with prefix and suffix and customized printer
pub fn format_seq_custom<'a, T: 'a, I: IntoIterator<Item = &'a T>, F: Fn(&T) -> String>(
    sep: &str,
    prefix: &str,
    suffix: &str,
    iter: I,
    display: F,
) -> String {
    let content: Vec<_> = iter.into_iter().map(display).collect();
    format!("{}{}{}", prefix, content.join(sep), suffix)
}

/// Convert a seq iterator to a separated string with prefix and suffix
pub fn format_seq<'a, T: Display + 'a, I: IntoIterator<Item = &'a T>>(
    sep: &str,
    prefix: &str,
    suffix: &str,
    iter: I,
) -> String {
    format_seq_custom(sep, prefix, suffix, iter, |i| i.to_string())
}

/// Convert a map iterator to a separated string with prefix and suffix
pub fn format_map_custom<
    'a,
    K: 'a,
    V: 'a,
    I: IntoIterator<Item = (&'a K, &'a V)>,
    F: Fn(&K, &V) -> String,
>(
    sep: &str,
    prefix: &str,
    suffix: &str,
    iter: I,
    display: F,
) -> String {
    let content: Vec<_> = iter.into_iter().map(|(k, v)| display(k, v)).collect();
    format!("{}{}{}", prefix, content.join(sep), suffix)
}

/// Convert a map iterator to a separated string with prefix and suffix
pub fn format_map<'a, K: 'a + Display, V: 'a + Display, I: IntoIterator<Item = (&'a K, &'a V)>>(
    sep: &str,
    prefix: &str,
    suffix: &str,
    iter: I,
    linker: &str,
) -> String {
    let content: Vec<_> = iter
        .into_iter()
        .map(|(k, v)| format!("{}{}{}", k, linker, v))
        .collect();
    format!("{}{}{}", prefix, content.join(sep), suffix)
}
