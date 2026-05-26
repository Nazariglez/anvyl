use anvyx_runtime::function;

#[inline(always)]
#[function]
pub fn str_len(s: &str) -> i64 {
    s.chars().count() as i64
}

#[inline(always)]
#[function]
pub fn str_contains(s: &str, sub: &str) -> bool {
    s.contains(sub)
}

#[inline(always)]
#[function]
pub fn str_starts_with(s: &str, prefix: &str) -> bool {
    s.starts_with(prefix)
}

#[inline(always)]
#[function]
pub fn str_ends_with(s: &str, suffix: &str) -> bool {
    s.ends_with(suffix)
}

#[inline(always)]
#[function]
pub fn str_find(s: &str, sub: &str) -> i64 {
    s.find(sub)
        .map_or(-1, |byte| s[..byte].chars().count() as i64)
}

#[inline(always)]
#[function]
pub fn str_to_upper(s: &str) -> String {
    s.to_uppercase()
}

#[inline(always)]
#[function]
pub fn str_to_lower(s: &str) -> String {
    s.to_lowercase()
}

#[inline(always)]
#[function]
pub fn str_trim(s: &str) -> String {
    s.trim().to_string()
}

#[inline(always)]
#[function]
pub fn str_trim_start(s: &str) -> String {
    s.trim_start().to_string()
}

#[inline(always)]
#[function]
pub fn str_trim_end(s: &str) -> String {
    s.trim_end().to_string()
}

#[inline(always)]
#[function]
pub fn str_substring(s: &str, start: i64, len: i64) -> Option<String> {
    if start < 0 || len < 0 {
        return None;
    }
    let start = usize::try_from(start).ok()?;
    let len = usize::try_from(len).ok()?;
    let chars = s.chars().collect::<Vec<_>>();
    let end = start.checked_add(len)?;
    (end <= chars.len()).then(|| chars[start..end].iter().collect())
}

#[inline(always)]
#[function]
pub fn str_char_at(s: &str, index: i64) -> Option<String> {
    usize::try_from(index)
        .ok()
        .and_then(|index| s.chars().nth(index))
        .map(|c| c.to_string())
}

#[inline(always)]
#[function]
pub fn str_split(s: &str, sep: &str) -> Vec<String> {
    s.split(sep).map(str::to_string).collect()
}

#[inline(always)]
#[function]
pub fn str_replace(s: &str, from: &str, to: &str) -> String {
    s.replace(from, to)
}

anvyx_runtime::builtin_module! {
    name: "core_string",
    source: include_str!("core_string.anv"),
    exports: [
        str_len, str_contains, str_starts_with, str_ends_with, str_find, str_to_upper,
        str_to_lower, str_trim, str_trim_start, str_trim_end, str_substring, str_char_at,
        str_split, str_replace
    ],
}
