use anvyx_runtime::{AnvList, AnvString, Ctx, ListStorage, function};

#[inline]
#[function]
pub fn str_len(s: &str) -> i64 {
    s.chars().count() as i64
}

#[inline]
#[function]
pub fn str_contains(s: &str, sub: &str) -> bool {
    s.contains(sub)
}

#[inline]
#[function]
pub fn str_starts_with(s: &str, prefix: &str) -> bool {
    s.starts_with(prefix)
}

#[inline]
#[function]
pub fn str_ends_with(s: &str, suffix: &str) -> bool {
    s.ends_with(suffix)
}

#[inline]
#[function]
pub fn str_find(s: &str, sub: &str) -> i64 {
    s.find(sub)
        .map_or(-1, |byte| s[..byte].chars().count() as i64)
}

#[inline]
#[function]
pub fn str_to_upper(s: &str) -> AnvString {
    AnvString::from(s.to_uppercase())
}

#[inline]
#[function]
pub fn str_to_lower(s: &str) -> AnvString {
    AnvString::from(s.to_lowercase())
}

#[inline]
#[function]
pub fn str_trim(s: &str) -> AnvString {
    AnvString::from(s.trim())
}

#[inline]
#[function]
pub fn str_trim_start(s: &str) -> AnvString {
    AnvString::from(s.trim_start())
}

#[inline]
#[function]
pub fn str_trim_end(s: &str) -> AnvString {
    AnvString::from(s.trim_end())
}

#[inline]
#[function]
pub fn str_substring(s: &str, start: i64, len: i64) -> Option<AnvString> {
    if start < 0 || len < 0 {
        return None;
    }
    let start = usize::try_from(start).ok()?;
    let len = usize::try_from(len).ok()?;
    let chars = s.chars().collect::<Vec<_>>();
    let end = start.checked_add(len)?;
    (end <= chars.len()).then(|| AnvString::from(chars[start..end].iter().collect::<String>()))
}

#[inline]
#[function]
pub fn str_char_at(s: &str, index: i64) -> Option<AnvString> {
    usize::try_from(index)
        .ok()
        .and_then(|index| s.chars().nth(index))
        .map(|c| AnvString::from(c.to_string()))
}

#[inline]
#[function(ctx)]
pub fn str_split<'cx>(ctx: &mut Ctx<'cx, '_>, s: &str, sep: &str) -> AnvList<'cx, AnvString> {
    let storage = ctx
        .heap()
        .register_untracked::<ListStorage<'_, AnvString>>();
    AnvList::from_elems(ctx, storage, s.split(sep).map(AnvString::from))
}

#[inline]
#[function]
pub fn str_replace(s: &str, from: &str, to: &str) -> AnvString {
    AnvString::from(s.replace(from, to))
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
