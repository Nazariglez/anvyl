use anvyx_runtime::{AnvString, function};

#[inline]
#[function]
pub fn char_codepoint(c: char) -> i64 {
    i64::from(c as u32)
}

#[inline]
#[function]
pub fn char_from_codepoint_ext(codepoint: i64) -> Option<char> {
    u32::try_from(codepoint).ok().and_then(char::from_u32)
}

#[inline]
#[function]
pub fn char_to_string(c: char) -> AnvString {
    c.to_string().into()
}

anvyx_runtime::builtin_module! {
    name: "core_char",
    source: include_str!("core_char.anv"),
    exports: [char_codepoint, char_from_codepoint_ext, char_to_string],
}
