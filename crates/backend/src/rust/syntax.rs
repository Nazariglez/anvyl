use std::fmt::Write;

use anvyx_frontend::ast::{BinaryOp, UnaryOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(super) struct FormatSpec {
    pub fill: char,
    pub align: Option<FormatAlign>,
    pub sign: FormatSign,
    pub zero_pad: bool,
    pub width: Option<u32>,
    pub precision: Option<u32>,
    pub kind: FormatKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum FormatAlign {
    Left,
    Right,
    Center,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(super) enum FormatSign {
    #[default]
    Default,
    Always,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(super) enum FormatKind {
    #[default]
    Default,
    Hex,
    HexUpper,
    Binary,
    Exp,
    ExpUpper,
}

pub(super) fn comma(items: impl IntoIterator<Item = String>) -> String {
    items.into_iter().collect::<Vec<_>>().join(", ")
}

pub(super) fn block_expr(stmts: impl IntoIterator<Item = String>, tail: Option<String>) -> String {
    let mut parts = vec!["{".to_string()];
    parts.extend(stmts);
    if let Some(tail) = tail {
        parts.push(tail);
    }
    parts.push("}".to_string());
    parts.join(" ")
}

pub(super) fn match_expr(scrutinee: &str, arms: impl IntoIterator<Item = String>) -> String {
    let arms = comma(arms);
    if arms.is_empty() {
        format!("match {scrutinee} {{}}")
    } else {
        format!("match {scrutinee} {{ {arms} }}")
    }
}

pub(super) fn field_init(name: &str, expr: impl std::fmt::Display) -> String {
    format!("{name}: {expr}")
}

pub(super) fn struct_lit(name: &str, fields: impl IntoIterator<Item = String>) -> String {
    format!("{name} {{ {} }}", comma(fields))
}

pub(super) fn variant_path(enm: &str, variant: &str) -> String {
    format!("{enm}::{variant}")
}

pub(super) fn tuple_variant(path: &str, fields: impl IntoIterator<Item = String>) -> String {
    format!("{path}({})", comma(fields))
}

pub(super) fn struct_variant(path: &str, fields: impl IntoIterator<Item = String>) -> String {
    struct_lit(path, fields)
}

pub(super) fn unit_variant_pattern(path: &str) -> String {
    path.to_string()
}

pub(super) fn tuple_variant_pattern(path: &str) -> String {
    format!("{path}(..)")
}

pub(super) fn struct_variant_pattern(path: &str) -> String {
    format!("{path} {{ .. }}")
}

pub(super) fn unary_op(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "-",
        UnaryOp::Not | UnaryOp::BitNot => "!",
    }
}

pub(super) fn binary_op(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Rem => "%",
        BinaryOp::Eq => "==",
        BinaryOp::NotEq => "!=",
        BinaryOp::LessThan => "<",
        BinaryOp::GreaterThan => ">",
        BinaryOp::LessThanEq => "<=",
        BinaryOp::GreaterThanEq => ">=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
        BinaryOp::Xor => "^",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::Coalesce => panic!("verified RIR excludes coalesce"),
    }
}

pub(super) fn format_fragment(spec: FormatSpec) -> String {
    let mut fragment = String::from("{");
    if spec != FormatSpec::default() {
        fragment.push(':');
        if let Some(align) = spec.align {
            let implicit_zero_align =
                spec.zero_pad && spec.fill == '0' && align == FormatAlign::Right;
            if !implicit_zero_align {
                if spec.fill != '\0' {
                    push_format_char(&mut fragment, spec.fill);
                }
                fragment.push(match align {
                    FormatAlign::Left => '<',
                    FormatAlign::Right => '>',
                    FormatAlign::Center => '^',
                });
            }
        }
        if spec.sign == FormatSign::Always {
            fragment.push('+');
        }
        if spec.zero_pad {
            fragment.push('0');
        }
        if let Some(width) = spec.width {
            write!(fragment, "{width}").expect("write to string failed");
        }
        if let Some(precision) = spec.precision {
            write!(fragment, ".{precision}").expect("write to string failed");
        }
        fragment.push_str(match spec.kind {
            FormatKind::Default => "",
            FormatKind::Hex => "x",
            FormatKind::HexUpper => "X",
            FormatKind::Binary => "b",
            FormatKind::Exp => "e",
            FormatKind::ExpUpper => "E",
        });
    }
    fragment.push('}');
    fragment
}

pub(super) fn rust_string(text: &str) -> String {
    let mut out = String::from("\"");
    for ch in text.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch.is_control() => {
                write!(out, "\\u{{{:x}}}", ch as u32).expect("write to string");
            }
            ch => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn push_format_char(out: &mut String, ch: char) {
    match ch {
        '{' => out.push_str("{{"),
        '}' => out.push_str("}}"),
        ch => out.push(ch),
    }
}

#[cfg(test)]
mod tests {
    use super::{
        FormatAlign, FormatKind, FormatSign, FormatSpec, block_expr, field_init, format_fragment,
        match_expr, rust_string, struct_lit, struct_variant, struct_variant_pattern, tuple_variant,
        tuple_variant_pattern, unit_variant_pattern, variant_path,
    };

    #[test]
    fn escapes_rust_strings() {
        assert_eq!(rust_string("a\\b\"c\n\r\t"), r#""a\\b\"c\n\r\t""#);
        assert_eq!(rust_string("\u{1f}"), r#""\u{1f}""#);
    }

    #[test]
    fn renders_default_format_fragment() {
        assert_eq!(format_fragment(FormatSpec::default()), "{}");
    }

    #[test]
    fn renders_block_expressions() {
        assert_eq!(
            block_expr(
                [
                    "let mut out = String::new();".to_string(),
                    "out.push('x');".to_string()
                ],
                Some("out".to_string())
            ),
            "{ let mut out = String::new(); out.push('x'); out }"
        );
        assert_eq!(
            block_expr(["map.insert(k, v);".to_string()], None),
            "{ map.insert(k, v); }"
        );
    }

    #[test]
    fn renders_match_expressions() {
        assert_eq!(
            match_expr("&value", ["A => B".to_string(), "C => D".to_string()]),
            "match &value { A => B, C => D }"
        );
        assert_eq!(match_expr("never", []), "match never {}");
    }

    #[test]
    fn renders_record_and_variant_syntax() {
        let path = variant_path("Message", "Text");

        assert_eq!(field_init("x", "1".to_string()), "x: 1");
        assert_eq!(
            struct_lit("Point", [field_init("x", "1".to_string())]),
            "Point { x: 1 }"
        );
        assert_eq!(
            tuple_variant(&path, ["value".to_string()]),
            "Message::Text(value)"
        );
        assert_eq!(
            struct_variant(&path, [field_init("text", "value".to_string())]),
            "Message::Text { text: value }"
        );
        assert_eq!(unit_variant_pattern(&path), "Message::Text");
        assert_eq!(tuple_variant_pattern(&path), "Message::Text(..)");
        assert_eq!(struct_variant_pattern(&path), "Message::Text { .. }");
    }

    #[test]
    fn renders_format_fill_braces() {
        assert_eq!(
            format_fragment(FormatSpec {
                fill: '{',
                align: Some(FormatAlign::Right),
                width: Some(5),
                ..FormatSpec::default()
            }),
            "{:{{>5}"
        );
        assert_eq!(
            format_fragment(FormatSpec {
                fill: '}',
                align: Some(FormatAlign::Left),
                width: Some(5),
                ..FormatSpec::default()
            }),
            "{:}}<5}"
        );
    }

    #[test]
    fn renders_full_format_fragment() {
        assert_eq!(
            format_fragment(FormatSpec {
                fill: '*',
                align: Some(FormatAlign::Center),
                sign: FormatSign::Always,
                zero_pad: true,
                width: Some(8),
                precision: Some(2),
                kind: FormatKind::HexUpper,
            }),
            "{:*^+08.2X}"
        );
    }
}
