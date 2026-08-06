use std::fmt::Write;

use anvyx_frontend::ast::{BinaryOp, FormatAlign, FormatKind, FormatSign, FormatSpec, UnaryOp};

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
        BinaryOp::Xor => "^",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::And | BinaryOp::Or | BinaryOp::Coalesce => {
            panic!("verified RIR excludes lazy binary operators")
        }
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
    format!("{text:?}")
}

pub(super) fn rust_char(ch: char) -> String {
    format!("{ch:?}")
}

fn push_format_char(out: &mut String, ch: char) {
    match ch {
        '{' => out.push_str("{{"),
        '}' => out.push_str("}}"),
        ch => out.push(ch),
    }
}
