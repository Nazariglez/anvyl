mod printer;
mod trivia;

use anvyx_frontend::{
    lexer::{self, Token},
    parser,
    source::{SourceKind, SourceTable},
    span::SourceSpan,
};
use chumsky::error::{Rich, RichPattern};

pub enum FormatError {
    Lex(String),
    Parse(String),
}

impl std::fmt::Display for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatError::Lex(msg) => write!(f, "Lex error: {msg}"),
            FormatError::Parse(msg) => write!(f, "Parse error: {msg}"),
        }
    }
}

impl std::fmt::Debug for FormatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

fn format_lex_errors(errors: &[Rich<'_, char>]) -> String {
    errors
        .iter()
        .take(5)
        .map(|e| {
            let span = e.span();
            let found = e
                .found()
                .map_or("end of input".to_string(), |c| format!("'{c}'"));
            let context = extract_context_label(e);
            let prefix = if context.is_empty() {
                String::new()
            } else {
                format!(" while {context}")
            };
            format!(
                "byte {}..{}: unexpected {found}{prefix}",
                span.start, span.end
            )
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn format_parse_errors(errors: &[Rich<'_, Token, SourceSpan>]) -> String {
    errors
        .iter()
        .take(5)
        .map(|e| {
            let found = e
                .found()
                .map_or("end of input".to_string(), |tok| format!("{tok:?}"));
            let context = extract_context_label(e);
            let prefix = if context.is_empty() {
                String::new()
            } else {
                format!(" while {context}")
            };
            format!("unexpected {found}{prefix}")
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn extract_context_label<T, S>(rich: &Rich<'_, T, S>) -> String {
    rich.contexts()
        .filter_map(|(pat, _)| match pat {
            RichPattern::Label(s) => Some(s.to_string()),
            _ => None,
        })
        .last()
        .unwrap_or_default()
}

pub fn format_source(source: &str) -> Result<String, FormatError> {
    let mut sources = SourceTable::default();
    let source_id = sources.add(SourceKind::Virtual, "formatter", None, source);
    let tokens = lexer::tokenize(source_id, source)
        .map_err(|errors| FormatError::Lex(format_lex_errors(&errors)))?;

    let ast = parser::parse_ast(&tokens)
        .map_err(|errors| FormatError::Parse(format_parse_errors(&errors)))?;

    let trivia = trivia::scan_trivia(source, &tokens.tokens);
    let mut printer = printer::Printer::new(source, &trivia);
    printer.format_program(&ast);
    Ok(printer.finish())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_fmt(source: &str, expected: &str) {
        let formatted = format_source(source).expect("format failed");
        assert_eq!(formatted, expected);
    }

    fn formatted_program(source: &str) -> anvyx_frontend::ast::Program {
        let formatted = format_source(source).expect("format failed");
        let mut sources = SourceTable::default();
        let source = sources.add(SourceKind::Virtual, "formatter", None, formatted.as_str());
        let tokens = lexer::tokenize(source, &formatted).expect("lex failed");
        parser::parse_ast(&tokens).expect("parse failed")
    }

    #[test]
    fn simple_roundtrip() {
        assert_fmt("fn main() {}", "fn main() {}\n");
    }

    #[test]
    fn preserves_comments() {
        let source = "fn foo() {} // comment\nfn bar() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("// comment"));
        assert!(formatted.contains("fn foo()"));
        assert!(formatted.contains("fn bar()"));
    }

    #[test]
    fn preserves_comment_after_non_ascii() {
        assert_fmt(
            "fn main() { let café = 1; // comment\ncafé; }",
            "fn main() {\n    let café = 1; // comment\n    café;\n}\n",
        );
    }

    #[test]
    fn empty_source() {
        assert_fmt("", "\n");
    }

    #[test]
    fn exact_function() {
        assert_fmt(
            "fn add(a:int,b:int)->int{a+b}",
            "fn add(a: int, b: int) -> int { a + b }\n",
        );
    }

    #[test]
    fn exact_aggregate_method() {
        assert_fmt(
            "struct Counter { value: int, fn get(self) -> int { self.value } }",
            "struct Counter {\n    value: int,\n\n    fn get(self) -> int { self.value }\n}\n",
        );
    }

    #[test]
    fn exact_if_expression() {
        assert_fmt(
            "fn f(x: bool) -> int { if x { 1 } else { 2 } }",
            "fn f(x: bool) -> int {\n    if x {\n        1\n    } else {\n        2\n    }\n}\n",
        );
    }

    #[test]
    fn exact_frontend_imports() {
        assert_fmt(
            "import ext:mem { collect_cycles };",
            "import ext:mem { collect_cycles };\n",
        );
        assert_fmt(
            "pub import pkg:math.vec { self as vec, Vec2 };",
            "pub import pkg:math.vec { self as vec, Vec2 };\n",
        );
        assert_fmt("import ..foo.bar;", "import ..foo.bar;\n");
    }

    #[test]
    fn exact_frontend_types_and_struct_literals() {
        assert_fmt("fn f(x: Box<int>) {}", "fn f(x: Box<int>) {}\n");
        assert_fmt(
            "fn f() { Box<int> { value: 1 }; }",
            "fn f() {\n    Box<int> { value: 1 };\n}\n",
        );
    }

    #[test]
    fn exact_dyn_contract_composition() {
        assert_fmt(
            "fn draw(x:dyn Drawable+Updatable){}",
            "fn draw(x: dyn Drawable + Updatable) {}\n",
        );
    }

    #[test]
    fn exact_downcast_expr() {
        assert_fmt(
            "fn f(){if let enemy=actor as? Enemy{enemy.draw();}}",
            "fn f() {\n    if let enemy = actor as? Enemy {\n        enemy.draw();\n    }\n}\n",
        );
    }

    #[test]
    fn exact_lazy_globals() {
        assert_fmt(
            "lazy let Config=load_config();",
            "lazy let Config = load_config();\n",
        );
        assert_fmt(
            "lazy var Audio:AudioManager=AudioManager.new();",
            "lazy var Audio: AudioManager = AudioManager.new();\n",
        );
        assert_fmt(
            "pub lazy let Assets=load_assets();",
            "pub lazy let Assets = load_assets();\n",
        );
        assert_fmt(
            "@deprecated(\"use Assets2\")\n/// runtime\npub lazy let Assets=load_assets();",
            "@deprecated(\"use Assets2\")\n/// runtime\npub lazy let Assets = load_assets();\n",
        );
        formatted_program("pub lazy var Services = Services.new();");
    }

    #[test]
    fn exact_type_aliases() {
        assert_fmt("type Id=int;", "type Id = int;\n");
        assert_fmt("type Pair<T> = (T,T);", "type Pair<T> = (T, T);\n");
        assert_fmt(
            "type Fixed<T,N:int> = [T;N];",
            "type Fixed<T, N: int> = [T; N];\n",
        );
        assert_fmt("/// ids\npub type Id=int;", "/// ids\npub type Id = int;\n");
        assert_fmt(
            "fn f<T>(x:T)->T{type Item=T;let y:Item=x;y}",
            "fn f<T>(x: T) -> T {\n    type Item = T;\n    let y: Item = x;\n    y\n}\n",
        );
    }

    #[test]
    fn exact_escaping_params() {
        assert_fmt(
            "fn on_click(f:escaping fn()){f();}",
            "fn on_click(f: escaping fn()) {\n    f();\n}\n",
        );
        assert_fmt(
            "fn main(){let f=|cb:escaping fn()|{cb();};}",
            "fn main() {\n    let f = |cb: escaping fn()| {\n        cb();\n    };\n}\n",
        );
        assert_fmt(
            "type Higher=fn(escaping fn());",
            "type Higher = fn(escaping fn());\n",
        );
    }

    #[test]
    fn exact_frontend_extern_members() {
        assert_fmt(
            "extern type T { init; ro: int; rw: int; computed cached: int; computed live: int; fn a(self); fn b(shared self); fn c(var self); }",
            "extern type T {\n    init;\n    ro: int;\n    rw: int;\n    computed cached: int;\n    computed live: int;\n    fn a(self);\n    fn b(shared self);\n    fn c(var self);\n}\n",
        );
    }

    #[test]
    fn exact_frontend_try() {
        assert_fmt(
            "fn f() -> int { try read() + 1 }",
            "fn f() -> int { try read() + 1 }\n",
        );
        assert_fmt(
            "fn f() -> int { (try read()).value }",
            "fn f() -> int { (try read()).value }\n",
        );
        assert_fmt(
            "fn f() -> int { -try read() }",
            "fn f() -> int { -try read() }\n",
        );
        assert_fmt(
            "fn f() -> int { try try nested() }",
            "fn f() -> int { try try nested() }\n",
        );
    }

    #[test]
    fn exact_frontend_methods_and_ternary() {
        assert_fmt(
            "struct S { fn make() -> S { S {} } fn get(self) -> int { 0 } }",
            "struct S {\n    fn make() -> S { S {} }\n    fn get(self) -> int { 0 }\n}\n",
        );
        assert_fmt(
            "extend int { fn abs(self) -> int { self } }",
            "extend int {\n    fn abs(self) -> int { self }\n}\n",
        );
        assert_fmt(
            "fn f(cond: bool) -> int { cond ? 1 + 2 : 3 * 4 }",
            "fn f(cond: bool) -> int { cond ? 1 + 2 : 3 * 4 }\n",
        );
        assert_fmt(
            "fn f(a: bool, b: bool) -> int { (a ? 1 : 2) + (b ? 3 : 4) }",
            "fn f(a: bool, b: bool) -> int { (a ? 1 : 2) + (b ? 3 : 4) }\n",
        );
        assert_fmt(
            "fn f(cond: bool) -> int { (cond ? a : b)() }",
            "fn f(cond: bool) -> int { (cond ? a : b)() }\n",
        );
        assert_fmt(
            "fn f(cond: bool) -> int { (cond ? a : b).x }",
            "fn f(cond: bool) -> int { (cond ? a : b).x }\n",
        );
        assert_fmt(
            "fn f(a: bool, b: bool) -> int { a ? b ? 1 : 2 : 3 }",
            "fn f(a: bool, b: bool) -> int { a ? (b ? 1 : 2) : 3 }\n",
        );
        assert_fmt(
            "fn f(a: bool, b: bool) -> int { (a ? 1 : 2) ? 3 : 4 }",
            "fn f(a: bool, b: bool) -> int { (a ? 1 : 2) ? 3 : 4 }\n",
        );
        assert_fmt(
            "fn f(a: bool, b: bool) -> int { a ? 1 : b ? 2 : 3 }",
            "fn f(a: bool, b: bool) -> int { a ? 1 : b ? 2 : 3 }\n",
        );
        assert_fmt(
            "fn f(cond: bool) -> string { f\"{(cond ? 1 : 2)}\" }",
            "fn f(cond: bool) -> string { f\"{(cond ? 1 : 2)}\" }\n",
        );
        assert_fmt(
            "fn f(cond: bool) -> string { f\"{(cond ? 1 : 2):04}\" }",
            "fn f(cond: bool) -> string { f\"{(cond ? 1 : 2):04}\" }\n",
        );
    }

    #[test]
    fn formatted_ternary_reparses_as_ternary() {
        let program = formatted_program("fn f(cond: bool) -> int { cond ? 1 : 2 }");
        let anvyx_frontend::ast::Stmt::Func(func) = &program.stmts[0].node else {
            panic!("expected function");
        };
        let Some(tail) = &func.node.body.node.tail else {
            panic!("expected tail");
        };
        assert!(matches!(
            tail.node.kind,
            anvyx_frontend::ast::ExprKind::Ternary(_)
        ));
    }

    #[test]
    fn formatted_if_reparses_as_if() {
        let program = formatted_program("fn f(cond: bool) -> int { if cond { 1 } else { 2 } }");
        let anvyx_frontend::ast::Stmt::Func(func) = &program.stmts[0].node else {
            panic!("expected function");
        };
        let Some(tail) = &func.node.body.node.tail else {
            panic!("expected tail");
        };
        assert!(matches!(
            tail.node.kind,
            anvyx_frontend::ast::ExprKind::If(_)
        ));
    }

    #[test]
    fn parse_error() {
        let source = "fn main() {";
        let result = format_source(source);
        assert!(result.is_err());
        match result.unwrap_err() {
            FormatError::Parse(_) => {}
            FormatError::Lex(other) => panic!("expected Parse error, got {other:?}"),
        }
    }

    #[test]
    fn multiple_statements() {
        let source = "fn a() {}\nfn b() {}\nfn c() {}\n";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn a()"));
        assert!(formatted.contains("fn b()"));
        assert!(formatted.contains("fn c()"));
    }

    #[test]
    fn trailing_comment() {
        let source = "fn main() {}\n// end";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("// end"));
    }

    // --- fn formatting ---

    #[test]
    fn fn_with_params() {
        let source = "fn add(a: int, b: int) -> int { a + b }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn add(a: int, b: int) -> int"));
    }

    #[test]
    fn fn_pub() {
        let source = "pub fn greet() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("pub fn greet()"));
    }

    #[test]
    fn fn_type_params() {
        let source = "fn identity<T>(x: T) -> T { x }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn identity<T>(x: T) -> T"));
    }

    #[test]
    fn fn_annotations_doc() {
        let source = "@deprecated\n/// Does stuff\nfn old() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("@deprecated"));
        assert!(formatted.contains("/// Does stuff"));
        assert!(formatted.contains("fn old()"));
        // annotation must come before doc comment
        let ann_pos = formatted.find("@deprecated").unwrap();
        let doc_pos = formatted.find("/// Does stuff").unwrap();
        let fn_pos = formatted.find("fn old()").unwrap();
        assert!(ann_pos < doc_pos && doc_pos < fn_pos);
    }

    #[test]
    fn fn_var_param() {
        let source = "fn push(var list: [int], val: int) {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("var list: [int]"));
    }

    // --- extern fn formatting ---

    #[test]
    fn extern_fn_simple() {
        let source = "extern fn tick();";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("extern fn tick();"));
    }

    #[test]
    fn extern_fn_params_ret() {
        let source = "extern fn add(a: int, b: int) -> int;";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("extern fn add(a: int, b: int) -> int;"));
    }

    // --- extern type formatting ---

    #[test]
    fn extern_type_simple() {
        let source = "extern type Sprite;";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("extern type Sprite;"));
    }

    #[test]
    fn extern_type_with_body() {
        let source = "extern type Point {\n    x: float;\n    y: float;\n    fn distance(self, other: Point) -> float;\n}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("extern type Point {"));
        assert!(formatted.contains("    x: float;"));
        assert!(formatted.contains("    y: float;"));
        assert!(formatted.contains("    fn distance(self, other: Point) -> float;"));
        assert!(formatted.contains('}'));
    }

    #[test]
    fn extern_type_with_init() {
        let source = "extern type Vec2 {\n    init;\n    x: float;\n    y: float;\n}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("    init;"));
        assert!(formatted.contains("    x: float;"));
    }

    // --- import formatting ---

    #[test]
    fn import_module() {
        let source = "import foo.bar;";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("import foo.bar;"));
    }

    #[test]
    fn import_as() {
        let source = "import foo as f;";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("import foo as f;"));
    }

    #[test]
    fn import_selective() {
        let source = "import foo { x, y as z };";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("import foo { x, y as z };"));
    }

    #[test]
    fn import_wildcard() {
        let source = "import foo { * };";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("import foo { * };"));
    }

    #[test]
    fn import_pub() {
        let source = "pub import helpers { * };";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("pub import helpers { * };"));
    }

    // --- const formatting ---

    #[test]
    fn const_simple() {
        let source = "const X = 10;";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("const X = 10;"));
    }

    #[test]
    fn const_typed() {
        let source = "const B: bool = true;";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("const B: bool = true;"));
    }

    #[test]
    fn const_pub() {
        let source = "pub const MAX = 100;";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("pub const MAX = 100;"));
    }

    // --- type formatting (via function signatures) ---

    #[test]
    fn type_list() {
        let source = "fn f(x: [int]) {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("x: [int]"));
    }

    #[test]
    fn type_map() {
        let source = "fn f(x: [string: int]) {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("x: [string: int]"));
    }

    #[test]
    fn type_tuple() {
        let source = "fn f(x: (int, string)) {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("x: (int, string)"));
    }

    #[test]
    fn type_func() {
        let source = "fn f(cb: fn(int) -> bool) {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("cb: fn(int) -> bool"));
    }

    // --- struct formatting ---

    #[test]
    fn struct_simple() {
        let source = "struct Point { x: int, y: int }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("struct Point {"));
        assert!(formatted.contains("    x: int,"));
        assert!(formatted.contains("    y: int,"));
    }

    #[test]
    fn struct_pub() {
        let source = "pub struct Pos { x: float }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("pub struct Pos {"));
        assert!(formatted.contains("    x: float,"));
    }

    #[test]
    fn struct_generic() {
        let source = "struct Pair<T> { first: T, second: T }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("struct Pair<T> {"));
        assert!(formatted.contains("    first: T,"));
        assert!(formatted.contains("    second: T,"));
    }

    #[test]
    fn struct_with_default() {
        let source = "struct Config { width: int = 800, height: int = 600 }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("struct Config {"));
        assert!(formatted.contains("width: int = 800,"));
        assert!(formatted.contains("height: int = 600,"));
    }

    #[test]
    fn struct_with_method() {
        let source = "struct Counter { value: int, fn get(self) -> int { self.value } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("struct Counter {"));
        assert!(formatted.contains("    value: int,"));
        assert!(formatted.contains("fn get(self) -> int"));
    }

    #[test]
    fn struct_static_method() {
        let source = "struct Counter { value: int, fn new(start: int) -> Counter { Counter { value: start } } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn new(start: int)"));
        // no self in static method
        assert!(!formatted.contains("fn new(self"));
    }

    #[test]
    fn struct_var_self_method() {
        let source = "struct Player { hp: int, fn damage(var self, amount: int) { self.hp = self.hp - amount; } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn damage(var self, amount: int)"));
    }

    // --- dataref formatting ---

    #[test]
    fn dataref_simple() {
        let source = "dataref Node { value: int }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("dataref Node {"));
        assert!(formatted.contains("    value: int,"));
    }

    #[test]
    fn dataref_with_method() {
        let source = "dataref Node { value: int, fn get(self) -> int { self.value } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("dataref Node {"));
        assert!(formatted.contains("fn get(self) -> int"));
    }

    // --- enum formatting ---

    #[test]
    fn enum_unit_variants() {
        let source = "enum Color { Red, Green, Blue }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("enum Color {"));
        assert!(formatted.contains("    Red,"));
        assert!(formatted.contains("    Green,"));
        assert!(formatted.contains("    Blue,"));
    }

    #[test]
    fn enum_tuple_variant() {
        let source = "enum Wrapper<T> { Some(T), None }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("enum Wrapper<T> {"));
        assert!(formatted.contains("    Some(T),"));
        assert!(formatted.contains("    None,"));
    }

    #[test]
    fn enum_struct_variant() {
        let source = "enum Event { Click { x: int, y: int }, Quit }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("enum Event {"));
        assert!(formatted.contains("Click { x: int, y: int },"));
        assert!(formatted.contains("    Quit,"));
    }

    #[test]
    fn enum_generic() {
        let source = "enum Result<T, E> { Ok(T), Err(E) }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("enum Result<T, E> {"));
        assert!(formatted.contains("    Ok(T),"));
        assert!(formatted.contains("    Err(E),"));
    }

    #[test]
    fn enum_pub() {
        let source = "pub enum Direction { Up, Down }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("pub enum Direction {"));
    }

    // --- extend formatting ---

    #[test]
    fn extend_simple() {
        let source = "extend float { fn double(self) -> float { self * 2.0 } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("extend float {"));
        assert!(formatted.contains("fn double(self) -> float"));
    }

    #[test]
    fn extend_var_self() {
        let source = "struct Counter { value: int }\nextend Counter { fn reset(var self) { self.value = 0; } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn reset(var self)"));
    }

    #[test]
    fn extend_generic() {
        let source = "struct Pair<T> { first: T, second: T }\nextend Pair<T> { fn first_val(self) -> T { self.first } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("extend Pair<T> {"));
        assert!(formatted.contains("fn first_val(self) -> T"));
    }

    #[test]
    fn extend_pub() {
        let source = "pub extend float { fn half(self) -> float { self / 2.0 } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("pub extend float {"));
    }

    #[test]
    fn extend_cast_from() {
        let source = "struct Vec2 { x: float, y: float }\nextend Vec2 { cast from(v: float) { Vec2 { x: v, y: v } } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("extend Vec2 {"));
        assert!(formatted.contains("cast from(v: float)"));
    }

    #[test]
    fn extend_dataref() {
        let source = "dataref Node { value: int }\nextend dataref Node { fn get(self) -> int { self.value } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("extend dataref Node {"));
    }

    #[test]
    fn extend_compound_type() {
        let source = "extend<T> [T] { fn first(self) -> T { self[0] } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("extend<T> [T] {"));
    }

    #[test]
    fn extend_doc_on_method() {
        let source = "extend float {\n    /// Returns double the value.\n    fn double(self) -> float { self * 2.0 }\n}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("/// Returns double the value."));
        assert!(formatted.contains("fn double(self) -> float"));
    }

    // --- statement formatting ---

    #[test]
    fn let_simple() {
        let source = "fn f() { let x = 5; }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("    let x = 5;"));
    }

    #[test]
    fn var_simple() {
        let source = "fn f() { var x = 5; }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("    var x = 5;"));
    }

    #[test]
    fn let_typed() {
        let source = "fn f() { let x: int = 5; }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("    let x: int = 5;"));
    }

    #[test]
    fn let_tuple_pattern() {
        let source = "fn f(p: (int, string)) { let (n, s) = p; }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("let (n, s) ="));
    }

    #[test]
    fn let_wildcard() {
        let source = "fn f() { let _ = 5; }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("    let _ = 5;"));
    }

    #[test]
    fn return_unit() {
        let source = "fn f() { return; }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("    return;"));
    }

    #[test]
    fn return_expr() {
        let source = "fn f() -> int { let x = 5; return x; }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("    return x;"));
    }

    #[test]
    fn while_simple() {
        let source = "fn f() { while true { } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("while true {}"));
    }

    #[test]
    fn while_let_simple() {
        let source = "fn f(x: int?) { while let val = x { } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("while let val = x {}"));
    }

    #[test]
    fn for_stmt() {
        for (source, expected) in [
            ("fn f() { for x in 0..10 { } }", "for x in 0..10 {}"),
            ("fn f() { for x in rev 0..10 { } }", "for x in rev 0..10 {}"),
            (
                "fn f() { for (a, b) in get() { } }",
                "for (a, b) in get() {}",
            ),
            ("fn f() { for var x in xs { } }", "for var x in xs {}"),
            (
                "fn f() { for var (a, b) in get() { } }",
                "for var (a, b) in get() {}",
            ),
            (
                "fn f() { for var x in rev xs step 2 { } }",
                "for var x in rev xs step 2 {}",
            ),
            ("fn f() { for i, var x in xs { } }", "for i, var x in xs {}"),
        ] {
            let formatted = format_source(source).expect("format failed");
            assert!(formatted.contains(expected));
        }
    }

    #[test]
    fn break_continue() {
        let source = "fn f() { while true { break; } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("    break;"));
    }

    #[test]
    fn defer_expr() {
        let source = "fn f() { defer cleanup(); }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("defer cleanup();"));
    }

    #[test]
    fn defer_block_stmt() {
        let source = "fn f() { defer { let _x = 5; } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("defer {"));
        assert!(formatted.contains("let _x = 5;"));
    }

    #[test]
    fn let_else_simple() {
        let source = "fn f(x: int?) { let val = x else { return; } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("let val = x else {"));
        assert!(formatted.contains("return;"));
    }

    #[test]
    fn empty_fn_body() {
        let source = "fn f() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn f() {}"));
    }

    #[test]
    fn fn_tail_only() {
        let source = "fn add(a: int, b: int) -> int { a + b }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ a + b }"));
    }

    #[test]
    fn fn_stmts_and_tail() {
        let source = "fn f() -> int { let x = 5; x }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("    let x = 5;"));
        assert!(formatted.contains("    x"));
    }

    #[test]
    fn let_inferred_enum_via_let_else() {
        let source = "fn f(x: int?) { let .Some(val) = x else { return; } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("let .Some(val) = x else {"));
    }

    // --- expressions ---

    #[test]
    fn expr_binary() {
        let source = "fn f() -> int { 1 + 2 * 3 }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ 1 + 2 * 3 }"));
    }

    #[test]
    fn expr_unary() {
        let source = "fn f(x: int) -> int { -x }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ -x }"));
    }

    #[test]
    fn expr_assign() {
        let source = "fn f() { var x = 0; x += 1; }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("x += 1;"));
    }

    #[test]
    fn expr_call_simple() {
        let source = "fn f() { foo(1, 2); }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("foo(1, 2);"));
    }

    #[test]
    fn expr_method_chain() {
        let source = "fn f() -> string { foo().bar().baz() }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("foo().bar().baz()"));
    }

    #[test]
    fn expr_field_access() {
        let source = "struct P { x: int }\nfn f(p: P) -> int { p.x }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ p.x }"));
    }

    #[test]
    fn expr_index() {
        let source = "fn f(a: [int]) -> int { a[0] }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ a[0] }"));
    }

    #[test]
    fn expr_tuple_index() {
        let source = "fn f(t: (int, string)) -> int { t.0 }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ t.0 }"));
    }

    #[test]
    fn expr_if_else() {
        let source = "fn f(x: bool) -> int { if x { 1 } else { 2 } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("if x {\n"));
        assert!(formatted.contains("} else {\n"));
    }

    #[test]
    fn expr_if_else_if() {
        let source = "fn f(x: int) -> int { if x == 1 { 10 } else if x == 2 { 20 } else { 30 } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("} else if x == 2 {\n"));
    }

    #[test]
    fn expr_if_let() {
        let source = "fn f(x: int?) { if let val = x { println(val); } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("if let val = x {"));
    }

    #[test]
    fn pattern_head_var() {
        for (source, expected) in [
            (
                "fn f(x: int?) { if var val? = x { val = 1; } }",
                "if var val? = x {",
            ),
            (
                "fn f(x: int?) { while var val? = x { val = 1; break; } }",
                "while var val? = x {",
            ),
            (
                "fn f(x: int?) { var val? = x else { return; } val = 1; }",
                "var val? = x else {",
            ),
            (
                "fn f(x: int?) { match var x { Option.Some(v) => { v = 1; }, Option.None => {} } }",
                "match var x {",
            ),
        ] {
            let formatted = format_source(source).expect("format failed");
            assert!(formatted.contains(expected));
        }
    }

    #[test]
    fn expr_match() {
        let source = r#"fn f(x: int) -> string { match x { 1 => "one", _ => "other" } }"#;
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("match x {"));
        assert!(formatted.contains(r#"1 => "one","#));
        assert!(formatted.contains(r#"_ => "other","#));
    }

    #[test]
    fn expr_dynamic_match() {
        let source = "fn f(actor: dyn Drawable) { match actor as? { Enemy(enemy) => enemy.attack(), other => other.draw() } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("match actor as? {"));
        assert!(formatted.contains("Enemy(enemy) => enemy.attack(),"));
        assert!(formatted.contains("other => other.draw(),"));
    }

    #[test]
    fn expr_dynamic_match_var_wildcards() {
        let source = "fn f(var actor: dyn Drawable) { match var actor as? { Enemy(_) => actor.draw(), _ => actor.draw(), } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("match var actor as? {"));
        assert!(formatted.contains("Enemy(_) => actor.draw(),"));
        assert!(formatted.contains("_ => actor.draw(),"));
    }

    #[test]
    fn expr_tuple() {
        let source = "fn f() -> (int, int) { (1, 2) }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ (1, 2) }"));
    }

    #[test]
    fn expr_struct_literal() {
        let source = "struct P { x: int, y: int }\nfn f() -> P { P { x: 1, y: 2 } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("P { x: 1, y: 2 }"));
    }

    #[test]
    fn expr_struct_shorthand() {
        let source = "struct P { x: int, y: int }\nfn f(x: int, y: int) -> P { P { x, y } }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("P { x, y }"));
    }

    #[test]
    fn expr_array() {
        let source = "fn f() -> [int] { [1, 2, 3] }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("[1, 2, 3]"));
    }

    #[test]
    fn expr_array_fill() {
        let source = "fn f() -> [int; 5] { [0; 5] }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("[0; 5]"));
    }

    #[test]
    fn expr_map() {
        let source = r#"fn f() -> [string: int] { ["a": 1, "b": 2] }"#;
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains(r#"["a": 1, "b": 2]"#));
    }

    #[test]
    fn expr_empty_map() {
        let source = "fn f() -> [string: int] { [:] }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("[:]"));
    }

    #[test]
    fn expr_range() {
        let source = "fn f() { for i in 0..10 {} }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("in 0..10"));
    }

    #[test]
    fn expr_range_inclusive() {
        let source = "fn f() { for i in 0..=10 {} }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("in 0..=10"));
    }

    #[test]
    fn expr_cast() {
        let source = "fn f(x: int) -> float { x as float }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("x as float"));
    }

    #[test]
    fn expr_string_interp() {
        let source = r#"fn f(x: string) -> string { f"hi {x}" }"#;
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains(r#"f"hi {x}""#));
    }

    #[test]
    fn expr_string_interp_expr() {
        let source = r#"fn f(x: int) -> string { f"val: {x + 1}" }"#;
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains(r#"f"val: {x + 1}""#));
    }

    #[test]
    fn expr_string_fmt_spec() {
        let source = r#"fn f(x: int) -> string { f"{x:04}" }"#;
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains(r#"f"{x:04}""#));
    }

    #[test]
    fn expr_lambda_no_params() {
        let source = "fn f() { let cb = || 42; }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("|| 42"));
    }

    #[test]
    fn expr_lambda_typed() {
        let source = "fn f() { let cb = |x: int| x + 1; }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("|x: int| x + 1"));
    }

    #[test]
    fn expr_inferred_enum() {
        let source = "fn f() -> int? { .Some(42) }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains(".Some(42)"));
    }

    #[test]
    fn const_expr_value() {
        let source = "const X = 1 + 2;";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("const X = 1 + 2;"));
    }

    #[test]
    fn binding_complex_expr() {
        let source = "fn f() { let x = foo(1, 2) + bar(); }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("let x = foo(1, 2) + bar();"));
    }

    // --- parenthesization and float literals ---

    #[test]
    fn expr_parens_unary_binary() {
        let source = "fn f(t: bool, f: bool) -> bool { !(t && f) }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("!(t && f)"));
    }

    #[test]
    fn expr_parens_precedence() {
        let source = "fn f(a: int, b: int, c: int) -> int { (a + b) * c }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("(a + b) * c"));
    }

    #[test]
    fn expr_parens_not_needed() {
        let source = "fn f(a: int, b: int, c: int) -> int { a + b * c }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ a + b * c }"));
    }

    #[test]
    fn expr_parens_right_assoc() {
        let source = "fn f(a: int, b: int, c: int) -> int { a - (b - c) }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("a - (b - c)"));
    }

    #[test]
    fn expr_parens_left_assoc_no_parens() {
        let source = "fn f(a: int, b: int, c: int) -> int { a - b - c }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("{ a - b - c }"));
    }

    #[test]
    fn expr_parens_cast_child() {
        let source = "fn f(a: int, b: int) -> float { (a + b) as float }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("(a + b) as float"));
    }

    #[test]
    fn expr_parens_precedence_boundaries() {
        let cases = [
            ("fn f() { let x = a ?? b || c; }", "a ?? b || c"),
            ("fn f() { let x = a && b ?? c; }", "a && b ?? c"),
            ("fn f() { let x = (a ? b : c).x; }", "(a ? b : c).x"),
            ("fn f() { let x = a ? b : c ? d : e; }", "a ? b : c ? d : e"),
            (
                "fn f() { let x = (a ? b : c) ? d : e; }",
                "(a ? b : c) ? d : e",
            ),
            (
                "fn f() { let x = (try read()).value; }",
                "(try read()).value",
            ),
            ("fn f() { let x = 0..10 + 1; }", "0..10 + 1"),
            ("fn f() { let x = (0..10) + 1; }", "(0..10) + 1"),
        ];

        for (source, expected) in cases {
            let formatted = format_source(source).expect("format failed");
            assert!(
                formatted.contains(expected),
                "expected `{expected}` in `{formatted}`"
            );
        }
    }

    #[test]
    fn expr_parens_negation_eq() {
        let source = "fn f(x: int) -> bool { !(x == 6) }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("!(x == 6)"));
    }

    #[test]
    fn float_literal_preserves_dot_zero() {
        let source = "fn f() -> float { 4.0 }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("4.0"));
    }

    #[test]
    fn float_literal_fractional() {
        let source = "fn f() -> float { 3.14 }";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("3.14"));
    }

    // -------------------------------------------------------------------------
    // Blank line normalization
    // -------------------------------------------------------------------------

    #[test]
    fn collapse_multiple_blank_lines() {
        let source = "fn a() {}\n\n\n\nfn b() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn a() {}\n\nfn b()"));
        assert!(!formatted.contains("\n\n\n"));
    }

    #[test]
    fn collapse_blank_lines_in_block() {
        let source = "fn main() {\n    let x = 1;\n\n\n\n    let y = 2;\n}";
        let formatted = format_source(source).expect("format failed");
        assert!(!formatted.contains("\n\n\n"));
    }

    #[test]
    fn blank_line_between_functions() {
        let source = "fn a() {}\nfn b() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("fn a() {}\n\nfn b()"));
    }

    #[test]
    fn blank_line_between_struct_and_fn() {
        let source = "struct A {\n    x: int,\n}\nfn main() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("}\n\nfn main()"));
    }

    #[test]
    fn no_blank_line_between_imports() {
        let source = "import foo;\nimport bar;\n\nfn main() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("import foo;\nimport bar;\n"));
    }

    #[test]
    fn no_blank_line_between_consts() {
        let source = "const A = 1;\nconst B = 2;\n\nfn main() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("const A = 1;\nconst B = 2;\n"));
    }

    #[test]
    fn blank_line_between_import_and_fn() {
        let source = "import foo;\nfn main() {}";
        let formatted = format_source(source).expect("format failed");
        assert!(formatted.contains("import foo;\n\nfn main()"));
    }

    #[test]
    fn blank_line_insertion_idempotent() {
        let source = "fn a() {}\nfn b() {}\nfn c() {}";
        let first = format_source(source).expect("format failed");
        let second = format_source(&first).expect("format failed");
        assert_eq!(first, second);
    }

    // -------------------------------------------------------------------------
    // Idempotency test
    // -------------------------------------------------------------------------

    #[test]
    fn idempotency() {
        let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
        let tests_dir = manifest_dir
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("tests");

        let files = walk_anv_files(&tests_dir);
        let mut tested = 0;
        let mut skipped = 0;

        for entry in &files {
            let source = std::fs::read_to_string(entry).unwrap();

            if source.starts_with("// @helper") {
                skipped += 1;
                continue;
            }

            let Ok(first) = format_source(&source) else {
                skipped += 1;
                continue;
            };

            let second = format_source(&first).unwrap_or_else(|e| {
                panic!(
                    "Formatted output failed to re-parse: {}\nFile: {}",
                    e,
                    entry.display()
                );
            });

            assert_eq!(
                first,
                second,
                "Idempotency failure: {}\n--- first ---\n{}\n--- second ---\n{}",
                entry.display(),
                first,
                second
            );
            tested += 1;
        }

        assert!(
            tested > 1000,
            "Expected >1000 files tested, got only {tested}"
        );
        eprintln!("Idempotency: {tested} tested, {skipped} skipped");
    }

    fn walk_anv_files(dir: &std::path::Path) -> Vec<std::path::PathBuf> {
        let mut files = vec![];
        walk_anv_files_rec(dir, &mut files);
        files.sort();
        files
    }

    fn walk_anv_files_rec(dir: &std::path::Path, files: &mut Vec<std::path::PathBuf>) {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return;
        };
        for entry in entries {
            let Ok(entry) = entry else {
                continue;
            };
            let path = entry.path();
            if path.is_dir() {
                let name = entry.file_name();
                if !name.to_str().is_some_and(|s| s.starts_with('.')) {
                    walk_anv_files_rec(&path, files);
                }
            } else if path.extension().is_some_and(|ext| ext == "anv") {
                files.push(path);
            }
        }
    }
}
