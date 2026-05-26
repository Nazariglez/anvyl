// many items here are only used via generated macro,
// so we can safely ignore dead_code warnings
#![allow(dead_code)]
#![allow(clippy::needless_pass_by_value)]

use anvyx_lang::{
    AnvyxConvert, AnvyxExternType, AnvyxFn, ExternDecl, ExternHandle, ExternHandleData,
    ExternTypeDeclConst, ManagedRc, OPTION_TYPE_ID, RuntimeError, Value, export_fn, export_type,
};

fn noop_drop(_id: u64) {}

fn extern_handle(id: u64) -> Value {
    Value::ExternHandle(ManagedRc::new(ExternHandleData {
        id,
        drop_fn: noop_drop,
        type_name: "Extern",
        to_string_fn: |_| "<Extern>".to_string(),
    }))
}

#[export_fn]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[test]
fn export_fn_generates_companion() {
    let (name, handler) = __anvyx_export_add();
    assert_eq!(name, "add");
    let result = handler(vec![Value::Int(3), Value::Int(4)]).unwrap();
    assert_eq!(result, Value::Int(7));
}

#[export_fn(name = "custom_name")]
fn my_fn(x: i64) -> i64 {
    x * 2
}

#[test]
fn export_fn_name_override() {
    let (name, _) = __anvyx_export_my_fn();
    assert_eq!(name, "custom_name");
}

#[export_fn]
fn greet(name: String) -> String {
    format!("hi {name}")
}

#[test]
fn export_fn_string_params() {
    let (_, handler) = __anvyx_export_greet();
    let result = handler(vec![Value::String(ManagedRc::new("world".to_string()))]).unwrap();
    assert_eq!(
        result,
        Value::String(ManagedRc::new("hi world".to_string()))
    );
}

#[export_fn]
fn greet_ref(name: &str) -> String {
    format!("hi {name}")
}

#[test]
fn export_fn_str_ref_param() {
    let (name, handler) = __anvyx_export_greet_ref();
    assert_eq!(name, "greet_ref");
    let result = handler(vec![Value::String(ManagedRc::new("world".to_string()))]).unwrap();
    assert_eq!(
        result,
        Value::String(ManagedRc::new("hi world".to_string()))
    );
}

#[export_fn]
fn concat_ref(a: &str, b: &str) -> String {
    format!("{a}{b}")
}

#[test]
fn export_fn_multiple_str_ref_params() {
    let (_, handler) = __anvyx_export_concat_ref();
    let result = handler(vec![
        Value::String(ManagedRc::new("hello ".to_string())),
        Value::String(ManagedRc::new("world".to_string())),
    ])
    .unwrap();
    assert_eq!(
        result,
        Value::String(ManagedRc::new("hello world".to_string()))
    );
}

#[test]
fn export_fn_str_ref_wrong_type() {
    let (_, handler) = __anvyx_export_greet_ref();
    let result = handler(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn export_fn_str_ref_param_decl() {
    let decl: ExternDecl = __ANVYX_DECL_GREET_REF();
    assert_eq!(decl.name, "greet_ref");
    assert_eq!(decl.params, &[("name", "string")]);
    assert_eq!(decl.ret, "string");
}

#[export_fn]
fn noop() {}

#[test]
fn export_fn_void_return() {
    let (_, handler) = __anvyx_export_noop();
    let result = handler(vec![]).unwrap();
    assert_eq!(result, Value::Nil);
}

#[test]
fn export_fn_wrong_type_returns_error() {
    let (_, handler) = __anvyx_export_add();
    let result = handler(vec![Value::Bool(true), Value::Int(1)]);
    assert!(result.is_err());
}

#[export_fn]
fn scale(x: f32, factor: f32) -> f32 {
    x * factor
}

#[test]
fn export_fn_float_params() {
    let (name, handler) = __anvyx_export_scale();
    assert_eq!(name, "scale");
    let result = handler(vec![Value::Float(2.5), Value::Float(4.0)]).unwrap();
    assert_eq!(result, Value::Float(10.0));
}

#[export_fn]
fn toggle(flag: bool) -> bool {
    !flag
}

#[test]
fn export_fn_bool_params() {
    let (name, handler) = __anvyx_export_toggle();
    assert_eq!(name, "toggle");
    let result = handler(vec![Value::Bool(true)]).unwrap();
    assert_eq!(result, Value::Bool(false));
}

// -- provider! tests --

mod math_mod {
    use anvyx_lang::export_fn;

    #[export_fn]
    pub fn double(x: i64) -> i64 {
        x * 2
    }

    #[export_fn(name = "triple")]
    pub fn triple_val(x: i64) -> i64 {
        x * 3
    }
}

mod greet_mod {
    use anvyx_lang::export_fn;

    #[export_fn]
    pub fn hello(name: String) -> String {
        format!("hello {name}")
    }
}

// provider! with module-qualified paths
anvyx_lang::provider!(math_mod::double, math_mod::triple_val, greet_mod::hello);

#[test]
fn provider_generates_anvyx_externs() {
    let externs = anvyx_externs();
    assert_eq!(externs.len(), 3);
    assert!(externs.contains_key("double"));
    assert!(externs.contains_key("triple"));
    assert!(externs.contains_key("hello"));
}

#[test]
fn provider_handlers_work_correctly() {
    let externs = anvyx_externs();

    let result = externs["double"](vec![Value::Int(5)]).unwrap();
    assert_eq!(result, Value::Int(10));

    let result = externs["triple"](vec![Value::Int(4)]).unwrap();
    assert_eq!(result, Value::Int(12));

    let result =
        externs["hello"](vec![Value::String(ManagedRc::new("world".to_string()))]).unwrap();
    assert_eq!(
        result,
        Value::String(ManagedRc::new("hello world".to_string()))
    );
}

// provider! with bare idents (no module prefix) — functions defined at the same scope
mod flat_mod {
    use anvyx_lang::export_fn;

    #[export_fn]
    pub fn inc(x: i64) -> i64 {
        x + 1
    }

    #[export_fn]
    pub fn dec(x: i64) -> i64 {
        x - 1
    }

    anvyx_lang::provider!(inc, dec);
}

#[test]
fn provider_bare_ident() {
    let externs = flat_mod::anvyx_externs();
    assert_eq!(externs.len(), 2);

    let result = externs["inc"](vec![Value::Int(5)]).unwrap();
    assert_eq!(result, Value::Int(6));

    let result = externs["dec"](vec![Value::Int(5)]).unwrap();
    assert_eq!(result, Value::Int(4));
}

#[test]
fn provider_trailing_comma() {
    // Ensure trailing comma is accepted
    let externs = flat_mod::anvyx_externs();
    assert!(externs.contains_key("inc"));
}

// -- ExternDecl metadata tests --

#[test]
fn export_fn_generates_decl_const() {
    let decl: ExternDecl = __ANVYX_DECL_ADD();
    assert_eq!(decl.name, "add");
    assert_eq!(decl.params, &[("a", "int"), ("b", "int")]);
    assert_eq!(decl.ret, "int");
}

#[test]
fn export_fn_name_override_in_decl() {
    let decl: ExternDecl = __ANVYX_DECL_MY_FN();
    assert_eq!(decl.name, "custom_name");
    assert_eq!(decl.params, &[("x", "int")]);
    assert_eq!(decl.ret, "int");
}

#[test]
fn export_fn_string_param_decl() {
    let decl: ExternDecl = __ANVYX_DECL_GREET();
    assert_eq!(decl.name, "greet");
    assert_eq!(decl.params, &[("name", "string")]);
    assert_eq!(decl.ret, "string");
}

#[test]
fn export_fn_void_return_decl() {
    let decl: ExternDecl = __ANVYX_DECL_NOOP();
    assert_eq!(decl.name, "noop");
    assert_eq!(decl.params, &[]);
    assert_eq!(decl.ret, "void");
}

#[test]
fn export_fn_float_param_decl() {
    let decl: ExternDecl = __ANVYX_DECL_SCALE();
    assert_eq!(decl.name, "scale");
    assert_eq!(decl.params, &[("x", "float"), ("factor", "float")]);
    assert_eq!(decl.ret, "float");
}

#[test]
fn export_fn_bool_param_decl() {
    let decl: ExternDecl = __ANVYX_DECL_TOGGLE();
    assert_eq!(decl.name, "toggle");
    assert_eq!(decl.params, &[("flag", "bool")]);
    assert_eq!(decl.ret, "bool");
}

#[test]
fn provider_generates_anvyx_exports() {
    assert_eq!(anvyx_exports().len(), 3);
    let names: Vec<&str> = anvyx_exports().iter().map(|d| d.name).collect();
    assert!(names.contains(&"double"));
    assert!(names.contains(&"triple"));
    assert!(names.contains(&"hello"));
}

#[test]
fn provider_exports_name_override() {
    let exports = anvyx_exports();
    let triple = exports.iter().find(|d| d.name == "triple").unwrap();
    assert_eq!(triple.params, &[("x", "int")]);
    assert_eq!(triple.ret, "int");
}

#[test]
fn provider_exports_correct_types() {
    let exports = anvyx_exports();
    let double = exports.iter().find(|d| d.name == "double").unwrap();
    assert_eq!(double.params, &[("x", "int")]);
    assert_eq!(double.ret, "int");

    let hello = exports.iter().find(|d| d.name == "hello").unwrap();
    assert_eq!(hello.params, &[("name", "string")]);
    assert_eq!(hello.ret, "string");
}

#[test]
fn provider_bare_ident_exports() {
    assert_eq!(flat_mod::anvyx_exports().len(), 2);
    let names: Vec<&str> = flat_mod::anvyx_exports().iter().map(|d| d.name).collect();
    assert!(names.contains(&"inc"));
    assert!(names.contains(&"dec"));

    let flat_exports = flat_mod::anvyx_exports();
    let inc = flat_exports.iter().find(|d| d.name == "inc").unwrap();
    assert_eq!(inc.params, &[("x", "int")]);
    assert_eq!(inc.ret, "int");
}

// -- Value passthrough tests --

#[export_fn]
fn identity(v: Value) -> Value {
    v
}

#[test]
fn export_fn_value_passthrough() {
    let (name, handler) = __anvyx_export_identity();
    assert_eq!(name, "identity");
    let result = handler(vec![Value::Int(42)]).unwrap();
    assert_eq!(result, Value::Int(42));
    let result = handler(vec![Value::Bool(true)]).unwrap();
    assert_eq!(result, Value::Bool(true));
}

#[test]
fn export_fn_value_passthrough_decl() {
    let decl: ExternDecl = __ANVYX_DECL_IDENTITY();
    assert_eq!(decl.name, "identity");
    assert_eq!(decl.params, &[("v", "any")]);
    assert_eq!(decl.ret, "any");
}

// -- ret annotation tests --

#[export_fn(ret = "[K: V]")]
fn make_map() -> Value {
    Value::Nil
}

#[test]
fn export_fn_ret_annotation_decl() {
    let decl: ExternDecl = __ANVYX_DECL_MAKE_MAP();
    assert_eq!(decl.name, "make_map");
    assert_eq!(decl.params, &[]);
    assert_eq!(decl.ret, "[K: V]");
}

#[test]
fn export_fn_ret_annotation_handler() {
    let (_, handler) = __anvyx_export_make_map();
    let result = handler(vec![]).unwrap();
    assert_eq!(result, Value::Nil);
}

// -- params annotation tests --

#[export_fn(params(list = "[T]"), ret = "int")]
fn list_length(list: Value) -> i64 {
    match list {
        Value::List(l) => l.len() as i64,
        _ => 0,
    }
}

#[test]
fn export_fn_params_annotation_decl() {
    let decl: ExternDecl = __ANVYX_DECL_LIST_LENGTH();
    assert_eq!(decl.name, "list_length");
    assert_eq!(decl.params, &[("list", "[T]")]);
    assert_eq!(decl.ret, "int");
}

#[test]
fn export_fn_params_annotation_handler() {
    let (_, handler) = __anvyx_export_list_length();
    let list = Value::List(ManagedRc::new(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
    ]));
    let result = handler(vec![list]).unwrap();
    assert_eq!(result, Value::Int(3));
}

// -- combined name + ret annotations --

#[export_fn(name = "ordered", ret = "[K: V]")]
fn create_ordered_map() -> Value {
    Value::Nil
}

#[test]
fn export_fn_name_and_ret_combined() {
    let decl: ExternDecl = __ANVYX_DECL_CREATE_ORDERED_MAP();
    assert_eq!(decl.name, "ordered");
    assert_eq!(decl.ret, "[K: V]");
}

// -- mixed Value and primitive params --

#[export_fn(params(data = "[T]"))]
fn mixed_params(x: i64, data: Value) -> bool {
    x > 0 && matches!(data, Value::List(_))
}

#[test]
fn export_fn_mixed_value_and_primitive_decl() {
    let decl: ExternDecl = __ANVYX_DECL_MIXED_PARAMS();
    assert_eq!(decl.params, &[("x", "int"), ("data", "[T]")]);
    assert_eq!(decl.ret, "bool");
}

#[test]
fn export_fn_mixed_value_and_primitive_handler() {
    let (_, handler) = __anvyx_export_mixed_params();
    let list = Value::List(ManagedRc::new(vec![]));
    let result = handler(vec![Value::Int(1), list]).unwrap();
    assert_eq!(result, Value::Bool(true));
}

// -- #[export_type] tests --

#[derive(Clone)]
#[export_type(name = "Sprite")]
pub struct SpriteData {
    x: f32,
    y: f32,
}

#[test]
fn export_type_generates_decl_const() {
    let decl: ExternTypeDeclConst = __ANVYX_TYPE_DECL_SPRITEDATA();
    assert_eq!(decl.name, "Sprite");
}

#[test]
fn export_type_generates_store() {
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        let mut store = s.borrow_mut();
        let id = store.insert(SpriteData { x: 1.0, y: 2.0 });
        assert_eq!(store.borrow(id).unwrap().x, 1.0);
        store.remove(id).unwrap();
    });
}

#[derive(Clone)]
#[export_type(name = "Handle")]
pub struct OpaqueHandle;

#[test]
fn export_type_unit_struct() {
    let decl: ExternTypeDeclConst = __ANVYX_TYPE_DECL_OPAQUEHANDLE();
    assert_eq!(decl.name, "Handle");
}

#[derive(Clone)]
#[export_type(name = "Color")]
pub struct Color(pub u8, pub u8, pub u8);

#[test]
fn export_type_tuple_struct() {
    let decl: ExternTypeDeclConst = __ANVYX_TYPE_DECL_COLOR();
    assert_eq!(decl.name, "Color");
    __ANVYX_STORE_COLOR.with(|s| {
        let mut store = s.borrow_mut();
        let id = store.insert(Color(255, 0, 128));
        assert_eq!(store.borrow(id).unwrap().0, 255);
    });
}

// -- to_string_fn display tests --

#[derive(Clone)]
#[export_type(name = "FmtVec2")]
pub struct TestVec2 {
    pub x: f32,
    pub y: f32,
}

impl std::fmt::Display for TestVec2 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Vec2({}, {})", self.x as i64, self.y as i64)
    }
}

#[test]
fn export_type_with_display_to_string() {
    let id = TestVec2::with_store(|s| s.borrow_mut().insert(TestVec2 { x: 1.0, y: 2.0 }));
    let result = TestVec2::to_display(id);
    assert_eq!(result, "Vec2(1, 2)");
    TestVec2::cleanup(id);
}

#[test]
fn export_type_without_display_to_string() {
    let id = OpaqueHandle::with_store(|s| s.borrow_mut().insert(OpaqueHandle));
    let result = OpaqueHandle::to_display(id);
    assert_eq!(result, "<Handle>");
    OpaqueHandle::cleanup(id);
}

#[test]
fn from_anvyx_does_not_remove_from_store() {
    use anvyx_lang::AnvyxConvert as _;
    let value = SpriteData { x: 1.0, y: 2.0 }.into_anvyx();

    let a = SpriteData::from_anvyx(&value).unwrap();
    assert_eq!(a.x, 1.0);
    assert_eq!(a.y, 2.0);

    // second call must succeed — value was NOT removed
    let b = SpriteData::from_anvyx(&value).unwrap();
    assert_eq!(b.x, 1.0);
    assert_eq!(b.y, 2.0);
}

// -- provider! with types: tests --

mod typed_provider {
    use anvyx_lang::{export_fn, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Widget")]
    pub struct Widget {
        pub val: i64,
    }

    #[export_methods]
    impl Widget {}

    #[export_fn]
    pub fn make_val(x: i64) -> i64 {
        x + 1
    }

    anvyx_lang::provider!(types: [Widget], make_val);
}

#[test]
fn provider_types_populates_type_exports() {
    let types = typed_provider::anvyx_type_exports();
    assert_eq!(types.len(), 1);
    assert_eq!(types[0].name, "Widget");
}

#[test]
fn provider_types_still_has_fn_exports() {
    assert_eq!(typed_provider::anvyx_exports().len(), 1);
    assert_eq!(typed_provider::anvyx_exports()[0].name, "make_val");
}

#[test]
fn provider_types_handlers_work() {
    let externs = typed_provider::anvyx_externs();
    assert_eq!(externs.len(), 1);
    let result = externs["make_val"](vec![Value::Int(5)]).unwrap();
    assert_eq!(result, Value::Int(6));
}

mod types_only {
    use anvyx_lang::{export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Node")]
    pub struct Node {
        pub id: i64,
    }

    #[export_methods]
    impl Node {}

    anvyx_lang::provider!(types: [Node]);
}

#[test]
fn provider_types_only() {
    let types = types_only::anvyx_type_exports();
    assert_eq!(types.len(), 1);
    assert_eq!(types[0].name, "Node");
    assert!(types_only::anvyx_exports().is_empty());
    assert!(types_only::anvyx_externs().is_empty());
}

mod qualified_type {
    pub mod inner {
        use anvyx_lang::{export_methods, export_type};

        #[derive(Clone)]
        #[export_type(name = "Inner")]
        pub struct InnerType {
            pub data: i64,
        }

        #[export_methods]
        impl InnerType {}
    }

    use anvyx_lang::export_fn;

    #[export_fn]
    pub fn get_data() -> i64 {
        42
    }

    anvyx_lang::provider!(types: [inner::InnerType], get_data);
}

#[test]
fn provider_module_qualified_type() {
    let types = qualified_type::anvyx_type_exports();
    assert_eq!(types.len(), 1);
    assert_eq!(types[0].name, "Inner");
}

mod multi_types {
    use anvyx_lang::{export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Texture")]
    pub struct Tex {
        pub w: i64,
    }

    #[export_methods]
    impl Tex {}

    #[derive(Clone)]
    #[export_type(name = "Shader")]
    pub struct Shd {
        pub id: i64,
    }

    #[export_methods]
    impl Shd {}

    anvyx_lang::provider!(types: [Tex, Shd]);
}

#[test]
fn provider_multiple_types() {
    let types = multi_types::anvyx_type_exports();
    assert_eq!(types.len(), 2);
    let names: Vec<&str> = types.iter().map(|d| d.name).collect();
    assert!(names.contains(&"Texture"));
    assert!(names.contains(&"Shader"));
}

#[test]
fn provider_no_types_backward_compat() {
    assert!(anvyx_type_exports().is_empty());
}

// -- #[export_fn] with extern types --

#[export_fn]
pub fn create_sprite(x: f32, y: f32) -> SpriteData {
    SpriteData { x, y }
}

#[export_fn]
pub fn sprite_x(s: &SpriteData) -> f32 {
    s.x
}

#[export_fn]
pub fn set_sprite_x(s: &mut SpriteData, x: f32) {
    s.x = x;
}

#[export_fn]
pub fn destroy_sprite(s: SpriteData) {
    let _ = s;
}

#[export_fn]
pub fn move_sprite(s: &mut SpriteData, dx: f32, dy: f32) {
    s.x += dx;
    s.y += dy;
}

// Step 5: extern type return

#[test]
fn export_fn_extern_type_return() {
    let (_, handler) = __anvyx_export_create_sprite();
    let result = handler(vec![Value::Float(1.0), Value::Float(2.0)]).unwrap();
    let Value::ExternHandle(id) = result else {
        panic!("expected ExternHandle");
    };
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        let store = s.borrow();
        let sprite = store.borrow(id.id).unwrap();
        assert_eq!(sprite.x, 1.0);
        assert_eq!(sprite.y, 2.0);
    });
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        s.borrow_mut().remove(id.id).unwrap();
    });
}

#[test]
fn export_fn_extern_type_return_decl() {
    let decl: ExternDecl = __ANVYX_DECL_CREATE_SPRITE();
    assert_eq!(decl.name, "create_sprite");
    assert_eq!(decl.params, &[("x", "float"), ("y", "float")]);
    assert_eq!(decl.ret, "Sprite");
}

// Step 6: extern ref param (&T)

#[test]
fn export_fn_extern_ref_param() {
    let id =
        __ANVYX_STORE_SPRITEDATA.with(|s| s.borrow_mut().insert(SpriteData { x: 5.0, y: 10.0 }));
    let (_, handler) = __anvyx_export_sprite_x();
    let result = handler(vec![extern_handle(id)]).unwrap();
    assert_eq!(result, Value::Float(5.0));
    // Sprite still in store (not consumed)
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        assert!(s.borrow().borrow(id).is_ok());
    });
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        s.borrow_mut().remove(id).unwrap();
    });
}

#[test]
fn export_fn_extern_ref_param_decl() {
    let decl: ExternDecl = __ANVYX_DECL_SPRITE_X();
    assert_eq!(decl.name, "sprite_x");
    assert_eq!(decl.params, &[("s", "Sprite")]);
    assert_eq!(decl.ret, "float");
}

// Step 7: extern mutable ref param (&mut T)

#[test]
fn export_fn_extern_mut_ref_param() {
    let id =
        __ANVYX_STORE_SPRITEDATA.with(|s| s.borrow_mut().insert(SpriteData { x: 1.0, y: 2.0 }));
    let (_, handler) = __anvyx_export_set_sprite_x();
    handler(vec![extern_handle(id), Value::Float(99.0)]).unwrap();
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        let store = s.borrow();
        let sprite = store.borrow(id).unwrap();
        assert_eq!(sprite.x, 99.0);
        assert_eq!(sprite.y, 2.0);
    });
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        s.borrow_mut().remove(id).unwrap();
    });
}

#[test]
fn export_fn_extern_mut_ref_param_decl() {
    let decl: ExternDecl = __ANVYX_DECL_SET_SPRITE_X();
    assert_eq!(decl.name, "set_sprite_x");
    assert_eq!(decl.params, &[("s", "Sprite"), ("x", "float")]);
    assert_eq!(decl.ret, "void");
}

// Step 8: extern owned param (consuming T)

#[test]
fn export_fn_extern_owned_param() {
    let id =
        __ANVYX_STORE_SPRITEDATA.with(|s| s.borrow_mut().insert(SpriteData { x: 1.0, y: 2.0 }));
    let (_, handler) = __anvyx_export_destroy_sprite();
    handler(vec![extern_handle(id)]).unwrap();
    // from_anvyx now borrows+clones; value is NOT removed from store
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        assert!(s.borrow().borrow(id).is_ok());
        s.borrow_mut().remove(id).unwrap();
    });
}

#[test]
fn export_fn_extern_owned_param_decl() {
    let decl: ExternDecl = __ANVYX_DECL_DESTROY_SPRITE();
    assert_eq!(decl.name, "destroy_sprite");
    assert_eq!(decl.params, &[("s", "Sprite")]);
    assert_eq!(decl.ret, "void");
}

// Step 9: mixed + error + round-trip

#[test]
fn export_fn_mixed_extern_and_primitive() {
    let id =
        __ANVYX_STORE_SPRITEDATA.with(|s| s.borrow_mut().insert(SpriteData { x: 1.0, y: 2.0 }));
    let (_, handler) = __anvyx_export_move_sprite();
    handler(vec![
        extern_handle(id),
        Value::Float(10.0),
        Value::Float(20.0),
    ])
    .unwrap();
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        let store = s.borrow();
        let sprite = store.borrow(id).unwrap();
        assert_eq!(sprite.x, 11.0);
        assert_eq!(sprite.y, 22.0);
    });
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        s.borrow_mut().remove(id).unwrap();
    });
}

#[test]
fn export_fn_mixed_extern_and_primitive_decl() {
    let decl: ExternDecl = __ANVYX_DECL_MOVE_SPRITE();
    assert_eq!(decl.name, "move_sprite");
    assert_eq!(
        decl.params,
        &[("s", "Sprite"), ("dx", "float"), ("dy", "float")]
    );
    assert_eq!(decl.ret, "void");
}

#[test]
fn export_fn_extern_ref_invalid_handle() {
    let (_, handler) = __anvyx_export_sprite_x();
    let result = handler(vec![extern_handle(99999)]);
    assert!(result.is_err());
}

#[test]
fn export_fn_extern_ref_wrong_type() {
    let (_, handler) = __anvyx_export_sprite_x();
    let result = handler(vec![Value::Int(42)]);
    assert!(result.is_err());
}

#[test]
fn export_fn_extern_type_full_round_trip() {
    let (_, create) = __anvyx_export_create_sprite();
    let (_, get_x) = __anvyx_export_sprite_x();
    let (_, set_x) = __anvyx_export_set_sprite_x();

    // Create
    let result = create(vec![Value::Float(10.0), Value::Float(20.0)]).unwrap();
    let Value::ExternHandle(id) = result else {
        panic!("expected ExternHandle");
    };
    let raw_id = id.id;

    // Read
    let x = get_x(vec![Value::ExternHandle(id.clone())]).unwrap();
    assert_eq!(x, Value::Float(10.0));

    // Mutate
    set_x(vec![Value::ExternHandle(id.clone()), Value::Float(99.0)]).unwrap();

    // Read again
    let x = get_x(vec![Value::ExternHandle(id.clone())]).unwrap();
    assert_eq!(x, Value::Float(99.0));

    // Drop last reference — triggers cleanup
    drop(id);
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        assert!(s.borrow().borrow(raw_id).is_err());
    });
}

// Step 10: provider integration with extern types

mod extern_type_provider {
    use anvyx_lang::{export_fn, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Widget")]
    pub struct Widget {
        pub val: i64,
    }

    #[export_methods]
    impl Widget {}

    #[export_fn]
    pub fn create_widget(val: i64) -> Widget {
        Widget { val }
    }

    #[export_fn]
    pub fn widget_val(w: &Widget) -> i64 {
        w.val
    }

    #[export_fn]
    pub fn destroy_widget(w: Widget) {
        let _ = w;
    }

    anvyx_lang::provider!(types: [Widget], create_widget, widget_val, destroy_widget);
}

#[test]
fn provider_extern_type_decls() {
    let types = extern_type_provider::anvyx_type_exports();
    assert_eq!(types.len(), 1);
    assert_eq!(types[0].name, "Widget");

    let ext_exports = extern_type_provider::anvyx_exports();
    assert_eq!(ext_exports.len(), 3);

    let create = ext_exports
        .iter()
        .find(|d| d.name == "create_widget")
        .unwrap();
    assert_eq!(create.params, &[("val", "int")]);
    assert_eq!(create.ret, "Widget");

    let get_val = ext_exports.iter().find(|d| d.name == "widget_val").unwrap();
    assert_eq!(get_val.params, &[("w", "Widget")]);
    assert_eq!(get_val.ret, "int");

    let destroy = ext_exports
        .iter()
        .find(|d| d.name == "destroy_widget")
        .unwrap();
    assert_eq!(destroy.params, &[("w", "Widget")]);
    assert_eq!(destroy.ret, "void");
}

#[test]
fn provider_extern_type_handlers_work() {
    let externs = extern_type_provider::anvyx_externs();

    // Create
    let result = externs["create_widget"](vec![Value::Int(42)]).unwrap();
    let Value::ExternHandle(id) = result else {
        panic!("expected ExternHandle");
    };

    // Read
    let val = externs["widget_val"](vec![Value::ExternHandle(id.clone())]).unwrap();
    assert_eq!(val, Value::Int(42));

    // Drop last reference — triggers cleanup
    drop(id);
}

// -- same-store multi-borrow tests --

#[export_fn]
pub fn distance_sprites(a: &SpriteData, b: &SpriteData) -> f32 {
    ((a.x - b.x).powi(2) + (a.y - b.y).powi(2)).sqrt()
}

#[export_fn]
pub fn move_sprite_towards(s: &mut SpriteData, target: &SpriteData) {
    s.x = target.x;
    s.y = target.y;
}

#[test]
fn export_fn_same_store_two_immutable_refs() {
    let id_a =
        __ANVYX_STORE_SPRITEDATA.with(|s| s.borrow_mut().insert(SpriteData { x: 0.0, y: 0.0 }));
    let id_b =
        __ANVYX_STORE_SPRITEDATA.with(|s| s.borrow_mut().insert(SpriteData { x: 3.0, y: 4.0 }));
    let (_, handler) = __anvyx_export_distance_sprites();
    let result = handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
    assert_eq!(result, Value::Float(5.0));
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        s.borrow_mut().remove(id_a).unwrap();
        s.borrow_mut().remove(id_b).unwrap();
    });
}

#[test]
fn export_fn_same_store_mut_and_immut_refs() {
    let id_a =
        __ANVYX_STORE_SPRITEDATA.with(|s| s.borrow_mut().insert(SpriteData { x: 0.0, y: 0.0 }));
    let id_b =
        __ANVYX_STORE_SPRITEDATA.with(|s| s.borrow_mut().insert(SpriteData { x: 10.0, y: 20.0 }));
    let (_, handler) = __anvyx_export_move_sprite_towards();
    handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        let store = s.borrow();
        let sprite = store.borrow(id_a).unwrap();
        assert_eq!(sprite.x, 10.0);
        assert_eq!(sprite.y, 20.0);
    });
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        s.borrow_mut().remove(id_a).unwrap();
        s.borrow_mut().remove(id_b).unwrap();
    });
}

#[test]
fn export_fn_same_store_same_handle_mut_fails() {
    let id =
        __ANVYX_STORE_SPRITEDATA.with(|s| s.borrow_mut().insert(SpriteData { x: 1.0, y: 2.0 }));
    let (_, handler) = __anvyx_export_move_sprite_towards();
    let result = handler(vec![extern_handle(id), extern_handle(id)]);
    assert!(result.is_err());
    __ANVYX_STORE_SPRITEDATA.with(|s| {
        s.borrow_mut().remove(id).unwrap();
    });
}

// -- cleanup integration tests --

#[test]
fn cleanup_loop_no_leak() {
    let (_, create) = __anvyx_export_create_sprite();
    for i in 0..100 {
        let _ = create(vec![Value::Float(i as f32), Value::Float(0.0_f32)]).unwrap();
    }
    __ANVYX_STORE_SPRITEDATA.with(|s| assert_eq!(s.borrow().len(), 0));
}

#[test]
fn cleanup_explicit_destroy_then_drop_no_panic() {
    let (_, create) = __anvyx_export_create_sprite();
    let (_, destroy) = __anvyx_export_destroy_sprite();

    let result = create(vec![Value::Float(1.0), Value::Float(2.0)]).unwrap();
    let Value::ExternHandle(handle) = result else {
        panic!("expected ExternHandle")
    };
    let lingering = handle.clone();

    destroy(vec![Value::ExternHandle(handle)]).unwrap();
    // from_anvyx borrows+clones; handle dropped from vec but lingering still alive
    __ANVYX_STORE_SPRITEDATA.with(|s| assert_eq!(s.borrow().len(), 1));

    drop(lingering);
    // last reference dropped — cleanup fires exactly once, no panic
    __ANVYX_STORE_SPRITEDATA.with(|s| assert_eq!(s.borrow().len(), 0));
}

// -- #[export_methods] tests --

mod method_tests {
    use anvyx_lang::{ExternHandle, Value, export_methods, export_type};

    use super::extern_handle;

    #[derive(Clone)]
    #[export_type(name = "Vec2")]
    pub struct Vec2 {
        pub x: f32,
        pub y: f32,
    }

    #[derive(Clone)]
    #[export_type(name = "Color")]
    pub struct Color {
        pub r: f32,
        pub g: f32,
        pub b: f32,
    }

    #[export_methods]
    impl Vec2 {
        pub fn new(x: f32, y: f32) -> Vec2 {
            Vec2 { x, y }
        }
        pub fn zero() -> Vec2 {
            Vec2 { x: 0.0, y: 0.0 }
        }
        pub fn get_x(&self) -> f32 {
            self.x
        }
        pub fn get_y(&self) -> f32 {
            self.y
        }
        pub fn length(&self) -> f32 {
            (self.x * self.x + self.y * self.y).sqrt()
        }
        pub fn move_by(&mut self, dx: f32, dy: f32) {
            self.x += dx;
            self.y += dy;
        }
        pub fn set_x(&mut self, x: f32) {
            self.x = x;
        }
        pub fn scaled(&self, factor: f32) -> Vec2 {
            Vec2 {
                x: self.x * factor,
                y: self.y * factor,
            }
        }
        pub fn negated(&self) -> Vec2 {
            Vec2 {
                x: -self.x,
                y: -self.y,
            }
        }
        pub fn dot(&self, other: &Vec2) -> f32 {
            self.x * other.x + self.y * other.y
        }
        pub fn add_assign(&mut self, other: &Vec2) {
            self.x += other.x;
            self.y += other.y;
        }
        pub fn color_brightness(&self, c: &Color) -> f32 {
            self.length() * (c.r + c.g + c.b) / 3.0
        }
        pub fn tinted_scale(&self, c: &Color) -> Vec2 {
            let factor = (c.r + c.g + c.b) / 3.0;
            Vec2 {
                x: self.x * factor,
                y: self.y * factor,
            }
        }
        pub fn apply_color(&self, c: Color) -> f32 {
            self.x * c.r + self.y * c.g
        }
        pub fn with_value(&self, v: Value) -> Value {
            match v {
                Value::Float(f) => Value::Float(f + self.x),
                other => other,
            }
        }
        pub fn from_color(c: &Color) -> Vec2 {
            Vec2 { x: c.r, y: c.g }
        }
        pub fn from_color_owned(c: Color) -> Vec2 {
            Vec2 { x: c.r, y: c.g }
        }
        pub fn to_handle(&self) -> ExternHandle<Self> {
            ExternHandle::new(Vec2 {
                x: self.x,
                y: self.y,
            })
        }
    }

    #[test]
    fn export_methods_static_handler_key() {
        let (key, _) = __anvyx_method_Vec2_new();
        assert_eq!(key, "Vec2::new");
    }

    #[test]
    fn export_methods_static_handler_creates_handle() {
        let (_, handler) = __anvyx_method_Vec2_new();
        let result = handler(vec![Value::Float(3.0), Value::Float(4.0)]).unwrap();
        let Value::ExternHandle(id) = result else {
            panic!("expected ExternHandle")
        };
        __ANVYX_STORE_VEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(id.id).unwrap();
            assert_eq!(v.x, 3.0);
            assert_eq!(v.y, 4.0);
        });
        __ANVYX_STORE_VEC2.with(|s| {
            s.borrow_mut().remove(id.id).unwrap();
        });
    }

    #[test]
    fn export_methods_static_no_params() {
        let (_, handler) = __anvyx_method_Vec2_zero();
        let result = handler(vec![]).unwrap();
        let Value::ExternHandle(id) = result else {
            panic!("expected ExternHandle")
        };
        __ANVYX_STORE_VEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(id.id).unwrap();
            assert_eq!(v.x, 0.0);
            assert_eq!(v.y, 0.0);
        });
        __ANVYX_STORE_VEC2.with(|s| {
            s.borrow_mut().remove(id.id).unwrap();
        });
    }

    #[test]
    fn export_methods_borrow_handler_primitive() {
        let id = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 7.0, y: 8.0 }));
        let (_, handler) = __anvyx_method_Vec2_get_x();
        let result = handler(vec![extern_handle(id)]).unwrap();
        assert_eq!(result, Value::Float(7.0));
        __ANVYX_STORE_VEC2.with(|s| {
            s.borrow_mut().remove(id).unwrap();
        });
    }

    #[test]
    fn export_methods_borrow_handler_length() {
        let id = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 3.0, y: 4.0 }));
        let (_, handler) = __anvyx_method_Vec2_length();
        let result = handler(vec![extern_handle(id)]).unwrap();
        assert_eq!(result, Value::Float(5.0));
        __ANVYX_STORE_VEC2.with(|s| {
            s.borrow_mut().remove(id).unwrap();
        });
    }

    #[test]
    fn export_methods_mut_handler() {
        let id = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 1.0, y: 2.0 }));
        let (_, handler) = __anvyx_method_Vec2_move_by();
        handler(vec![
            extern_handle(id),
            Value::Float(10.0),
            Value::Float(20.0),
        ])
        .unwrap();
        __ANVYX_STORE_VEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(id).unwrap();
            assert_eq!(v.x, 11.0);
            assert_eq!(v.y, 22.0);
        });
        __ANVYX_STORE_VEC2.with(|s| {
            s.borrow_mut().remove(id).unwrap();
        });
    }

    #[test]
    fn export_methods_borrow_returns_self() {
        let id = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 2.0, y: 3.0 }));
        let (_, handler) = __anvyx_method_Vec2_scaled();
        let result = handler(vec![extern_handle(id), Value::Float(2.0)]).unwrap();
        let Value::ExternHandle(new_id) = result else {
            panic!("expected ExternHandle")
        };
        assert_ne!(id, new_id.id);
        __ANVYX_STORE_VEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(new_id.id).unwrap();
            assert_eq!(v.x, 4.0);
            assert_eq!(v.y, 6.0);
        });
        __ANVYX_STORE_VEC2.with(|s| {
            s.borrow_mut().remove(id).unwrap();
            s.borrow_mut().remove(new_id.id).unwrap();
        });
    }

    #[test]
    fn export_methods_borrow_returns_self_no_params() {
        let id = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 5.0, y: 6.0 }));
        let (_, handler) = __anvyx_method_Vec2_negated();
        let result = handler(vec![extern_handle(id)]).unwrap();
        let Value::ExternHandle(new_id) = result else {
            panic!("expected ExternHandle")
        };
        __ANVYX_STORE_VEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(new_id.id).unwrap();
            assert_eq!(v.x, -5.0);
            assert_eq!(v.y, -6.0);
        });
        __ANVYX_STORE_VEC2.with(|s| {
            s.borrow_mut().remove(id).unwrap();
            s.borrow_mut().remove(new_id.id).unwrap();
        });
    }

    #[test]
    fn export_methods_invalid_handle() {
        let (_, handler) = __anvyx_method_Vec2_get_x();
        let result = handler(vec![extern_handle(99999)]);
        assert!(result.is_err());
    }

    #[test]
    fn export_methods_wrong_type_for_self() {
        let (_, handler) = __anvyx_method_Vec2_get_x();
        let result = handler(vec![Value::Int(42)]);
        assert!(result.is_err());
    }

    #[test]
    fn export_methods_method_metadata() {
        let methods = __ANVYX_METHODS_DECL_VEC2();
        let names: Vec<&str> = methods.iter().map(|m| m.name).collect();
        assert!(names.contains(&"get_x"));
        assert!(names.contains(&"get_y"));
        assert!(names.contains(&"length"));
        assert!(names.contains(&"move_by"));
        assert!(names.contains(&"set_x"));
        assert!(names.contains(&"scaled"));
        assert!(names.contains(&"negated"));

        let get_x = methods.iter().find(|m| m.name == "get_x").unwrap();
        assert_eq!(get_x.receiver, "self");
        assert_eq!(get_x.params, &[]);
        assert_eq!(get_x.ret, "float");

        let move_by = methods.iter().find(|m| m.name == "move_by").unwrap();
        assert_eq!(move_by.receiver, "var");
        assert_eq!(move_by.params, &[("dx", "float"), ("dy", "float")]);
        assert_eq!(move_by.ret, "void");

        let scaled = methods.iter().find(|m| m.name == "scaled").unwrap();
        assert_eq!(scaled.receiver, "self");
        assert_eq!(scaled.params, &[("factor", "float")]);
        assert_eq!(scaled.ret, "Vec2");
    }

    #[test]
    fn export_methods_static_metadata() {
        let statics = __ANVYX_STATICS_DECL_VEC2();
        let names: Vec<&str> = statics.iter().map(|s| s.name).collect();
        assert!(names.contains(&"new"));
        assert!(names.contains(&"zero"));

        let new_m = statics.iter().find(|s| s.name == "new").unwrap();
        assert_eq!(new_m.params, &[("x", "float"), ("y", "float")]);
        assert_eq!(new_m.ret, "Vec2");

        let zero_m = statics.iter().find(|s| s.name == "zero").unwrap();
        assert_eq!(zero_m.params, &[]);
        assert_eq!(zero_m.ret, "Vec2");
    }

    #[test]
    fn export_methods_convenience_function() {
        let handlers = __anvyx_methods_vec2();
        let keys: Vec<&str> = handlers.iter().map(|(k, _)| *k).collect();
        assert!(keys.contains(&"Vec2::new"));
        assert!(keys.contains(&"Vec2::zero"));
        assert!(keys.contains(&"Vec2::get_x"));
        assert!(keys.contains(&"Vec2::get_y"));
        assert!(keys.contains(&"Vec2::length"));
        assert!(keys.contains(&"Vec2::move_by"));
        assert!(keys.contains(&"Vec2::set_x"));
        assert!(keys.contains(&"Vec2::scaled"));
        assert!(keys.contains(&"Vec2::negated"));
    }

    #[test]
    fn export_methods_full_round_trip() {
        let handler_map: std::collections::HashMap<&str, _> =
            __anvyx_methods_vec2().into_iter().collect();

        let result = handler_map["Vec2::new"](vec![Value::Float(1.0), Value::Float(2.0)]).unwrap();
        let Value::ExternHandle(id) = result else {
            panic!("expected ExternHandle")
        };

        let x = handler_map["Vec2::get_x"](vec![Value::ExternHandle(id.clone())]).unwrap();
        assert_eq!(x, Value::Float(1.0));

        handler_map["Vec2::set_x"](vec![Value::ExternHandle(id.clone()), Value::Float(99.0)])
            .unwrap();

        let x = handler_map["Vec2::get_x"](vec![Value::ExternHandle(id.clone())]).unwrap();
        assert_eq!(x, Value::Float(99.0));

        __ANVYX_STORE_VEC2.with(|s| {
            s.borrow_mut().remove(id.id).unwrap();
        });
    }

    #[test]
    fn export_methods_same_store_borrow() {
        let id_a = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 1.0, y: 2.0 }));
        let id_b = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 3.0, y: 4.0 }));
        let (_, handler) = __anvyx_method_Vec2_dot();
        let result = handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
        assert_eq!(result, Value::Float(1.0 * 3.0 + 2.0 * 4.0));
        __ANVYX_STORE_VEC2.with(|s| {
            assert!(s.borrow().borrow(id_a).is_ok());
            assert!(s.borrow().borrow(id_b).is_ok());
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
        });
    }

    #[test]
    fn export_methods_same_store_mut_and_ref() {
        let id_a = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 1.0, y: 2.0 }));
        let id_b = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 10.0, y: 20.0 }));
        let (_, handler) = __anvyx_method_Vec2_add_assign();
        handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
        __ANVYX_STORE_VEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(id_a).unwrap();
            assert_eq!(v.x, 11.0);
            assert_eq!(v.y, 22.0);
        });
        __ANVYX_STORE_VEC2.with(|s| {
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
        });
    }

    #[test]
    fn export_methods_same_store_same_handle_fails() {
        let id = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 1.0, y: 2.0 }));
        let (_, handler) = __anvyx_method_Vec2_add_assign();
        let result = handler(vec![extern_handle(id), extern_handle(id)]);
        assert!(result.is_err());
        __ANVYX_STORE_VEC2.with(|s| {
            assert!(s.borrow().borrow(id).is_ok());
            s.borrow_mut().remove(id).unwrap();
        });
    }

    #[test]
    fn export_methods_cross_store_borrow() {
        let id_v = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 3.0, y: 4.0 }));
        let id_c = __ANVYX_STORE_COLOR.with(|s| {
            s.borrow_mut().insert(Color {
                r: 0.5,
                g: 0.5,
                b: 0.5,
            })
        });
        let (_, handler) = __anvyx_method_Vec2_color_brightness();
        let result = handler(vec![extern_handle(id_v), extern_handle(id_c)]).unwrap();
        // length(3,4) = 5.0, (0.5+0.5+0.5)/3 = 0.5, product = 2.5
        assert_eq!(result, Value::Float(2.5));
        __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().remove(id_v).unwrap());
        __ANVYX_STORE_COLOR.with(|s| s.borrow_mut().remove(id_c).unwrap());
    }

    #[test]
    fn export_methods_cross_store_returns_self() {
        let id_v = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 2.0, y: 3.0 }));
        let id_c = __ANVYX_STORE_COLOR.with(|s| {
            s.borrow_mut().insert(Color {
                r: 1.0,
                g: 1.0,
                b: 1.0,
            })
        });
        let (_, handler) = __anvyx_method_Vec2_tinted_scale();
        let result = handler(vec![extern_handle(id_v), extern_handle(id_c)]).unwrap();
        let Value::ExternHandle(new_id) = result else {
            panic!("expected ExternHandle");
        };
        assert_ne!(id_v, new_id.id);
        __ANVYX_STORE_VEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(new_id.id).unwrap();
            assert_eq!(v.x, 2.0);
            assert_eq!(v.y, 3.0);
        });
        __ANVYX_STORE_VEC2.with(|s| {
            s.borrow_mut().remove(id_v).unwrap();
            s.borrow_mut().remove(new_id.id).unwrap();
        });
        __ANVYX_STORE_COLOR.with(|s| s.borrow_mut().remove(id_c).unwrap());
    }

    #[test]
    fn export_methods_owned_param() {
        let id_v = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 2.0, y: 3.0 }));
        let id_c = __ANVYX_STORE_COLOR.with(|s| {
            s.borrow_mut().insert(Color {
                r: 0.5,
                g: 1.0,
                b: 0.0,
            })
        });
        let (_, handler) = __anvyx_method_Vec2_apply_color();
        let result = handler(vec![extern_handle(id_v), extern_handle(id_c)]).unwrap();
        // 2.0*0.5 + 3.0*1.0 = 4.0
        assert_eq!(result, Value::Float(4.0));
        // from_anvyx borrows+clones; Color stays in store
        __ANVYX_STORE_COLOR.with(|s| {
            assert!(s.borrow().borrow(id_c).is_ok());
            s.borrow_mut().remove(id_c).unwrap();
        });
        __ANVYX_STORE_VEC2.with(|s| {
            assert!(s.borrow().borrow(id_v).is_ok());
            s.borrow_mut().remove(id_v).unwrap();
        });
    }

    #[test]
    fn export_methods_value_passthrough() {
        let id = __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().insert(Vec2 { x: 10.0, y: 0.0 }));
        let (_, handler) = __anvyx_method_Vec2_with_value();
        let result = handler(vec![extern_handle(id), Value::Float(5.0)]).unwrap();
        assert_eq!(result, Value::Float(15.0));
        __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().remove(id).unwrap());
    }

    #[test]
    fn export_methods_static_with_extern_ref() {
        let id_c = __ANVYX_STORE_COLOR.with(|s| {
            s.borrow_mut().insert(Color {
                r: 3.0,
                g: 4.0,
                b: 5.0,
            })
        });
        let (_, handler) = __anvyx_method_Vec2_from_color();
        let result = handler(vec![extern_handle(id_c)]).unwrap();
        let Value::ExternHandle(new_id) = result else {
            panic!("expected ExternHandle");
        };
        __ANVYX_STORE_VEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(new_id.id).unwrap();
            assert_eq!(v.x, 3.0);
            assert_eq!(v.y, 4.0);
        });
        __ANVYX_STORE_COLOR.with(|s| assert!(s.borrow().borrow(id_c).is_ok()));
        __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().remove(new_id.id).unwrap());
        __ANVYX_STORE_COLOR.with(|s| s.borrow_mut().remove(id_c).unwrap());
    }

    #[test]
    fn export_methods_static_with_owned() {
        let id_c = __ANVYX_STORE_COLOR.with(|s| {
            s.borrow_mut().insert(Color {
                r: 7.0,
                g: 8.0,
                b: 9.0,
            })
        });
        let (_, handler) = __anvyx_method_Vec2_from_color_owned();
        let result = handler(vec![extern_handle(id_c)]).unwrap();
        let Value::ExternHandle(new_id) = result else {
            panic!("expected ExternHandle");
        };
        __ANVYX_STORE_VEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(new_id.id).unwrap();
            assert_eq!(v.x, 7.0);
            assert_eq!(v.y, 8.0);
        });
        // from_anvyx borrows+clones; Color stays in store
        __ANVYX_STORE_COLOR.with(|s| {
            assert!(s.borrow().borrow(id_c).is_ok());
            s.borrow_mut().remove(id_c).unwrap();
        });
        __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().remove(new_id.id).unwrap());
    }

    #[test]
    fn export_methods_metadata_cross_type() {
        let methods = __ANVYX_METHODS_DECL_VEC2();

        let dot = methods.iter().find(|m| m.name == "dot").unwrap();
        assert_eq!(dot.params, &[("other", "Vec2")]);
        assert_eq!(dot.ret, "float");

        let cb = methods
            .iter()
            .find(|m| m.name == "color_brightness")
            .unwrap();
        assert_eq!(cb.params, &[("c", "Color")]);
        assert_eq!(cb.ret, "float");

        let ac = methods.iter().find(|m| m.name == "apply_color").unwrap();
        assert_eq!(ac.params, &[("c", "Color")]);

        let wv = methods.iter().find(|m| m.name == "with_value").unwrap();
        assert_eq!(wv.params, &[("v", "any")]);

        let statics = __ANVYX_STATICS_DECL_VEC2();

        let fc = statics.iter().find(|s| s.name == "from_color").unwrap();
        assert_eq!(fc.params, &[("c", "Color")]);
        assert_eq!(fc.ret, "Vec2");
    }

    #[test]
    fn export_methods_extern_handle_self_return() {
        let methods = __ANVYX_METHODS_DECL_VEC2();
        let th = methods.iter().find(|m| m.name == "to_handle").unwrap();
        assert_eq!(th.params, &[]);
        assert_eq!(th.ret, "Vec2");
    }
}

// -- provider! + #[export_methods] integration tests --

mod methods_provider {
    use anvyx_lang::{Value, export_fn, export_methods, export_type, exports_to_json};

    #[derive(Clone)]
    #[export_type(name = "Vec2")]
    pub struct Vec2 {
        pub x: f32,
        pub y: f32,
    }

    #[export_methods]
    impl Vec2 {
        pub fn new(x: f32, y: f32) -> Vec2 {
            Vec2 { x, y }
        }
        pub fn x(&self) -> f32 {
            self.x
        }
        pub fn set_x(&mut self, x: f32) {
            self.x = x;
        }
    }

    #[export_fn]
    pub fn add_floats(a: f32, b: f32) -> f32 {
        a + b
    }

    anvyx_lang::provider!(types: [Vec2], add_floats);

    #[test]
    fn provider_methods_in_externs() {
        let externs = anvyx_externs();
        assert_eq!(externs.len(), 4);
        assert!(externs.contains_key("Vec2::new"));
        assert!(externs.contains_key("Vec2::x"));
        assert!(externs.contains_key("Vec2::set_x"));
        assert!(externs.contains_key("add_floats"));
    }

    #[test]
    fn provider_method_handlers_work() {
        let externs = anvyx_externs();
        let result = externs["Vec2::new"](vec![Value::Float(3.0), Value::Float(4.0)]).unwrap();
        let Value::ExternHandle(id) = result else {
            panic!("expected ExternHandle")
        };
        let x = externs["Vec2::x"](vec![Value::ExternHandle(id.clone())]).unwrap();
        assert_eq!(x, Value::Float(3.0));
        externs["Vec2::set_x"](vec![Value::ExternHandle(id.clone()), Value::Float(99.0)]).unwrap();
        let x = externs["Vec2::x"](vec![Value::ExternHandle(id.clone())]).unwrap();
        assert_eq!(x, Value::Float(99.0));
        __ANVYX_STORE_VEC2.with(|s| s.borrow_mut().remove(id.id).unwrap());
    }

    #[test]
    fn provider_type_exports_have_methods() {
        let types = anvyx_type_exports();
        assert_eq!(types.len(), 1);
        let ty = &types[0];
        assert_eq!(ty.name, "Vec2");
        assert_eq!(ty.methods.len(), 2);
        assert_eq!(ty.statics.len(), 1);

        let x_method = ty.methods.iter().find(|m| m.name == "x").unwrap();
        assert_eq!(x_method.receiver, "self");
        assert_eq!(x_method.params, &[]);
        assert_eq!(x_method.ret, "float");

        let set_x = ty.methods.iter().find(|m| m.name == "set_x").unwrap();
        assert_eq!(set_x.receiver, "var");

        let new_static = ty.statics.iter().find(|s| s.name == "new").unwrap();
        assert_eq!(new_static.params, &[("x", "float"), ("y", "float")]);
        assert_eq!(new_static.ret, "Vec2");
    }

    #[test]
    fn provider_metadata_json_includes_methods() {
        let json = exports_to_json(&anvyx_exports(), &anvyx_type_exports());
        assert!(json.contains("\"methods\":["));
        assert!(json.contains("\"statics\":["));
        assert!(json.contains("\"receiver\":\"self\""));
        assert!(json.contains("\"name\":\"new\""));
    }
}

// -- #[field] annotation + field handlers + provider integration tests --

mod field_provider {
    use anvyx_lang::{Value, export_methods, export_type, exports_to_json};

    #[derive(Clone)]
    #[export_type(name = "Pos")]
    pub struct Pos {
        #[field]
        pub x: f32,
        #[field]
        pub y: f32,
        pub internal: f32, // not exported — no #[field]
    }

    #[export_methods]
    impl Pos {
        pub fn new(x: f32, y: f32) -> Pos {
            Pos {
                x,
                y,
                internal: 0.0,
            }
        }
    }

    anvyx_lang::provider!(types: [Pos]);

    #[test]
    fn field_handler_count() {
        let externs = anvyx_externs();
        // 1 static method (Pos::new) + 4 field handlers (get_x, set_x, get_y, set_y)
        assert_eq!(externs.len(), 5);
    }

    #[test]
    fn field_handler_keys() {
        let externs = anvyx_externs();
        assert!(externs.contains_key("Pos::__get_x"));
        assert!(externs.contains_key("Pos::__set_x"));
        assert!(externs.contains_key("Pos::__get_y"));
        assert!(externs.contains_key("Pos::__set_y"));
        assert!(externs.contains_key("Pos::new"));
        // internal field should NOT have handlers
        assert!(!externs.contains_key("Pos::__get_internal"));
    }

    #[test]
    fn field_getter_returns_correct_value() {
        let externs = anvyx_externs();
        let result = externs["Pos::new"](vec![Value::Float(3.5), Value::Float(7.0)]).unwrap();
        let Value::ExternHandle(id) = result else {
            panic!("expected ExternHandle")
        };
        let x = externs["Pos::__get_x"](vec![Value::ExternHandle(id.clone())]).unwrap();
        assert_eq!(x, Value::Float(3.5));
        let y = externs["Pos::__get_y"](vec![Value::ExternHandle(id)]).unwrap();
        assert_eq!(y, Value::Float(7.0));
    }

    #[test]
    fn field_setter_modifies_value() {
        let externs = anvyx_externs();
        let result = externs["Pos::new"](vec![Value::Float(1.0), Value::Float(2.0)]).unwrap();
        let Value::ExternHandle(id) = result else {
            panic!("expected ExternHandle")
        };

        externs["Pos::__set_x"](vec![Value::ExternHandle(id.clone()), Value::Float(99.0)]).unwrap();
        let x = externs["Pos::__get_x"](vec![Value::ExternHandle(id)]).unwrap();
        assert_eq!(x, Value::Float(99.0));
    }

    #[test]
    fn field_metadata_in_type_exports() {
        let types = anvyx_type_exports();
        assert_eq!(types.len(), 1);
        let ty = &types[0];
        assert_eq!(ty.name, "Pos");
        assert_eq!(ty.fields.len(), 2);
        assert_eq!(ty.fields[0].name, "x");
        assert_eq!(ty.fields[0].ty, "float");
        assert_eq!(ty.fields[1].name, "y");
        assert_eq!(ty.fields[1].ty, "float");
    }

    #[test]
    fn field_metadata_json() {
        let json = exports_to_json(&anvyx_exports(), &anvyx_type_exports());
        assert!(json.contains("\"fields\":["));
        assert!(json.contains("\"name\":\"x\""));
        assert!(json.contains("\"ty\":\"float\""));
    }
}

mod getter_setter_tests {
    use anvyx_lang::{Value, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Vec2")]
    pub struct GsVec2(f32, f32);

    #[export_methods(name = "Vec2")]
    impl GsVec2 {
        pub fn new(x: f32, y: f32) -> GsVec2 {
            GsVec2(x, y)
        }
        #[getter]
        pub fn x(&self) -> f32 {
            self.0
        }
        #[setter]
        pub fn set_x(&mut self, v: f32) {
            self.0 = v;
        }
        #[getter]
        pub fn y(&self) -> f32 {
            self.1
        }
        #[setter]
        pub fn set_y(&mut self, v: f32) {
            self.1 = v;
        }
        pub fn length(&self) -> f32 {
            (self.0 * self.0 + self.1 * self.1).sqrt()
        }
    }

    anvyx_lang::provider!(types: [GsVec2]);

    #[test]
    fn getter_setter_handlers_registered() {
        let externs = anvyx_externs();
        assert!(externs.contains_key("Vec2::__get_x"));
        assert!(externs.contains_key("Vec2::__set_x"));
        assert!(externs.contains_key("Vec2::__get_y"));
        assert!(externs.contains_key("Vec2::__set_y"));
        assert!(externs.contains_key("Vec2::new"));
        assert!(externs.contains_key("Vec2::length"));
        assert_eq!(externs.len(), 6);
    }

    #[test]
    fn getter_setter_type_exports_fields() {
        let types = anvyx_type_exports();
        assert_eq!(types.len(), 1);
        let ty = &types[0];
        assert_eq!(ty.fields.len(), 2);
        assert_eq!(ty.fields[0].name, "x");
        assert_eq!(ty.fields[0].ty, "float");
        assert_eq!(ty.fields[1].name, "y");
        assert_eq!(ty.fields[1].ty, "float");
    }

    #[test]
    fn getter_setter_methods_exclude_getters_setters() {
        let types = anvyx_type_exports();
        let ty = &types[0];
        assert_eq!(ty.methods.len(), 1);
        assert_eq!(ty.methods[0].name, "length");
    }

    #[test]
    fn getter_returns_correct_value() {
        let externs = anvyx_externs();
        let handle = externs["Vec2::new"](vec![Value::Float(3.0), Value::Float(4.0)]).unwrap();
        let Value::ExternHandle(ref ehd) = handle else {
            panic!("expected ExternHandle")
        };
        let x = externs["Vec2::__get_x"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(x, Value::Float(3.0));
        let y = externs["Vec2::__get_y"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(y, Value::Float(4.0));
    }

    #[test]
    fn setter_modifies_value() {
        let externs = anvyx_externs();
        let handle = externs["Vec2::new"](vec![Value::Float(1.0), Value::Float(2.0)]).unwrap();
        let Value::ExternHandle(ref ehd) = handle else {
            panic!("expected ExternHandle")
        };
        externs["Vec2::__set_x"](vec![Value::ExternHandle(ehd.clone()), Value::Float(99.0)])
            .unwrap();
        let x = externs["Vec2::__get_x"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(x, Value::Float(99.0));
    }
}

mod getter_setter_with_field_tests {
    use anvyx_lang::{Value, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Sprite")]
    pub struct GsSprite {
        #[field]
        pub x: f32,
        #[field]
        pub y: f32,
        scale: f32,
    }

    #[export_methods(name = "Sprite")]
    impl GsSprite {
        pub fn new(x: f32, y: f32) -> GsSprite {
            GsSprite { x, y, scale: 1.0 }
        }
        #[getter]
        pub fn scale(&self) -> f32 {
            self.scale
        }
        #[setter]
        pub fn set_scale(&mut self, v: f32) {
            self.scale = v;
        }
    }

    anvyx_lang::provider!(types: [GsSprite]);

    #[test]
    fn combined_field_and_getter_fields() {
        let types = anvyx_type_exports();
        assert_eq!(types.len(), 1);
        let ty = &types[0];
        // x, y from #[field]; scale from #[getter]
        assert_eq!(ty.fields.len(), 3);
        assert_eq!(ty.fields[0].name, "x");
        assert_eq!(ty.fields[1].name, "y");
        assert_eq!(ty.fields[2].name, "scale");
        assert_eq!(ty.fields[2].ty, "float");
    }

    #[test]
    fn getter_setter_scale_works() {
        let externs = anvyx_externs();
        let handle = externs["Sprite::new"](vec![Value::Float(0.0), Value::Float(0.0)]).unwrap();
        let Value::ExternHandle(ref ehd) = handle else {
            panic!("expected ExternHandle")
        };
        let scale = externs["Sprite::__get_scale"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(scale, Value::Float(1.0));
        externs["Sprite::__set_scale"](vec![Value::ExternHandle(ehd.clone()), Value::Float(2.5)])
            .unwrap();
        let scale2 =
            externs["Sprite::__get_scale"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(scale2, Value::Float(2.5));
    }
}

mod getter_setter_name_override_tests {
    use anvyx_lang::{Value, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Pt")]
    pub struct SomePoint {
        #[field]
        pub x: f32,
    }

    #[export_methods(name = "Pt")]
    impl SomePoint {
        pub fn new(x: f32) -> SomePoint {
            SomePoint { x }
        }
        #[getter]
        pub fn mag(&self) -> f32 {
            self.x.abs()
        }
        #[setter]
        pub fn set_mag(&mut self, v: f32) {
            self.x = v;
        }
    }

    anvyx_lang::provider!(types: [SomePoint]);

    #[test]
    fn name_override_uses_pt_prefix() {
        let externs = anvyx_externs();
        assert!(externs.contains_key("Pt::new"));
        assert!(externs.contains_key("Pt::__get_mag"));
        assert!(externs.contains_key("Pt::__set_mag"));
        assert!(externs.contains_key("Pt::__get_x"));
        assert!(externs.contains_key("Pt::__set_x"));
        assert!(!externs.contains_key("SomePoint::new"));
    }

    #[test]
    fn name_override_getter_works() {
        let externs = anvyx_externs();
        let handle = externs["Pt::new"](vec![Value::Float(-5.0)]).unwrap();
        let Value::ExternHandle(ref ehd) = handle else {
            panic!("expected ExternHandle")
        };
        let mag = externs["Pt::__get_mag"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(mag, Value::Float(5.0));
    }
}

mod init_explicit_tests {
    use anvyx_lang::{Value, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Vec2")]
    pub struct InitVec2(f32, f32);

    #[export_methods(name = "Vec2")]
    impl InitVec2 {
        #[init]
        pub fn create(x: f32, y: f32) -> InitVec2 {
            InitVec2(x, y)
        }
        #[getter]
        pub fn x(&self) -> f32 {
            self.0
        }
        #[setter]
        pub fn set_x(&mut self, v: f32) {
            self.0 = v;
        }
        #[getter]
        pub fn y(&self) -> f32 {
            self.1
        }
        #[setter]
        pub fn set_y(&mut self, v: f32) {
            self.1 = v;
        }
        pub fn length(&self) -> f32 {
            (self.0 * self.0 + self.1 * self.1).sqrt()
        }
    }

    anvyx_lang::provider!(types: [InitVec2]);

    #[test]
    fn init_handler_registered() {
        let externs = anvyx_externs();
        assert!(externs.contains_key("Vec2::__init__"));
    }

    #[test]
    fn init_method_not_in_statics() {
        let types = anvyx_type_exports();
        assert_eq!(types[0].statics.len(), 0);
    }

    #[test]
    fn init_has_init_flag() {
        let types = anvyx_type_exports();
        assert!(types[0].has_init);
    }

    #[test]
    fn init_handler_creates_handle() {
        let externs = anvyx_externs();
        let handle = externs["Vec2::__init__"](vec![Value::Float(3.0), Value::Float(4.0)]).unwrap();
        let Value::ExternHandle(ref ehd) = handle else {
            panic!("expected ExternHandle")
        };
        let x = externs["Vec2::__get_x"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(x, Value::Float(3.0));
        let y = externs["Vec2::__get_y"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(y, Value::Float(4.0));
    }

    #[test]
    fn init_regular_methods_still_work() {
        let externs = anvyx_externs();
        assert!(externs.contains_key("Vec2::length"));
    }
}

mod init_auto_tests {
    use anvyx_lang::{Value, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "AutoPos")]
    pub struct AutoPos {
        #[field]
        pub x: f32,
        #[field]
        pub y: f32,
    }

    #[export_methods(name = "AutoPos")]
    impl AutoPos {
        pub fn new(x: f32, y: f32) -> AutoPos {
            AutoPos { x, y }
        }
    }

    anvyx_lang::provider!(types: [AutoPos]);

    #[test]
    fn auto_init_handler_registered() {
        let externs = anvyx_externs();
        assert!(externs.contains_key("AutoPos::__init__"));
    }

    #[test]
    fn auto_init_new_still_exists() {
        let externs = anvyx_externs();
        assert!(externs.contains_key("AutoPos::new"));
    }

    #[test]
    fn auto_init_has_init_flag() {
        let types = anvyx_type_exports();
        assert!(types[0].has_init);
    }

    #[test]
    fn auto_init_handler_works() {
        let externs = anvyx_externs();
        let handle =
            externs["AutoPos::__init__"](vec![Value::Float(1.5), Value::Float(2.5)]).unwrap();
        let Value::ExternHandle(ref ehd) = handle else {
            panic!("expected ExternHandle")
        };
        let x = externs["AutoPos::__get_x"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(x, Value::Float(1.5));
        let y = externs["AutoPos::__get_y"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(y, Value::Float(2.5));
    }
}

mod init_no_auto_tests {
    use anvyx_lang::{Value, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Obj")]
    pub struct NoAutoObj {
        #[field]
        pub x: f32,
        internal: f32,
    }

    #[export_methods(name = "Obj")]
    impl NoAutoObj {
        pub fn new(x: f32) -> NoAutoObj {
            NoAutoObj { x, internal: 0.0 }
        }
    }

    anvyx_lang::provider!(types: [NoAutoObj]);

    #[test]
    fn no_auto_init_handler() {
        let externs = anvyx_externs();
        assert!(!externs.contains_key("Obj::__init__"));
    }

    #[test]
    fn no_auto_init_flag() {
        let types = anvyx_type_exports();
        assert!(!types[0].has_init);
    }

    #[test]
    fn no_auto_new_still_works() {
        let externs = anvyx_externs();
        let handle = externs["Obj::new"](vec![Value::Float(7.0)]).unwrap();
        let Value::ExternHandle(ref ehd) = handle else {
            panic!("expected ExternHandle")
        };
        let x = externs["Obj::__get_x"](vec![Value::ExternHandle(ehd.clone())]).unwrap();
        assert_eq!(x, Value::Float(7.0));
    }
}

// -- #[op(...)] annotation tests --

mod op_tests {
    use anvyx_lang::{Value, export_methods, export_type};

    use super::extern_handle;

    #[derive(Clone)]
    #[export_type(name = "Vec2")]
    pub struct OpVec2 {
        pub x: f32,
        pub y: f32,
    }

    #[export_methods(name = "Vec2")]
    impl OpVec2 {
        #[init]
        pub fn create(x: f32, y: f32) -> OpVec2 {
            OpVec2 { x, y }
        }

        #[getter]
        pub fn x(&self) -> f32 {
            self.x
        }
        #[setter]
        pub fn set_x(&mut self, v: f32) {
            self.x = v;
        }
        #[getter]
        pub fn y(&self) -> f32 {
            self.y
        }
        #[setter]
        pub fn set_y(&mut self, v: f32) {
            self.y = v;
        }

        #[op(Self + Self)]
        pub fn add(&self, other: &OpVec2) -> OpVec2 {
            OpVec2 {
                x: self.x + other.x,
                y: self.y + other.y,
            }
        }

        #[op(Self - Self)]
        pub fn sub(&self, other: &OpVec2) -> OpVec2 {
            OpVec2 {
                x: self.x - other.x,
                y: self.y - other.y,
            }
        }

        #[op(Self * float)]
        pub fn mul_scalar(&self, s: f32) -> OpVec2 {
            OpVec2 {
                x: self.x * s,
                y: self.y * s,
            }
        }

        #[op(float * Self)]
        pub fn scalar_mul(&self, s: f32) -> OpVec2 {
            OpVec2 {
                x: self.x * s,
                y: self.y * s,
            }
        }

        #[op(-Self)]
        pub fn neg(&self) -> OpVec2 {
            OpVec2 {
                x: -self.x,
                y: -self.y,
            }
        }

        #[op(Self == Self)]
        pub fn eq(&self, other: &OpVec2) -> bool {
            self.x == other.x && self.y == other.y
        }
    }

    #[test]
    fn op_add_generates_correct_key() {
        let (key, _) = __anvyx_method_OpVec2___op_add__Vec2();
        assert_eq!(key, "Vec2::__op_add__Vec2");
    }

    #[test]
    fn op_add_handler_works() {
        let id_a = __ANVYX_STORE_OPVEC2.with(|s| s.borrow_mut().insert(OpVec2 { x: 1.0, y: 2.0 }));
        let id_b = __ANVYX_STORE_OPVEC2.with(|s| s.borrow_mut().insert(OpVec2 { x: 3.0, y: 4.0 }));
        let (_, handler) = __anvyx_method_OpVec2___op_add__Vec2();
        let result = handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
        let Value::ExternHandle(new_handle) = result else {
            panic!("expected ExternHandle")
        };
        __ANVYX_STORE_OPVEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(new_handle.id).unwrap();
            assert_eq!(v.x, 4.0);
            assert_eq!(v.y, 6.0);
        });
        __ANVYX_STORE_OPVEC2.with(|s| {
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
        });
    }

    #[test]
    fn op_sub_handler_works() {
        let id_a = __ANVYX_STORE_OPVEC2.with(|s| s.borrow_mut().insert(OpVec2 { x: 5.0, y: 7.0 }));
        let id_b = __ANVYX_STORE_OPVEC2.with(|s| s.borrow_mut().insert(OpVec2 { x: 2.0, y: 3.0 }));
        let (_, handler) = __anvyx_method_OpVec2___op_sub__Vec2();
        let result = handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
        let Value::ExternHandle(new_handle) = result else {
            panic!("expected ExternHandle")
        };
        __ANVYX_STORE_OPVEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(new_handle.id).unwrap();
            assert_eq!(v.x, 3.0);
            assert_eq!(v.y, 4.0);
        });
        __ANVYX_STORE_OPVEC2.with(|s| {
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
        });
    }

    #[test]
    fn op_mul_scalar_generates_correct_key() {
        let (key, _) = __anvyx_method_OpVec2___op_mul__float();
        assert_eq!(key, "Vec2::__op_mul__float");
    }

    #[test]
    fn op_mul_scalar_handler_works() {
        let id = __ANVYX_STORE_OPVEC2.with(|s| s.borrow_mut().insert(OpVec2 { x: 2.0, y: 3.0 }));
        let (_, handler) = __anvyx_method_OpVec2___op_mul__float();
        let result = handler(vec![extern_handle(id), Value::Float(2.0)]).unwrap();
        let Value::ExternHandle(new_handle) = result else {
            panic!("expected ExternHandle")
        };
        __ANVYX_STORE_OPVEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(new_handle.id).unwrap();
            assert_eq!(v.x, 4.0);
            assert_eq!(v.y, 6.0);
        });
        __ANVYX_STORE_OPVEC2.with(|s| {
            s.borrow_mut().remove(id).unwrap();
        });
    }

    #[test]
    fn op_scalar_mul_generates_correct_key() {
        let (key, _) = __anvyx_method_OpVec2___op_rmul__float();
        assert_eq!(key, "Vec2::__op_rmul__float");
    }

    #[test]
    fn op_scalar_mul_handler_swaps_args() {
        let id = __ANVYX_STORE_OPVEC2.with(|s| s.borrow_mut().insert(OpVec2 { x: 2.0, y: 3.0 }));
        let (_, handler) = __anvyx_method_OpVec2___op_rmul__float();
        // right-dispatch: float first, Vec2 handle second
        let result = handler(vec![Value::Float(2.0), extern_handle(id)]).unwrap();
        let Value::ExternHandle(new_handle) = result else {
            panic!("expected ExternHandle")
        };
        __ANVYX_STORE_OPVEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(new_handle.id).unwrap();
            assert_eq!(v.x, 4.0);
            assert_eq!(v.y, 6.0);
        });
        __ANVYX_STORE_OPVEC2.with(|s| {
            s.borrow_mut().remove(id).unwrap();
        });
    }

    #[test]
    fn op_neg_generates_correct_key() {
        let (key, _) = __anvyx_method_OpVec2___op_neg();
        assert_eq!(key, "Vec2::__op_neg");
    }

    #[test]
    fn op_neg_handler_works() {
        let id = __ANVYX_STORE_OPVEC2.with(|s| s.borrow_mut().insert(OpVec2 { x: 1.0, y: -2.0 }));
        let (_, handler) = __anvyx_method_OpVec2___op_neg();
        let result = handler(vec![extern_handle(id)]).unwrap();
        let Value::ExternHandle(new_handle) = result else {
            panic!("expected ExternHandle")
        };
        __ANVYX_STORE_OPVEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(new_handle.id).unwrap();
            assert_eq!(v.x, -1.0);
            assert_eq!(v.y, 2.0);
        });
        __ANVYX_STORE_OPVEC2.with(|s| {
            s.borrow_mut().remove(id).unwrap();
        });
    }

    #[test]
    fn op_eq_generates_correct_key() {
        let (key, _) = __anvyx_method_OpVec2___op_eq__Vec2();
        assert_eq!(key, "Vec2::__op_eq__Vec2");
    }

    #[test]
    fn op_eq_handler_returns_bool() {
        let id_a = __ANVYX_STORE_OPVEC2.with(|s| s.borrow_mut().insert(OpVec2 { x: 1.0, y: 2.0 }));
        let id_b = __ANVYX_STORE_OPVEC2.with(|s| s.borrow_mut().insert(OpVec2 { x: 1.0, y: 2.0 }));
        let id_c = __ANVYX_STORE_OPVEC2.with(|s| s.borrow_mut().insert(OpVec2 { x: 9.0, y: 9.0 }));
        let (_, handler) = __anvyx_method_OpVec2___op_eq__Vec2();
        let result_eq = handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
        assert_eq!(result_eq, Value::Bool(true));
        let result_ne = handler(vec![extern_handle(id_a), extern_handle(id_c)]).unwrap();
        assert_eq!(result_ne, Value::Bool(false));
        __ANVYX_STORE_OPVEC2.with(|s| {
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
            s.borrow_mut().remove(id_c).unwrap();
        });
    }

    #[test]
    fn op_handlers_registered_in_companion() {
        let handlers = __anvyx_methods_opvec2();
        let keys: Vec<&str> = handlers.iter().map(|(k, _)| *k).collect();
        assert!(keys.contains(&"Vec2::__op_add__Vec2"));
        assert!(keys.contains(&"Vec2::__op_sub__Vec2"));
        assert!(keys.contains(&"Vec2::__op_mul__float"));
        assert!(keys.contains(&"Vec2::__op_rmul__float"));
        assert!(keys.contains(&"Vec2::__op_neg"));
        assert!(keys.contains(&"Vec2::__op_eq__Vec2"));
    }

    #[test]
    fn op_metadata_const_has_all_entries() {
        let ops = __ANVYX_OPS_DECL_OPVEC2();
        assert_eq!(ops.len(), 6);
    }

    #[test]
    fn op_metadata_binary_left_dispatch() {
        let ops = __ANVYX_OPS_DECL_OPVEC2();
        let add = ops.iter().find(|o| o.op == "Add").unwrap();
        assert_eq!(add.rhs, Some("Vec2"));
        assert!(add.lhs.is_none());
        assert_eq!(add.ret, "Vec2");
    }

    #[test]
    fn op_metadata_binary_right_dispatch() {
        let ops = __ANVYX_OPS_DECL_OPVEC2();
        // float * Self
        let rmul = ops
            .iter()
            .find(|o| o.op == "Mul" && o.lhs.is_some())
            .unwrap();
        assert!(rmul.rhs.is_none());
        assert_eq!(rmul.lhs, Some("float"));
        assert_eq!(rmul.ret, "Vec2");
    }

    #[test]
    fn op_metadata_binary_left_float() {
        let ops = __ANVYX_OPS_DECL_OPVEC2();
        // Self * float
        let mul = ops
            .iter()
            .find(|o| o.op == "Mul" && o.rhs.is_some())
            .unwrap();
        assert_eq!(mul.rhs, Some("float"));
        assert!(mul.lhs.is_none());
    }

    #[test]
    fn op_metadata_unary() {
        let ops = __ANVYX_OPS_DECL_OPVEC2();
        let neg = ops.iter().find(|o| o.op == "Neg").unwrap();
        assert!(neg.rhs.is_none());
        assert!(neg.lhs.is_none());
        assert_eq!(neg.ret, "Vec2");
    }

    #[test]
    fn op_metadata_eq_returns_bool() {
        let ops = __ANVYX_OPS_DECL_OPVEC2();
        let eq = ops.iter().find(|o| o.op == "Eq").unwrap();
        assert_eq!(eq.rhs, Some("Vec2"));
        assert!(eq.lhs.is_none());
        assert_eq!(eq.ret, "bool");
    }
}

// -- Fallible return tests --

#[export_fn]
pub fn checked_div(a: i64, b: i64) -> Result<i64, RuntimeError> {
    if b == 0 {
        return Err(RuntimeError::new("division by zero"));
    }
    Ok(a / b)
}

#[test]
fn fallible_int_ok() {
    let (_, handler) = __anvyx_export_checked_div();
    let result = handler(vec![Value::Int(10), Value::Int(2)]).unwrap();
    assert_eq!(result, Value::Int(5));
}

#[test]
fn fallible_int_err() {
    let (_, handler) = __anvyx_export_checked_div();
    let result = handler(vec![Value::Int(10), Value::Int(0)]);
    assert!(result.is_err());
    assert!(result.unwrap_err().message.contains("division by zero"));
}

#[export_fn]
pub fn must_upper(s: String) -> Result<String, RuntimeError> {
    if s.is_empty() {
        return Err(RuntimeError::new("empty string"));
    }
    Ok(s.to_uppercase())
}

#[test]
fn fallible_string_ok() {
    let (_, handler) = __anvyx_export_must_upper();
    let result = handler(vec![Value::String(ManagedRc::new("hello".to_string()))]).unwrap();
    assert_eq!(result, Value::String(ManagedRc::new("HELLO".to_string())));
}

#[test]
fn fallible_string_err() {
    let (_, handler) = __anvyx_export_must_upper();
    let result = handler(vec![Value::String(ManagedRc::new(String::new()))]);
    assert!(result.is_err());
}

#[export_fn]
pub fn assert_positive(n: i64) -> Result<(), RuntimeError> {
    if n <= 0 {
        return Err(RuntimeError::new("not positive"));
    }
    Ok(())
}

#[test]
fn fallible_void_ok() {
    let (_, handler) = __anvyx_export_assert_positive();
    let result = handler(vec![Value::Int(1)]).unwrap();
    assert_eq!(result, Value::Nil);
}

#[test]
fn fallible_void_err() {
    let (_, handler) = __anvyx_export_assert_positive();
    let result = handler(vec![Value::Int(-1)]);
    assert!(result.is_err());
}

#[export_fn(ret = "[int]")]
pub fn parse_list(s: String) -> Result<Value, RuntimeError> {
    if s.is_empty() {
        return Err(RuntimeError::new("empty input"));
    }
    Ok(Value::List(ManagedRc::new(vec![
        Value::Int(1),
        Value::Int(2),
    ])))
}

#[test]
fn fallible_value_ok() {
    let (_, handler) = __anvyx_export_parse_list();
    let result = handler(vec![Value::String(ManagedRc::new("x".to_string()))]).unwrap();
    let Value::List(list) = result else {
        panic!("expected List");
    };
    assert_eq!(*list, vec![Value::Int(1), Value::Int(2)]);
}

#[test]
fn fallible_value_err() {
    let (_, handler) = __anvyx_export_parse_list();
    let result = handler(vec![Value::String(ManagedRc::new(String::new()))]);
    assert!(result.is_err());
}

// -- AnvyxOption return tests --

#[export_fn]
pub fn safe_div(a: i64, b: i64) -> Option<i64> {
    if b == 0 {
        return None;
    }
    Some(a / b)
}

#[test]
fn option_int_some() {
    let (_, handler) = __anvyx_export_safe_div();
    let result = handler(vec![Value::Int(10), Value::Int(2)]).unwrap();
    let Value::Enum(e) = result else {
        panic!("expected Enum");
    };
    assert_eq!(e.type_id, OPTION_TYPE_ID);
    assert_eq!(e.variant, 1);
    assert_eq!(e.fields, vec![Value::Int(5)]);
}

#[test]
fn option_int_none() {
    let (_, handler) = __anvyx_export_safe_div();
    let result = handler(vec![Value::Int(10), Value::Int(0)]).unwrap();
    let Value::Enum(e) = result else {
        panic!("expected Enum");
    };
    assert_eq!(e.type_id, OPTION_TYPE_ID);
    assert_eq!(e.variant, 0);
    assert!(e.fields.is_empty());
}

#[export_fn]
pub fn char_at(s: String, idx: i64) -> Option<String> {
    s.chars().nth(idx as usize).map(|c| c.to_string())
}

#[test]
fn option_string_some() {
    let (_, handler) = __anvyx_export_char_at();
    let result = handler(vec![
        Value::String(ManagedRc::new("hello".to_string())),
        Value::Int(1),
    ])
    .unwrap();
    let Value::Enum(e) = result else {
        panic!("expected Enum");
    };
    assert_eq!(e.type_id, OPTION_TYPE_ID);
    assert_eq!(e.variant, 1);
    let Value::String(s) = &e.fields[0] else {
        panic!("expected String");
    };
    assert_eq!(s.as_str(), "e");
}

#[test]
fn option_string_none() {
    let (_, handler) = __anvyx_export_char_at();
    let result = handler(vec![
        Value::String(ManagedRc::new("hello".to_string())),
        Value::Int(10),
    ])
    .unwrap();
    let Value::Enum(e) = result else {
        panic!("expected Enum");
    };
    assert_eq!(e.type_id, OPTION_TYPE_ID);
    assert_eq!(e.variant, 0);
    assert!(e.fields.is_empty());
}

// -- ExternDecl metadata tests for Fallible functions --

#[test]
fn fallible_int_decl() {
    let decl: ExternDecl = __ANVYX_DECL_CHECKED_DIV();
    assert_eq!(decl.ret, "int");
    assert_eq!(decl.params, &[("a", "int"), ("b", "int")]);
}

#[test]
fn fallible_string_decl() {
    let decl: ExternDecl = __ANVYX_DECL_MUST_UPPER();
    assert_eq!(decl.ret, "string");
    assert_eq!(decl.params, &[("s", "string")]);
}

#[test]
fn fallible_void_decl() {
    let decl: ExternDecl = __ANVYX_DECL_ASSERT_POSITIVE();
    assert_eq!(decl.ret, "void");
    assert_eq!(decl.params, &[("n", "int")]);
}

#[test]
fn fallible_value_decl() {
    let decl: ExternDecl = __ANVYX_DECL_PARSE_LIST();
    assert_eq!(decl.ret, "[int]");
    assert_eq!(decl.params, &[("s", "string")]);
}

// -- ExternDecl metadata tests for AnvyxOption functions --

#[test]
fn option_int_decl() {
    let decl: ExternDecl = __ANVYX_DECL_SAFE_DIV();
    assert_eq!(decl.ret, "Option<int>");
    assert_eq!(decl.params, &[("a", "int"), ("b", "int")]);
}

#[test]
fn option_string_decl() {
    let decl: ExternDecl = __ANVYX_DECL_CHAR_AT();
    assert_eq!(decl.ret, "Option<string>");
    assert_eq!(decl.params, &[("s", "string"), ("idx", "int")]);
}

// -- Provider integration tests --

mod fallible_mod {
    use anvyx_lang::{RuntimeError, export_fn};

    #[export_fn]
    pub fn safe_div(a: i64, b: i64) -> Result<i64, RuntimeError> {
        if b == 0 {
            return Err(RuntimeError::new("div by zero"));
        }
        Ok(a / b)
    }

    anvyx_lang::provider!(safe_div);
}

#[test]
fn provider_fallible_exports() {
    assert_eq!(fallible_mod::anvyx_exports()[0].ret, "int");
    assert_eq!(fallible_mod::anvyx_exports()[0].name, "safe_div");
}

#[test]
fn provider_fallible_handler() {
    let externs = fallible_mod::anvyx_externs();
    let result = externs["safe_div"](vec![Value::Int(10), Value::Int(2)]).unwrap();
    assert_eq!(result, Value::Int(5));
    let result = externs["safe_div"](vec![Value::Int(10), Value::Int(0)]);
    assert!(result.is_err());
}

mod option_mod {
    use anvyx_lang::export_fn;

    #[export_fn]
    pub fn maybe_inc(n: i64) -> Option<i64> {
        if n < 0 { None } else { Some(n + 1) }
    }

    anvyx_lang::provider!(maybe_inc);
}

#[test]
fn provider_option_exports() {
    assert_eq!(option_mod::anvyx_exports()[0].ret, "Option<int>");
    assert_eq!(option_mod::anvyx_exports()[0].name, "maybe_inc");
}

#[test]
fn provider_option_handler() {
    let externs = option_mod::anvyx_externs();

    let result = externs["maybe_inc"](vec![Value::Int(5)]).unwrap();
    let Value::Enum(e) = result else {
        panic!("expected Enum");
    };
    assert_eq!(e.type_id, OPTION_TYPE_ID);
    assert_eq!(e.variant, 1);
    assert_eq!(e.fields, vec![Value::Int(6)]);

    let result = externs["maybe_inc"](vec![Value::Int(-1)]).unwrap();
    let Value::Enum(e) = result else {
        panic!("expected Enum");
    };
    assert_eq!(e.type_id, OPTION_TYPE_ID);
    assert_eq!(e.variant, 0);
    assert!(e.fields.is_empty());
}

// -- Getter/setter pair returning/accepting an extern type --

mod getter_extern_type_tests {
    use anvyx_lang::{Value, export_methods, export_type};

    use super::extern_handle;

    #[derive(Clone)]
    #[export_type(name = "Inner")]
    pub struct Inner {
        pub val: f32,
    }

    #[export_methods]
    impl Inner {}

    #[derive(Clone)]
    #[export_type(name = "Container")]
    pub struct Container {
        pub inner_val: f32,
    }

    #[export_methods(name = "Container")]
    impl Container {
        pub fn new(inner_val: f32) -> Container {
            Container { inner_val }
        }

        #[getter]
        pub fn inner(&self) -> Inner {
            Inner {
                val: self.inner_val,
            }
        }

        #[setter]
        pub fn set_inner(&mut self, v: Inner) {
            self.inner_val = v.val;
        }
    }

    anvyx_lang::provider!(types: [Inner, Container]);

    #[test]
    fn getter_extern_type_returns_handle() {
        let externs = anvyx_externs();
        let container = externs["Container::new"](vec![Value::Float(3.5)]).unwrap();
        let Value::ExternHandle(ref container_ehd) = container else {
            panic!("expected ExternHandle for Container");
        };
        let result =
            externs["Container::__get_inner"](vec![Value::ExternHandle(container_ehd.clone())])
                .unwrap();
        let Value::ExternHandle(ref inner_ehd) = result else {
            panic!("expected ExternHandle for Inner");
        };
        let inner_id = inner_ehd.id;
        __ANVYX_STORE_INNER.with(|s| {
            let store = s.borrow();
            let inner = store.borrow(inner_id).unwrap();
            assert_eq!(inner.val, 3.5);
        });
        // container and result drop here, auto-cleaning up via real drop_fn
    }

    #[test]
    fn setter_extern_type_consumes_handle() {
        let externs = anvyx_externs();
        let container = externs["Container::new"](vec![Value::Float(1.0)]).unwrap();
        let Value::ExternHandle(ref container_ehd) = container else {
            panic!("expected ExternHandle for Container");
        };

        // Manually insert an Inner to pass to the setter
        let inner_id = __ANVYX_STORE_INNER.with(|s| s.borrow_mut().insert(Inner { val: 9.0 }));

        // Set inner — from_anvyx borrows+clones; original stays in Inner store
        externs["Container::__set_inner"](vec![
            Value::ExternHandle(container_ehd.clone()),
            extern_handle(inner_id),
        ])
        .unwrap();

        // Original Inner is still in store (not consumed)
        __ANVYX_STORE_INNER.with(|s| {
            assert!(s.borrow().borrow(inner_id).is_ok());
            s.borrow_mut().remove(inner_id).unwrap();
        });

        // Verify the container now reflects the new inner_val
        let new_inner =
            externs["Container::__get_inner"](vec![Value::ExternHandle(container_ehd.clone())])
                .unwrap();
        let Value::ExternHandle(ref new_ehd) = new_inner else {
            panic!("expected ExternHandle");
        };
        __ANVYX_STORE_INNER.with(|s| {
            let store = s.borrow();
            let inner = store.borrow(new_ehd.id).unwrap();
            assert_eq!(inner.val, 9.0);
        });
        // container and new_inner auto-cleanup on drop
    }

    #[test]
    fn getter_setter_extern_round_trip() {
        let externs = anvyx_externs();

        // Create container A with inner_val 7.0
        let container_a = externs["Container::new"](vec![Value::Float(7.0)]).unwrap();
        let Value::ExternHandle(ref ehd_a) = container_a else {
            panic!("expected ExternHandle");
        };

        // Get inner from A (creates a new Inner {val: 7.0} inserted into Inner store)
        let inner_from_a =
            externs["Container::__get_inner"](vec![Value::ExternHandle(ehd_a.clone())]).unwrap();

        // Create container B with inner_val 0.0
        let container_b = externs["Container::new"](vec![Value::Float(0.0)]).unwrap();
        let Value::ExternHandle(ref ehd_b) = container_b else {
            panic!("expected ExternHandle");
        };

        // Set B's inner to the value from A — inner_from_a is consumed by the setter
        externs["Container::__set_inner"](vec![Value::ExternHandle(ehd_b.clone()), inner_from_a])
            .unwrap();

        // Verify B now returns an inner with val 7.0
        let b_inner =
            externs["Container::__get_inner"](vec![Value::ExternHandle(ehd_b.clone())]).unwrap();
        let Value::ExternHandle(ref b_inner_ehd) = b_inner else {
            panic!("expected ExternHandle");
        };
        __ANVYX_STORE_INNER.with(|s| {
            let store = s.borrow();
            let inner = store.borrow(b_inner_ehd.id).unwrap();
            assert_eq!(inner.val, 7.0);
        });
        // All handles auto-cleanup on drop
    }

    #[test]
    fn getter_extern_type_metadata() {
        let types = anvyx_type_exports();
        let container_ty = types.iter().find(|t| t.name == "Container").unwrap();
        assert_eq!(container_ty.fields.len(), 1);
        assert_eq!(container_ty.fields[0].name, "inner");
        assert_eq!(container_ty.fields[0].ty, "Inner");
        assert!(container_ty.fields[0].computed);
    }

    #[test]
    fn getter_extern_handler_keys() {
        let externs = anvyx_externs();
        assert!(externs.contains_key("Container::__get_inner"));
        assert!(externs.contains_key("Container::__set_inner"));
        assert!(externs.contains_key("Container::new"));
    }
}

// -- #[init] accepting an extern type param --

mod init_extern_param_tests {
    use anvyx_lang::{Value, export_methods, export_type};

    use super::extern_handle;

    #[derive(Clone)]
    #[export_type(name = "Pos")]
    pub struct Pos {
        pub x: f32,
        pub y: f32,
    }

    #[export_methods]
    impl Pos {}

    #[derive(Clone)]
    #[export_type(name = "Entity")]
    pub struct Entity {
        pub px: f32,
        pub py: f32,
        pub hp: f32,
    }

    #[export_methods(name = "Entity")]
    impl Entity {
        #[init]
        pub fn create(pos: Pos, hp: f32) -> Entity {
            Entity {
                px: pos.x,
                py: pos.y,
                hp,
            }
        }

        #[getter]
        pub fn hp(&self) -> f32 {
            self.hp
        }

        #[setter]
        pub fn set_hp(&mut self, v: f32) {
            self.hp = v;
        }
    }

    anvyx_lang::provider!(types: [Pos, Entity]);

    #[test]
    fn init_extern_param_creates_entity() {
        let externs = anvyx_externs();
        let pos_id = __ANVYX_STORE_POS.with(|s| s.borrow_mut().insert(Pos { x: 10.0, y: 20.0 }));
        let entity =
            externs["Entity::__init__"](vec![extern_handle(pos_id), Value::Float(100.0)]).unwrap();
        let Value::ExternHandle(ref entity_ehd) = entity else {
            panic!("expected ExternHandle for Entity");
        };
        __ANVYX_STORE_ENTITY.with(|s| {
            let store = s.borrow();
            let e = store.borrow(entity_ehd.id).unwrap();
            assert_eq!(e.px, 10.0);
            assert_eq!(e.py, 20.0);
            assert_eq!(e.hp, 100.0);
        });
        // entity auto-cleanup on drop; pos stays in store (borrow+clone semantics)
    }

    #[test]
    fn init_extern_param_does_not_consume_pos() {
        let externs = anvyx_externs();
        let pos_id = __ANVYX_STORE_POS.with(|s| s.borrow_mut().insert(Pos { x: 1.0, y: 2.0 }));
        externs["Entity::__init__"](vec![extern_handle(pos_id), Value::Float(50.0)]).unwrap();
        // from_anvyx borrows+clones; Pos is NOT consumed from store
        __ANVYX_STORE_POS.with(|s| {
            assert!(s.borrow().borrow(pos_id).is_ok());
            s.borrow_mut().remove(pos_id).unwrap();
        });
        // entity handle auto-cleanup on drop
    }

    #[test]
    fn init_extern_param_metadata() {
        let types = anvyx_type_exports();
        let entity_ty = types.iter().find(|t| t.name == "Entity").unwrap();
        assert!(entity_ty.has_init);
        let fields = &entity_ty.fields;
        // fields = init_fields [("pos", "Pos"), ("hp", "float")];
        // getter "hp" is filtered because "hp" is already in init_names
        assert_eq!(fields.len(), 2);
        let pos_field = fields.iter().find(|f| f.name == "pos").unwrap();
        assert_eq!(pos_field.ty, "Pos");
        assert!(!pos_field.computed);
        let hp_field = fields.iter().find(|f| f.name == "hp").unwrap();
        assert_eq!(hp_field.ty, "float");
        assert!(!hp_field.computed);
    }

    #[test]
    fn init_extern_param_wrong_type_fails() {
        let externs = anvyx_externs();
        // Pass float instead of ExternHandle for the Pos param
        let result = externs["Entity::__init__"](vec![Value::Float(1.0), Value::Float(50.0)]);
        assert!(result.is_err());
    }
}

// -- #[init] with a String param --

mod init_string_param_tests {
    use anvyx_lang::{ManagedRc, Value, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Label")]
    pub struct Label {
        pub text: String,
        pub size: f32,
    }

    #[export_methods(name = "Label")]
    impl Label {
        #[init]
        pub fn create(text: String, size: f32) -> Label {
            Label { text, size }
        }
    }

    anvyx_lang::provider!(types: [Label]);

    #[test]
    fn init_string_param_creates_label() {
        let externs = anvyx_externs();
        let label = externs["Label::__init__"](vec![
            Value::String(ManagedRc::new("hello".to_string())),
            Value::Float(16.0),
        ])
        .unwrap();
        let Value::ExternHandle(ref label_ehd) = label else {
            panic!("expected ExternHandle for Label");
        };
        let label_id = label_ehd.id;
        __ANVYX_STORE_LABEL.with(|s| {
            let store = s.borrow();
            let v = store.borrow(label_id).unwrap();
            assert_eq!(v.text, "hello");
            assert_eq!(v.size, 16.0);
        });
        // label auto-cleanup on drop
    }

    #[test]
    fn init_string_param_metadata() {
        let types = anvyx_type_exports();
        let label_ty = types.iter().find(|t| t.name == "Label").unwrap();
        assert!(label_ty.has_init);
        let fields = &label_ty.fields;
        assert_eq!(fields.len(), 2);
        assert!(fields.iter().any(|f| f.name == "text" && f.ty == "string"));
        assert!(fields.iter().any(|f| f.name == "size" && f.ty == "float"));
    }
}

// -- #[init] returning Result<Self, RuntimeError> --

mod fallible_init_tests {
    use anvyx_lang::{RuntimeError, Value, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Ratio")]
    pub struct Ratio {
        pub num: f32,
        pub den: f32,
    }

    #[export_methods(name = "Ratio")]
    impl Ratio {
        #[init]
        pub fn create(num: f32, den: f32) -> Result<Ratio, RuntimeError> {
            if den == 0.0 {
                return Err(RuntimeError::new("denominator cannot be zero"));
            }
            Ok(Ratio { num, den })
        }

        #[getter]
        pub fn num(&self) -> f32 {
            self.num
        }

        #[setter]
        pub fn set_num(&mut self, v: f32) {
            self.num = v;
        }

        #[getter]
        pub fn den(&self) -> f32 {
            self.den
        }

        #[setter]
        pub fn set_den(&mut self, v: f32) {
            self.den = v;
        }
    }

    anvyx_lang::provider!(types: [Ratio]);

    #[test]
    fn fallible_init_ok() {
        let externs = anvyx_externs();
        let result = externs["Ratio::__init__"](vec![Value::Float(3.0), Value::Float(4.0)]);
        assert!(result.is_ok());
        let ratio = result.unwrap();
        let Value::ExternHandle(ref ratio_ehd) = ratio else {
            panic!("expected ExternHandle for Ratio");
        };
        __ANVYX_STORE_RATIO.with(|s| {
            let store = s.borrow();
            let v = store.borrow(ratio_ehd.id).unwrap();
            assert_eq!(v.num, 3.0);
            assert_eq!(v.den, 4.0);
        });
        // ratio auto-cleanup on drop
    }

    #[test]
    fn fallible_init_err() {
        let externs = anvyx_externs();
        let result = externs["Ratio::__init__"](vec![Value::Float(1.0), Value::Float(0.0)]);
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("denominator"));
    }

    #[test]
    fn fallible_init_has_init_flag() {
        let types = anvyx_type_exports();
        let ratio_ty = types.iter().find(|t| t.name == "Ratio").unwrap();
        assert!(ratio_ty.has_init);
    }

    #[test]
    fn fallible_init_handler_key() {
        let externs = anvyx_externs();
        assert!(externs.contains_key("Ratio::__init__"));
    }
}

// -- #[op] returning Option<f32> --

mod op_option_return_tests {
    use anvyx_lang::{OPTION_TYPE_ID, Value, export_methods, export_type};

    use super::extern_handle;

    #[derive(Clone)]
    #[export_type(name = "Num")]
    pub struct Num {
        pub val: f32,
    }

    #[export_methods(name = "Num")]
    impl Num {
        #[init]
        pub fn create(val: f32) -> Num {
            Num { val }
        }

        #[getter]
        pub fn val(&self) -> f32 {
            self.val
        }

        #[setter]
        pub fn set_val(&mut self, v: f32) {
            self.val = v;
        }

        #[op(Self / Self)]
        pub fn div(&self, other: &Num) -> Option<f32> {
            if other.val == 0.0 {
                None
            } else {
                Some(self.val / other.val)
            }
        }
    }

    anvyx_lang::provider!(types: [Num]);

    #[test]
    fn op_option_some() {
        let id_a = __ANVYX_STORE_NUM.with(|s| s.borrow_mut().insert(Num { val: 10.0 }));
        let id_b = __ANVYX_STORE_NUM.with(|s| s.borrow_mut().insert(Num { val: 2.0 }));
        let (_, handler) = __anvyx_method_Num___op_div__Num();
        let result = handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
        let Value::Enum(ref e) = result else {
            panic!("expected Enum");
        };
        assert_eq!(e.type_id, OPTION_TYPE_ID);
        assert_eq!(e.variant, 1);
        assert_eq!(e.fields, vec![Value::Float(5.0)]);
        __ANVYX_STORE_NUM.with(|s| {
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
        });
    }

    #[test]
    fn op_option_none() {
        let id_a = __ANVYX_STORE_NUM.with(|s| s.borrow_mut().insert(Num { val: 10.0 }));
        let id_b = __ANVYX_STORE_NUM.with(|s| s.borrow_mut().insert(Num { val: 0.0 }));
        let (_, handler) = __anvyx_method_Num___op_div__Num();
        let result = handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
        let Value::Enum(ref e) = result else {
            panic!("expected Enum");
        };
        assert_eq!(e.type_id, OPTION_TYPE_ID);
        assert_eq!(e.variant, 0);
        assert!(e.fields.is_empty());
        __ANVYX_STORE_NUM.with(|s| {
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
        });
    }

    #[test]
    fn op_option_metadata() {
        let ops = __ANVYX_OPS_DECL_NUM();
        let div = ops.iter().find(|o| o.op == "Div").unwrap();
        assert_eq!(div.ret, "Option<float>");
        assert_eq!(div.rhs, Some("Num"));
        assert!(div.lhs.is_none());
    }

    #[test]
    fn op_option_handler_key() {
        let (key, _) = __anvyx_method_Num___op_div__Num();
        assert_eq!(key, "Num::__op_div__Num");
    }
}

// -- #[op] returning Result<f32, RuntimeError> --

mod op_result_return_tests {
    use anvyx_lang::{RuntimeError, Value, export_methods, export_type};

    use super::extern_handle;

    #[derive(Clone)]
    #[export_type(name = "SafeNum")]
    pub struct SafeNum {
        pub val: f32,
    }

    #[export_methods(name = "SafeNum")]
    impl SafeNum {
        #[init]
        pub fn create(val: f32) -> SafeNum {
            SafeNum { val }
        }

        #[getter]
        pub fn val(&self) -> f32 {
            self.val
        }

        #[setter]
        pub fn set_val(&mut self, v: f32) {
            self.val = v;
        }

        #[op(Self / Self)]
        pub fn div(&self, other: &SafeNum) -> Result<f32, RuntimeError> {
            if other.val == 0.0 {
                Err(RuntimeError::new("division by zero"))
            } else {
                Ok(self.val / other.val)
            }
        }
    }

    anvyx_lang::provider!(types: [SafeNum]);

    #[test]
    fn op_result_ok() {
        let id_a = __ANVYX_STORE_SAFENUM.with(|s| s.borrow_mut().insert(SafeNum { val: 10.0 }));
        let id_b = __ANVYX_STORE_SAFENUM.with(|s| s.borrow_mut().insert(SafeNum { val: 4.0 }));
        let (_, handler) = __anvyx_method_SafeNum___op_div__SafeNum();
        let result = handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
        assert_eq!(result, Value::Float(2.5));
        __ANVYX_STORE_SAFENUM.with(|s| {
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
        });
    }

    #[test]
    fn op_result_err() {
        let id_a = __ANVYX_STORE_SAFENUM.with(|s| s.borrow_mut().insert(SafeNum { val: 10.0 }));
        let id_b = __ANVYX_STORE_SAFENUM.with(|s| s.borrow_mut().insert(SafeNum { val: 0.0 }));
        let (_, handler) = __anvyx_method_SafeNum___op_div__SafeNum();
        let result = handler(vec![extern_handle(id_a), extern_handle(id_b)]);
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("division by zero"));
        __ANVYX_STORE_SAFENUM.with(|s| {
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
        });
    }

    #[test]
    fn op_result_metadata() {
        let ops = __ANVYX_OPS_DECL_SAFENUM();
        let div = ops.iter().find(|o| o.op == "Div").unwrap();
        // Result wrapper unwraps in metadata — ret is the Ok inner type
        assert_eq!(div.ret, "float");
        assert_eq!(div.rhs, Some("SafeNum"));
        assert!(div.lhs.is_none());
    }
}

// -- Method returning Option<ExternType> --

mod method_option_extern_return_tests {
    use anvyx_lang::{OPTION_TYPE_ID, Value, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Item")]
    pub struct Item {
        pub val: f32,
    }

    #[export_methods]
    impl Item {}

    #[derive(Clone)]
    #[export_type(name = "Bag")]
    pub struct Bag {
        pub item_val: f32,
        pub has_item: bool,
    }

    #[export_methods(name = "Bag")]
    impl Bag {
        #[init]
        pub fn create(item_val: f32, has_item: bool) -> Bag {
            Bag { item_val, has_item }
        }

        #[getter]
        pub fn item_val(&self) -> f32 {
            self.item_val
        }

        #[setter]
        pub fn set_item_val(&mut self, v: f32) {
            self.item_val = v;
        }

        pub fn take_item(&self) -> Option<Item> {
            if self.has_item {
                Some(Item { val: self.item_val })
            } else {
                None
            }
        }
    }

    anvyx_lang::provider!(types: [Item, Bag]);

    #[test]
    fn method_option_extern_some() {
        let externs = anvyx_externs();
        let bag = externs["Bag::__init__"](vec![Value::Float(42.0), Value::Bool(true)]).unwrap();
        let Value::ExternHandle(ref bag_ehd) = bag else {
            panic!("expected ExternHandle for Bag");
        };
        let result = externs["Bag::take_item"](vec![Value::ExternHandle(bag_ehd.clone())]).unwrap();
        let Value::Enum(ref e) = result else {
            panic!("expected Enum");
        };
        assert_eq!(e.type_id, OPTION_TYPE_ID);
        assert_eq!(e.variant, 1);
        let Value::ExternHandle(ref item_ehd) = e.fields[0] else {
            panic!("expected ExternHandle inside Option");
        };
        __ANVYX_STORE_ITEM.with(|s| {
            let store = s.borrow();
            let item = store.borrow(item_ehd.id).unwrap();
            assert_eq!(item.val, 42.0);
        });
        // bag and result (containing item handle) auto-cleanup on drop
    }

    #[test]
    fn method_option_extern_none() {
        let externs = anvyx_externs();
        let bag = externs["Bag::__init__"](vec![Value::Float(0.0), Value::Bool(false)]).unwrap();
        let Value::ExternHandle(ref bag_ehd) = bag else {
            panic!("expected ExternHandle for Bag");
        };
        let result = externs["Bag::take_item"](vec![Value::ExternHandle(bag_ehd.clone())]).unwrap();
        let Value::Enum(ref e) = result else {
            panic!("expected Enum");
        };
        assert_eq!(e.type_id, OPTION_TYPE_ID);
        assert_eq!(e.variant, 0);
        assert!(e.fields.is_empty());
        // bag auto-cleanup on drop
    }

    #[test]
    fn method_option_extern_metadata() {
        let methods = __ANVYX_METHODS_DECL_BAG();
        let take_item = methods.iter().find(|m| m.name == "take_item").unwrap();
        assert_eq!(take_item.ret, "Option<Item>");
        assert_eq!(take_item.receiver, "self");
        assert_eq!(take_item.params, &[]);
    }
}

// -- Method returning Result<ExternType, RuntimeError> --

mod method_result_extern_return_tests {
    use anvyx_lang::{RuntimeError, Value, export_methods, export_type};

    #[derive(Clone)]
    #[export_type(name = "Product")]
    pub struct Product {
        pub val: f32,
    }

    #[export_methods]
    impl Product {}

    #[derive(Clone)]
    #[export_type(name = "Factory")]
    pub struct Factory {
        pub rate: f32,
    }

    #[export_methods(name = "Factory")]
    impl Factory {
        #[init]
        pub fn create(rate: f32) -> Factory {
            Factory { rate }
        }

        #[getter]
        pub fn rate(&self) -> f32 {
            self.rate
        }

        #[setter]
        pub fn set_rate(&mut self, v: f32) {
            self.rate = v;
        }

        pub fn produce(&self, quantity: f32) -> Result<Product, RuntimeError> {
            if quantity <= 0.0 {
                Err(RuntimeError::new("quantity must be positive"))
            } else {
                Ok(Product {
                    val: self.rate * quantity,
                })
            }
        }
    }

    anvyx_lang::provider!(types: [Product, Factory]);

    #[test]
    fn method_result_extern_ok() {
        let externs = anvyx_externs();
        let factory = externs["Factory::__init__"](vec![Value::Float(2.5)]).unwrap();
        let Value::ExternHandle(ref factory_ehd) = factory else {
            panic!("expected ExternHandle for Factory");
        };
        let result = externs["Factory::produce"](vec![
            Value::ExternHandle(factory_ehd.clone()),
            Value::Float(4.0),
        ])
        .unwrap();
        let Value::ExternHandle(ref product_ehd) = result else {
            panic!("expected ExternHandle for Product");
        };
        __ANVYX_STORE_PRODUCT.with(|s| {
            let store = s.borrow();
            let p = store.borrow(product_ehd.id).unwrap();
            assert_eq!(p.val, 10.0);
        });
        // factory and result auto-cleanup on drop
    }

    #[test]
    fn method_result_extern_err() {
        let externs = anvyx_externs();
        let factory = externs["Factory::__init__"](vec![Value::Float(1.0)]).unwrap();
        let Value::ExternHandle(ref factory_ehd) = factory else {
            panic!("expected ExternHandle for Factory");
        };
        let result = externs["Factory::produce"](vec![
            Value::ExternHandle(factory_ehd.clone()),
            Value::Float(-1.0),
        ]);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .message
                .contains("quantity must be positive")
        );
        // factory still valid after error — auto-cleanup on drop
    }

    #[test]
    fn method_result_extern_metadata() {
        let methods = __ANVYX_METHODS_DECL_FACTORY();
        let produce = methods.iter().find(|m| m.name == "produce").unwrap();
        // Result wrapper unwraps in metadata — ret is the Ok inner type
        assert_eq!(produce.ret, "Product");
        assert_eq!(produce.receiver, "self");
        assert_eq!(produce.params, &[("quantity", "float")]);
    }
}

// -- #[op] returning Option<ExternType> --

mod op_option_extern_return_tests {
    use anvyx_lang::{OPTION_TYPE_ID, Value, export_methods, export_type};

    use super::extern_handle;

    #[derive(Clone)]
    #[export_type(name = "Vec2")]
    pub struct OptVec2 {
        pub x: f32,
        pub y: f32,
    }

    #[export_methods(name = "Vec2")]
    impl OptVec2 {
        #[init]
        pub fn create(x: f32, y: f32) -> OptVec2 {
            OptVec2 { x, y }
        }

        #[getter]
        pub fn x(&self) -> f32 {
            self.x
        }

        #[setter]
        pub fn set_x(&mut self, v: f32) {
            self.x = v;
        }

        #[getter]
        pub fn y(&self) -> f32 {
            self.y
        }

        #[setter]
        pub fn set_y(&mut self, v: f32) {
            self.y = v;
        }

        #[op(Self / float)]
        pub fn div_scalar(&self, s: f32) -> Option<OptVec2> {
            if s == 0.0 {
                None
            } else {
                Some(OptVec2 {
                    x: self.x / s,
                    y: self.y / s,
                })
            }
        }
    }

    anvyx_lang::provider!(types: [OptVec2]);

    #[test]
    fn op_option_extern_some() {
        let id =
            __ANVYX_STORE_OPTVEC2.with(|s| s.borrow_mut().insert(OptVec2 { x: 10.0, y: 20.0 }));
        let (_, handler) = __anvyx_method_OptVec2___op_div__float();
        let result = handler(vec![extern_handle(id), Value::Float(2.0)]).unwrap();
        let Value::Enum(ref e) = result else {
            panic!("expected Enum");
        };
        assert_eq!(e.type_id, OPTION_TYPE_ID);
        assert_eq!(e.variant, 1);
        let Value::ExternHandle(ref inner_ehd) = e.fields[0] else {
            panic!("expected ExternHandle inside Option");
        };
        __ANVYX_STORE_OPTVEC2.with(|s| {
            let store = s.borrow();
            let v = store.borrow(inner_ehd.id).unwrap();
            assert_eq!(v.x, 5.0);
            assert_eq!(v.y, 10.0);
        });
        __ANVYX_STORE_OPTVEC2.with(|s| s.borrow_mut().remove(id).unwrap());
        // result (containing inner_ehd) auto-cleanup on drop
    }

    #[test]
    fn op_option_extern_none() {
        let id =
            __ANVYX_STORE_OPTVEC2.with(|s| s.borrow_mut().insert(OptVec2 { x: 10.0, y: 20.0 }));
        let (_, handler) = __anvyx_method_OptVec2___op_div__float();
        let result = handler(vec![extern_handle(id), Value::Float(0.0)]).unwrap();
        let Value::Enum(ref e) = result else {
            panic!("expected Enum");
        };
        assert_eq!(e.type_id, OPTION_TYPE_ID);
        assert_eq!(e.variant, 0);
        assert!(e.fields.is_empty());
        __ANVYX_STORE_OPTVEC2.with(|s| s.borrow_mut().remove(id).unwrap());
    }

    #[test]
    fn op_option_extern_metadata() {
        let ops = __ANVYX_OPS_DECL_OPTVEC2();
        let div = ops.iter().find(|o| o.op == "Div").unwrap();
        assert_eq!(div.ret, "Option<Vec2>");
        assert_eq!(div.rhs, Some("float"));
        assert!(div.lhs.is_none());
    }
}

// -- #[op] returning Result<ExternType, RuntimeError> --

mod op_result_extern_return_tests {
    use anvyx_lang::{RuntimeError, Value, export_methods, export_type};

    use super::extern_handle;

    #[derive(Clone)]
    #[export_type(name = "Frac")]
    pub struct Frac {
        pub num: f32,
        pub den: f32,
    }

    #[export_methods(name = "Frac")]
    impl Frac {
        #[init]
        pub fn create(num: f32, den: f32) -> Frac {
            Frac { num, den }
        }

        #[getter]
        pub fn num(&self) -> f32 {
            self.num
        }

        #[setter]
        pub fn set_num(&mut self, v: f32) {
            self.num = v;
        }

        #[getter]
        pub fn den(&self) -> f32 {
            self.den
        }

        #[setter]
        pub fn set_den(&mut self, v: f32) {
            self.den = v;
        }

        #[op(Self + Self)]
        pub fn add(&self, other: &Frac) -> Result<Frac, RuntimeError> {
            let new_den = self.den * other.den;
            if new_den == 0.0 {
                return Err(RuntimeError::new("zero denominator in addition"));
            }
            Ok(Frac {
                num: self.num * other.den + other.num * self.den,
                den: new_den,
            })
        }
    }

    anvyx_lang::provider!(types: [Frac]);

    #[test]
    fn op_result_extern_ok() {
        // 1/2 + 1/3 = (1*3 + 1*2) / (2*3) = 5/6
        let id_a = __ANVYX_STORE_FRAC.with(|s| s.borrow_mut().insert(Frac { num: 1.0, den: 2.0 }));
        let id_b = __ANVYX_STORE_FRAC.with(|s| s.borrow_mut().insert(Frac { num: 1.0, den: 3.0 }));
        let (_, handler) = __anvyx_method_Frac___op_add__Frac();
        let result = handler(vec![extern_handle(id_a), extern_handle(id_b)]).unwrap();
        let Value::ExternHandle(ref frac_ehd) = result else {
            panic!("expected ExternHandle for Frac result");
        };
        __ANVYX_STORE_FRAC.with(|s| {
            let store = s.borrow();
            let v = store.borrow(frac_ehd.id).unwrap();
            assert_eq!(v.num, 5.0);
            assert_eq!(v.den, 6.0);
        });
        __ANVYX_STORE_FRAC.with(|s| {
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
        });
        // result auto-cleanup on drop
    }

    #[test]
    fn op_result_extern_err() {
        let id_a = __ANVYX_STORE_FRAC.with(|s| s.borrow_mut().insert(Frac { num: 1.0, den: 0.0 }));
        let id_b = __ANVYX_STORE_FRAC.with(|s| s.borrow_mut().insert(Frac { num: 1.0, den: 1.0 }));
        let (_, handler) = __anvyx_method_Frac___op_add__Frac();
        let result = handler(vec![extern_handle(id_a), extern_handle(id_b)]);
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("zero denominator"));
        // inputs are borrows — they remain in the store after error
        __ANVYX_STORE_FRAC.with(|s| {
            s.borrow_mut().remove(id_a).unwrap();
            s.borrow_mut().remove(id_b).unwrap();
        });
    }

    #[test]
    fn op_result_extern_metadata() {
        let ops = __ANVYX_OPS_DECL_FRAC();
        let add = ops.iter().find(|o| o.op == "Add").unwrap();
        // Result wrapper unwraps in metadata — ret is the Ok inner type
        assert_eq!(add.ret, "Frac");
        assert_eq!(add.rhs, Some("Frac"));
        assert!(add.lhs.is_none());
    }
}

// ── AnvyxFn callback tests ────────────────────────────────────────────────────

#[export_fn]
fn cb_apply(value: i64, cb: AnvyxFn<(i64,), i64>) -> Result<i64, RuntimeError> {
    cb.call(value)
}

#[test]
fn export_fn_anvyx_fn_decl_auto_derives_type() {
    let decl: ExternDecl = __ANVYX_DECL_CB_APPLY();
    assert_eq!(decl.name, "cb_apply");
    assert_eq!(decl.params, &[("value", "int"), ("cb", "fn(int) -> int")]);
    assert_eq!(decl.ret, "int");
}

#[test]
fn export_fn_anvyx_fn_generates_handler() {
    let (name, _handler) = __anvyx_export_cb_apply();
    assert_eq!(name, "cb_apply");
}

#[export_fn]
fn cb_void(cb: AnvyxFn<(i64,), ()>) -> Result<(), RuntimeError> {
    cb.call(0)
}

#[test]
fn export_fn_anvyx_fn_void_return_decl() {
    let decl: ExternDecl = __ANVYX_DECL_CB_VOID();
    assert_eq!(decl.params, &[("cb", "fn(int) -> void")]);
    assert_eq!(decl.ret, "void");
}

#[export_fn]
fn cb_multi(cb: AnvyxFn<(i64, f32, bool), String>) -> Result<String, RuntimeError> {
    cb.call(1, 2.0, true)
}

#[test]
fn export_fn_anvyx_fn_multi_arg_decl() {
    let decl: ExternDecl = __ANVYX_DECL_CB_MULTI();
    assert_eq!(decl.params, &[("cb", "fn(int, float, bool) -> string")]);
    assert_eq!(decl.ret, "string");
}

#[export_fn]
fn cb_zero(cb: AnvyxFn<(), i64>) -> Result<i64, RuntimeError> {
    cb.call()
}

#[test]
fn export_fn_anvyx_fn_zero_arg_decl() {
    let decl: ExternDecl = __ANVYX_DECL_CB_ZERO();
    assert_eq!(decl.params, &[("cb", "fn() -> int")]);
    assert_eq!(decl.ret, "int");
}

#[export_fn(params(cb = "fn(Point) -> float"))]
fn cb_override(cb: AnvyxFn<(i64,), f32>) -> Result<f32, RuntimeError> {
    cb.call(0)
}

#[test]
fn export_fn_anvyx_fn_explicit_override_wins() {
    let decl: ExternDecl = __ANVYX_DECL_CB_OVERRIDE();
    assert_eq!(decl.params, &[("cb", "fn(Point) -> float")]);
}

#[export_fn]
fn cb_mixed(a: i64, cb: AnvyxFn<(i64,), bool>, name: String) -> Result<bool, RuntimeError> {
    drop(name);
    cb.call(a)
}

#[test]
fn export_fn_anvyx_fn_mixed_params_decl() {
    let decl: ExternDecl = __ANVYX_DECL_CB_MIXED();
    assert_eq!(
        decl.params,
        &[("a", "int"), ("cb", "fn(int) -> bool"), ("name", "string")]
    );
}

#[export_fn]
fn cb_double(cb: AnvyxFn<(f64,), f64>) -> Result<f64, RuntimeError> {
    cb.call(1.0)
}

#[test]
fn export_fn_anvyx_fn_double_decl() {
    let decl: ExternDecl = __ANVYX_DECL_CB_DOUBLE();
    assert_eq!(decl.params, &[("cb", "fn(double) -> double")]);
}

// ── ExternHandle<T> tests ─────────────────────────────────────────────────────

#[export_fn]
fn take_handle(h: ExternHandle<Color>) -> i64 {
    h.with_borrow(|c| i64::from(c.0)).unwrap()
}

#[test]
fn export_fn_extern_handle_param_decl() {
    let decl: ExternDecl = __ANVYX_DECL_TAKE_HANDLE();
    assert_eq!(decl.params, &[("h", "Color")]);
    assert_eq!(decl.ret, "int");
}

#[export_fn]
fn make_handle(r: i64, g: i64, b: i64) -> ExternHandle<Color> {
    ExternHandle::new(Color(r as u8, g as u8, b as u8))
}

#[test]
fn export_fn_extern_handle_return_decl() {
    let decl: ExternDecl = __ANVYX_DECL_MAKE_HANDLE();
    assert_eq!(decl.ret, "Color");
}

#[export_fn]
fn identity_handle(h: ExternHandle<Color>) -> ExternHandle<Color> {
    h
}

#[test]
fn export_fn_extern_handle_roundtrip() {
    let handle = ExternHandle::new(Color(10, 20, 30));
    let value = handle.into_anvyx();
    let (_, handler) = __anvyx_export_identity_handle();
    let result = handler(vec![value]).unwrap();
    assert!(matches!(result, Value::ExternHandle(_)));
}

#[export_fn]
fn cb_with_handle(cb: AnvyxFn<(ExternHandle<Color>,), ()>) {
    drop(cb);
}

#[test]
fn export_fn_callback_extern_handle_decl() {
    let decl: ExternDecl = __ANVYX_DECL_CB_WITH_HANDLE();
    assert_eq!(decl.params, &[("cb", "fn(Color) -> void")]);
}

#[export_fn]
fn cb_returns_handle(
    cb: AnvyxFn<(i64,), ExternHandle<Color>>,
) -> Result<ExternHandle<Color>, RuntimeError> {
    cb.call(0)
}

#[test]
fn export_fn_callback_extern_handle_return_decl() {
    let decl: ExternDecl = __ANVYX_DECL_CB_RETURNS_HANDLE();
    assert_eq!(decl.params, &[("cb", "fn(int) -> Color")]);
    assert_eq!(decl.ret, "Color");
}

#[export_fn(params(cb = "fn(Sprite) -> void"))]
fn cb_override_handle(cb: AnvyxFn<(ExternHandle<SpriteData>,), ()>) {
    drop(cb);
}

#[test]
fn export_fn_callback_extern_handle_explicit_override() {
    let decl: ExternDecl = __ANVYX_DECL_CB_OVERRIDE_HANDLE();
    assert_eq!(decl.params, &[("cb", "fn(Sprite) -> void")]);
}
