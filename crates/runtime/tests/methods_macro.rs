#![allow(dead_code)]

use anvyx_runtime::{
    AnvInitField, AnvRef, AnvString, AnvyxEnum, AnvyxInline, AnvyxRef, BinaryOp, CallbackEscape,
    CallbackThread, Ctx, EscapingLambda, ExternBindingOp, ExternMemberSelector, ExternOperator,
    ExternTypeExpr, Heap, ReceiverMode, RuntimeError, RuntimeResult, RustAbiSupport, RustParamAbi,
    RustReturnAbi, RustWrapperCtx, ScopedLambda, methods,
};

#[derive(AnvyxInline)]
#[anvyx(name = "Vector2")]
struct DerivedVec2 {
    #[anvyx(field)]
    x: f64,
}

#[methods(name = "Vector2")]
impl DerivedVec2 {
    #[anvyx(init)]
    pub fn new(x: f64) -> Self {
        Self { x }
    }

    pub fn x(&self) -> f64 {
        self.x
    }
}

#[derive(AnvyxRef)]
struct GenericResource<'cx> {
    id: i64,
    _brand: std::marker::PhantomData<&'cx ()>,
}

#[methods]
impl GenericResource<'_> {
    pub fn id(&self) -> i64 {
        self.id
    }
}

struct CallbackOps {
    id: i64,
}

#[methods]
impl CallbackOps {
    pub fn each(f: ScopedLambda<'_, '_, (i64,), ()>) -> RuntimeResult<()> {
        let result = f.call(1);
        let _ = std::hint::black_box(f);
        result
    }

    pub fn retain(f: EscapingLambda<(i64,), ()>) {
        drop(f);
    }

    pub fn retain_on_self(&self, f: EscapingLambda<(i64,), ()>) -> i64 {
        drop(f);
        self.id
    }
}

struct CtxBox {
    value: i64,
}

#[methods]
impl CtxBox {
    #[anvyx(ctx)]
    pub fn add(&self, ctx: &mut Ctx<'_, '_>, value: i64) -> i64 {
        let _ = ctx.heap();
        self.value + value
    }
}

#[derive(AnvyxEnum)]
enum InitError {
    Bad,
}

struct DefaultInit;

#[methods]
impl DefaultInit {
    #[anvyx(init)]
    pub fn new() -> Self {
        Self
    }
}

struct PresenceInit {
    value: i64,
}

#[methods]
impl PresenceInit {
    #[anvyx(init)]
    pub fn new(value: AnvInitField<AnvString>) -> Self {
        Self {
            value: match value {
                AnvInitField::Provided(value) => value.len() as i64,
                AnvInitField::Omitted => 0,
            },
        }
    }
}

struct FallibleInit;

#[methods]
impl FallibleInit {
    #[anvyx(init)]
    pub fn try_new(ok: bool) -> Result<Self, InitError> {
        ok.then_some(Self).ok_or(InitError::Bad)
    }
}

struct RuntimeResultInit;

#[methods]
impl RuntimeResultInit {
    #[anvyx(init)]
    pub fn try_new(ok: bool) -> RuntimeResult<Self> {
        if ok {
            Ok(Self)
        } else {
            Err(RuntimeError::new("bad init"))
        }
    }
}

#[derive(AnvyxRef)]
struct OwnerReturns {
    value: i64,
}

#[methods]
impl OwnerReturns {
    pub fn duplicate(&self) -> Self {
        Self { value: self.value }
    }

    pub fn make(value: i64) -> Self {
        Self { value }
    }

    pub fn maybe(&self, ok: bool) -> Option<Self> {
        ok.then_some(Self { value: self.value })
    }

    pub fn visible(&self, ok: bool) -> Result<Self, InitError> {
        ok.then_some(Self { value: self.value })
            .ok_or(InitError::Bad)
    }

    pub fn hidden(&self) -> RuntimeResult<Self> {
        if self.value >= 0 {
            Ok(Self { value: self.value })
        } else {
            Err(RuntimeError::new("negative"))
        }
    }

    #[anvyx(ctx)]
    pub fn explicit_ref<'cx>(&self, ctx: &mut Ctx<'cx, '_>) -> AnvRef<'cx, Self> {
        let _ = (self.value, ctx.heap());
        panic!("metadata only")
    }
}

struct RenamedOwnerReturn;

#[methods(name = "ExportedOwner")]
impl RenamedOwnerReturn {
    pub fn duplicate(&self) -> Self {
        let _ = self;
        Self
    }

    pub fn make() -> Self {
        Self
    }
}

#[test]
fn method_owner_returns_use_export_name_and_owned_abi() {
    let export = __anvyx_methods_ownerreturns();
    let owner = ExternTypeExpr::Named {
        module: None,
        name: "OwnerReturns".to_string(),
        args: vec![],
    };

    let duplicate = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Method(name) if name == "duplicate"))
        .unwrap();
    assert_eq!(export.descriptor.methods[0].signature.ret, owner);
    assert!(matches!(
        &duplicate.abi.ret,
        RustReturnAbi::OwnedNamed(ExternTypeExpr::Named { name, .. }) if name == "OwnerReturns"
    ));

    let make = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Static(name) if name == "make"))
        .unwrap();
    assert!(matches!(
        &make.abi.ret,
        RustReturnAbi::OwnedNamed(ExternTypeExpr::Named { name, .. }) if name == "OwnerReturns"
    ));

    let maybe = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Method(name) if name == "maybe"))
        .unwrap();
    assert!(matches!(
        &maybe.abi.ret,
        RustReturnAbi::Option(inner)
            if matches!(inner.as_ref(), RustReturnAbi::OwnedNamed(ExternTypeExpr::Named { name, .. }) if name == "OwnerReturns")
    ));

    let visible = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Method(name) if name == "visible"))
        .unwrap();
    assert!(matches!(
        &visible.abi.ret,
        RustReturnAbi::Result(ok, _)
            if matches!(ok.as_ref(), RustReturnAbi::OwnedNamed(ExternTypeExpr::Named { name, .. }) if name == "OwnerReturns")
    ));

    let hidden = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Method(name) if name == "hidden"))
        .unwrap();
    assert!(hidden.abi.fallible);
    assert!(matches!(
        &hidden.abi.ret,
        RustReturnAbi::OwnedNamed(ExternTypeExpr::Named { name, .. }) if name == "OwnerReturns"
    ));
}

#[test]
fn explicit_owner_ref_return_stays_value_abi() {
    let export = __anvyx_methods_ownerreturns();
    let binding = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Method(name) if name == "explicit_ref"))
        .unwrap();

    assert!(matches!(
        &binding.abi.ret,
        RustReturnAbi::Value(ExternTypeExpr::Named { name, .. }) if name == "OwnerReturns"
    ));
}

#[test]
fn renamed_owner_returns_use_export_name() {
    let export = __anvyx_methods_renamedownerreturn();

    assert_eq!(export.descriptor.name, "ExportedOwner");
    assert!(matches!(
        &export.descriptor.methods[0].signature.ret,
        ExternTypeExpr::Named { name, .. } if name == "ExportedOwner"
    ));
    assert!(matches!(
        &export.bindings[0].abi.ret,
        RustReturnAbi::OwnedNamed(ExternTypeExpr::Named { name, .. }) if name == "ExportedOwner"
    ));
}

#[test]
fn scoped_lambda_method_uses_callback_descriptor_and_no_hidden_ctx() {
    let export = __anvyx_methods_callbackops();
    let method = export
        .descriptor
        .statics
        .iter()
        .find(|method| method.name == "each")
        .unwrap();
    let ExternTypeExpr::Callback(callback) = &method.signature.params[0].ty else {
        panic!("expected callback descriptor");
    };

    assert_eq!(callback.params[0].ty, ExternTypeExpr::Int);
    assert_eq!(callback.params[0].escape, CallbackEscape::NonEscaping);
    assert_eq!(*callback.ret, ExternTypeExpr::Void);
    assert_eq!(callback.policy.thread, CallbackThread::SameThread);
    assert_eq!(
        export.bindings
            .iter()
            .find(|binding| matches!(&binding.selector, ExternMemberSelector::Static(name) if name == "each"))
            .unwrap()
            .abi
            .support,
        RustAbiSupport::NeedsWrapperConversion
    );
    let binding = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Static(name) if name == "each"))
        .unwrap();
    assert_eq!(binding.abi.ctx, RustWrapperCtx::None);
    assert!(matches!(
        binding.abi.params[0],
        RustParamAbi::ScopedLambda(_)
    ));
}

#[test]
fn escaping_lambda_static_method_uses_escaping_callback_descriptor() {
    let export = __anvyx_methods_callbackops();
    let method = export
        .descriptor
        .statics
        .iter()
        .find(|method| method.name == "retain")
        .unwrap();
    let ExternTypeExpr::Callback(callback) = &method.signature.params[0].ty else {
        panic!("expected callback descriptor");
    };
    let binding = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Static(name) if name == "retain"))
        .unwrap();

    assert_eq!(method.signature.params[0].escape, CallbackEscape::Escaping);
    assert_eq!(callback.policy.escape, CallbackEscape::Escaping);
    assert!(matches!(
        binding.abi.params[0],
        RustParamAbi::EscapingLambda(_)
    ));
    assert_eq!(binding.abi.support, RustAbiSupport::NeedsWrapperConversion);
    assert_eq!(binding.abi.ctx, RustWrapperCtx::None);
}

#[test]
fn escaping_lambda_method_receiver_uses_escaping_callback_descriptor() {
    let export = __anvyx_methods_callbackops();
    let method = export
        .descriptor
        .methods
        .iter()
        .find(|method| method.name == "retain_on_self")
        .unwrap();
    let ExternTypeExpr::Callback(callback) = &method.signature.params[0].ty else {
        panic!("expected callback descriptor");
    };
    let binding = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Method(name) if name == "retain_on_self"))
        .unwrap();

    assert_eq!(method.signature.params[0].escape, CallbackEscape::Escaping);
    assert_eq!(callback.policy.escape, CallbackEscape::Escaping);
    assert!(matches!(binding.abi.params[0], RustParamAbi::Borrow(_)));
    assert!(matches!(
        binding.abi.params[1],
        RustParamAbi::EscapingLambda(_)
    ));
    assert_eq!(binding.abi.support, RustAbiSupport::NeedsWrapperConversion);
    assert_eq!(binding.abi.ctx, RustWrapperCtx::None);
}

#[test]
fn init_supports_visible_result_and_runtime_result_metadata() {
    let default = __anvyx_methods_defaultinit();
    let default_init = default.descriptor.init.as_ref().unwrap();
    assert!(default_init.params.is_empty());
    assert!(default_init.field_init.is_empty());
    assert!(default_init.presence_init.is_empty());

    let presence = __anvyx_methods_presenceinit();
    let presence_init = presence.descriptor.init.as_ref().unwrap();
    assert_eq!(presence_init.params.len(), 1);
    assert_eq!(presence_init.params[0].ty, ExternTypeExpr::String);
    assert!(presence_init.field_init.is_empty());
    assert_eq!(presence_init.presence_init, ["value"]);
    let presence_binding = presence
        .bindings
        .iter()
        .find(|binding| matches!(binding.selector, ExternMemberSelector::Init))
        .unwrap();
    assert_eq!(
        presence_binding.abi.params[0],
        RustParamAbi::InitField(Box::new(RustParamAbi::Value(ExternTypeExpr::String)))
    );

    let visible = __anvyx_methods_fallibleinit();
    let visible_init = visible.descriptor.init.as_ref().unwrap();
    assert_eq!(visible_init.params.len(), 1);
    assert_eq!(visible_init.field_init, ["ok"]);
    assert!(!visible_init.effects.fallible);
    assert!(matches!(visible_init.ret, ExternTypeExpr::Result(_, _)));
    assert!(matches!(
        visible
            .bindings
            .iter()
            .find(|binding| matches!(binding.selector, ExternMemberSelector::Init))
            .unwrap()
            .abi
            .ret,
        RustReturnAbi::Result(_, _)
    ));

    let hidden = __anvyx_methods_runtimeresultinit();
    let hidden_init = hidden.descriptor.init.as_ref().unwrap();
    assert_eq!(
        hidden_init.ret,
        ExternTypeExpr::Named {
            module: None,
            name: "RuntimeResultInit".to_string(),
            args: vec![],
        }
    );
    assert_eq!(hidden_init.field_init, ["ok"]);
    assert!(hidden_init.effects.fallible);
    let hidden_binding = hidden
        .bindings
        .iter()
        .find(|binding| matches!(binding.selector, ExternMemberSelector::Init))
        .unwrap();
    assert!(hidden_binding.abi.fallible);
    assert!(matches!(
        hidden_binding.abi.ret,
        RustReturnAbi::OwnedNamed(_)
    ));
}

#[test]
fn ctx_method_hides_ctx_from_metadata_and_wrapper_uses_ctx_first() {
    let export = __anvyx_methods_ctxbox();

    assert_eq!(export.descriptor.methods[0].signature.params.len(), 1);
    assert_eq!(
        export.bindings[0].abi.params,
        [
            RustParamAbi::Borrow(ExternTypeExpr::Named {
                module: None,
                name: "CtxBox".to_string(),
                args: vec![],
            }),
            RustParamAbi::Value(ExternTypeExpr::Int),
        ]
    );
    Heap::scope(|heap| {
        let mut ctx = Ctx::new(heap);
        let receiver = CtxBox { value: 40 };
        assert_eq!(
            __anvyx_methods_native_ctxbox::add(&mut ctx, &receiver, 2),
            42
        );
    });
}

struct Vec2 {
    x: f64,
    y: f64,
}

impl Vec2 {
    fn into_parts(self) -> (f64, f64) {
        (self.x, self.y)
    }
}

#[methods]
impl Vec2 {
    /// Length squared.
    pub fn len2(&self) -> f64 {
        self.x * self.x + self.y * self.y
    }

    pub fn reset(&mut self) {
        self.x = 0.0;
        self.y = 0.0;
    }

    pub fn unit() -> f64 {
        1.0
    }

    #[anvyx(init)]
    pub fn new(x: f64, y: f64) -> Self {
        Self { x, y }
    }

    #[anvyx(getter)]
    pub fn length(&self) -> f64 {
        self.len2()
    }

    #[anvyx(setter)]
    pub fn set_length(&mut self, value: f64) {
        self.x = value;
    }

    #[anvyx(op(Self + Self))]
    pub fn add(&self, rhs: Self) -> Self {
        let (x, y) = rhs.into_parts();
        Self {
            x: self.x + x,
            y: self.y + y,
        }
    }

    #[anvyx(op(Self <= f64))]
    pub fn le(&self, rhs: f64) -> bool {
        self.x <= rhs
    }

    #[anvyx(op(f64 * Self))]
    pub fn scale_left(&self, lhs: f64) -> f64 {
        lhs * self.x
    }
}

#[test]
fn methods_merge_into_generic_ref_descriptor() {
    let export = __anvyx_export_genericresource();

    assert_eq!(export.descriptor.name, "GenericResource");
    assert_eq!(export.descriptor.methods[0].name, "id");
}

#[test]
fn methods_merge_into_derive_owned_type_descriptor() {
    let export = __anvyx_export_derivedvec2();

    assert_eq!(export.descriptor.name, "Vector2");
    assert_eq!(export.descriptor.fields[0].name, "x");
    assert_eq!(export.descriptor.methods[0].name, "x");
    assert_eq!(export.descriptor.rep, anvyx_runtime::ExternRep::Inline);
    let descriptor_init = export.descriptor.init.as_ref().unwrap();
    assert_eq!(descriptor_init.field_init, ["x"]);
    assert!(matches!(
        &descriptor_init.ret,
        ExternTypeExpr::Named { name, .. } if name == "Vector2"
    ));
    let init = export
        .bindings
        .iter()
        .find(|binding| matches!(binding.selector, ExternMemberSelector::Init))
        .unwrap();
    assert!(matches!(
        &init.abi.ret,
        RustReturnAbi::OwnedNamed(ExternTypeExpr::Named { name, .. }) if name == "Vector2"
    ));
}

#[derive(AnvyxInline)]
#[anvyx(name = "NamedOps")]
struct RenamedOps {
    #[anvyx(field)]
    x: f64,
}

impl RenamedOps {
    fn into_x(self) -> f64 {
        self.x
    }
}

#[methods]
impl RenamedOps {
    #[anvyx(op(Self + Self))]
    pub fn add(&self, rhs: RenamedOps) -> RenamedOps {
        RenamedOps {
            x: self.x + rhs.into_x(),
        }
    }
}

#[test]
fn self_operator_uses_derive_owned_type_name() {
    let export = __anvyx_export_renamedops();
    let op = &export.descriptor.operators[0];

    assert!(matches!(
        &op.signature.params[0].ty,
        ExternTypeExpr::Named { name, .. } if name == "NamedOps"
    ));
    assert!(matches!(
        &op.signature.ret,
        ExternTypeExpr::Named { name, .. } if name == "NamedOps"
    ));
    let binding = export
        .bindings
        .iter()
        .find(|binding| {
            matches!(
                &binding.selector,
                ExternMemberSelector::Operator(ExternOperator::Binary {
                    op: BinaryOp::Add,
                    ..
                })
            )
        })
        .unwrap();
    assert!(matches!(
        &binding.abi.params[1],
        RustParamAbi::OwnedNamed(ExternTypeExpr::Named { name, .. }) if name == "NamedOps"
    ));
}

#[test]
fn methods_descriptor_covers_member_roles() {
    let export = __anvyx_methods_vec2();

    assert_eq!(export.descriptor.name, "Vec2");
    assert_eq!(export.descriptor.methods.len(), 2);
    assert_eq!(export.descriptor.methods[0].name, "len2");
    assert_eq!(
        export.descriptor.methods[0].doc.as_deref(),
        Some("Length squared.")
    );
    assert_eq!(export.descriptor.methods[0].receiver, ReceiverMode::Shared);
    assert_eq!(export.descriptor.methods[1].receiver, ReceiverMode::Mutable);
    assert_eq!(export.descriptor.statics[0].name, "unit");
    let init = export.descriptor.init.as_ref().unwrap();
    assert_eq!(init.params.len(), 2);
    assert_eq!(init.field_init, ["x", "y"]);
    assert_eq!(export.descriptor.fields.len(), 1);
    assert!(export.descriptor.fields.iter().all(|field| field.computed));
    assert!(export.descriptor.fields[0].readable);
    assert!(export.descriptor.fields[0].writable);
    assert_eq!(export.descriptor.operators.len(), 3);
    assert!(export.descriptor.operators.iter().any(|op| op.op
        == ExternOperator::Binary {
            op: BinaryOp::Add,
            self_on_right: false,
        }));
    assert!(export.descriptor.operators.iter().any(|op| op.op
        == ExternOperator::Binary {
            op: BinaryOp::LessThanEq,
            self_on_right: false,
        }));
    assert!(export.descriptor.operators.iter().any(|op| op.op
        == ExternOperator::Binary {
            op: BinaryOp::Mul,
            self_on_right: true,
        }));
}

#[derive(AnvyxInline)]
struct DuplicateOps {
    x: i64,
}

#[methods]
impl DuplicateOps {
    #[anvyx(op(Self + i64))]
    pub fn add_i64(&self, rhs: i64) -> i64 {
        self.x + rhs
    }

    #[anvyx(op(Self + f64))]
    pub fn add_f64(&self, rhs: f64) -> f64 {
        self.x as f64 + rhs
    }
}

#[test]
#[should_panic(expected = "duplicate extern operator")]
fn duplicate_operator_fragments_are_rejected() {
    let _ = __anvyx_export_duplicateops();
}

#[test]
fn method_bindings_use_member_keys_and_operations() {
    let export = __anvyx_methods_vec2();

    let len2 = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Method(name) if name == "len2"))
        .unwrap();
    assert_eq!(len2.operation, ExternBindingOp::Call);
    let owner = ExternTypeExpr::Named {
        module: None,
        name: "Vec2".to_string(),
        args: vec![],
    };
    assert_eq!(len2.abi.params[0], RustParamAbi::Borrow(owner.clone()));
    let reset = export
        .bindings
        .iter()
        .find(|binding| matches!(&binding.selector, ExternMemberSelector::Method(name) if name == "reset"))
        .unwrap();
    assert_eq!(reset.abi.params[0], RustParamAbi::MutBorrow(owner.clone()));
    let setter = export
        .bindings
        .iter()
        .find(|binding| {
            matches!((&binding.selector, binding.operation), (ExternMemberSelector::Field(name), ExternBindingOp::Set) if name == "length")
        })
        .unwrap();
    assert_eq!(setter.abi.params[0], RustParamAbi::MutBorrow(owner));
    assert!(export.bindings.iter().any(|binding| matches!(
        (&binding.selector, binding.operation),
        (ExternMemberSelector::Static(name), ExternBindingOp::Call) if name == "unit"
    )));
    assert!(export.bindings.iter().any(|binding| matches!(
        (&binding.selector, binding.operation),
        (ExternMemberSelector::Field(name), ExternBindingOp::Get) if name == "length"
    )));
    assert!(export.bindings.iter().any(|binding| matches!(
        (&binding.selector, binding.operation),
        (ExternMemberSelector::Field(name), ExternBindingOp::Set) if name == "length"
    )));
    assert!(export.bindings.iter().any(|binding| matches!(
        (&binding.selector, binding.operation),
        (
            ExternMemberSelector::Operator(ExternOperator::Binary {
                op: BinaryOp::Add,
                ..
            }),
            ExternBindingOp::Call
        )
    )));
}
