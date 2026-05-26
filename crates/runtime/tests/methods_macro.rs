#![allow(dead_code)]

use anvyx_runtime::{
    AnvyxInline, BinaryOp, ExternBindingOp, ExternMemberSelector, ExternOperator, ReceiverMode,
    methods,
};

#[derive(AnvyxInline)]
#[anvyx(name = "Vector2")]
struct DerivedVec2 {
    #[anvyx(field)]
    x: f64,
}

#[methods(name = "Vector2")]
impl DerivedVec2 {
    pub fn x(&self) -> f64 {
        self.x
    }
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
    pub fn new() -> Self {
        Self { x: 0.0, y: 0.0 }
    }

    #[anvyx(getter)]
    pub fn length(&self) -> f64 {
        self.len2()
    }

    #[anvyx(setter)]
    pub fn magnitude(&mut self, value: f64) {
        self.x = value;
    }

    #[anvyx(op(Self + Self))]
    pub fn add(&self, rhs: Vec2) -> Vec2 {
        let (x, y) = rhs.into_parts();
        Vec2 {
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
fn methods_merge_into_derive_owned_type_descriptor() {
    let export = __anvyx_export_derivedvec2();

    assert_eq!(export.descriptor.name, "Vector2");
    assert_eq!(export.descriptor.fields[0].name, "x");
    assert_eq!(export.descriptor.methods[0].name, "x");
    assert_eq!(export.descriptor.rep, anvyx_runtime::ExternRep::Inline);
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
        anvyx_runtime::ExternTypeExpr::Named { name, .. } if name == "NamedOps"
    ));
    assert!(matches!(
        &op.signature.ret,
        anvyx_runtime::ExternTypeExpr::Named { name, .. } if name == "NamedOps"
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
    assert!(export.descriptor.init.is_some());
    assert_eq!(export.descriptor.fields.len(), 2);
    assert!(export.descriptor.fields.iter().all(|field| field.computed));
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

    assert!(export.bindings.iter().any(|binding| matches!(
        (&binding.selector, binding.operation, binding.receiver),
        (ExternMemberSelector::Method(name), ExternBindingOp::Call, Some(ReceiverMode::Shared)) if name == "len2"
    )));
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
        (ExternMemberSelector::Field(name), ExternBindingOp::Set) if name == "magnitude"
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
