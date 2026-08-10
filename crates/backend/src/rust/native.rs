use anvyx_externs::{
    CallbackEscape, ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternEffects,
    ExternFieldDescriptor, ExternFunctionDescriptor, ExternInitDescriptor, ExternMemberSelector,
    ExternMethodDescriptor, ExternModuleDescriptor, ExternOperatorDescriptor, ExternParam,
    ExternSignature, ExternStaticDescriptor, ExternTypeDescriptor, ExternTypeExpr, ReceiverMode,
    RustCallContext, RustExternBinding,
};
use anvyx_frontend::{air, ast::Ident};

pub(super) fn attests_air(
    program: &air::Program,
    decl: &air::ExternDecl,
    binding: &RustExternBinding,
    module: &ExternModuleDescriptor,
) -> Result<Option<usize>, ()> {
    let expected_key = decl.binding.as_ref().ok_or(())?;
    if binding.key != expected_key.key
        || !module_matches(program, decl.module, binding_module(&binding.key))
        || !member_identity_matches(program, decl, &binding.key)
    {
        return Err(());
    }
    let receiver = descriptor_matches_air(program, decl, module, binding)?;
    let hidden_ctx_ok = binding.abi.ctx != RustCallContext::HiddenRuntime
        || decl
            .call_params()
            .zip(&binding.abi.params)
            .all(|(param, adapter)| {
                !adapter.is_shared_or_mut_borrow()
                    || !matches!(
                        program.type_arena.data(param.ty),
                        air::TypeData::Extern(id)
                            if program.extern_type(*id).rep == air::ExternRep::Shared
                    )
            });
    (binding.abi.params.len() == decl.call_arity() && hidden_ctx_ok)
        .then_some(receiver)
        .ok_or(())
}

pub(super) fn attests_air_type(
    program: &air::Program,
    decl: &air::ExternTypeDecl,
    module: &ExternModuleDescriptor,
    catalog: &ExternTypeDescriptor,
) -> bool {
    let Some(binding) = &decl.binding else {
        return false;
    };
    let rep_matches = matches!(
        (decl.rep, catalog.rep),
        (air::ExternRep::Inline, anvyx_externs::ExternRep::Inline)
            | (air::ExternRep::Shared, anvyx_externs::ExternRep::Shared)
    );
    module_matches(program, decl.module, &binding.key.module)
        && binding.key.module == module.path
        && binding.key.name == catalog.name
        && decl.name.as_str() == catalog.name
        && rep_matches
        && decl.layout == catalog.layout
        && decl.materialization == catalog.materialization
        && decl.owns_heap_edges == catalog.owns_heap_edges
}

fn module_matches(
    program: &air::Program,
    actual: air::ModuleId,
    expected: &anvyx_externs::ModulePath,
) -> bool {
    program
        .module(actual)
        .path
        .iter()
        .map(Ident::as_str)
        .eq(expected.segments.iter().map(String::as_str))
}

fn binding_module(key: &ExternBindingKey) -> &anvyx_externs::ModulePath {
    match &key.target {
        ExternBindingTarget::Function(function) => &function.module,
        ExternBindingTarget::Member(member) => &member.owner.module,
    }
}

enum DescriptorBinding<'a> {
    Function(&'a ExternFunctionDescriptor),
    Field {
        owner: &'a ExternTypeDescriptor,
        field: &'a ExternFieldDescriptor,
        writable: bool,
    },
    Method {
        owner: &'a ExternTypeDescriptor,
        method: &'a ExternMethodDescriptor,
    },
    Static(&'a ExternStaticDescriptor),
    Init(&'a ExternInitDescriptor),
    Operator {
        owner: &'a ExternTypeDescriptor,
        operator: &'a ExternOperatorDescriptor,
    },
}

fn descriptor_binding<'a>(
    module: &'a ExternModuleDescriptor,
    key: &ExternBindingKey,
) -> Option<DescriptorBinding<'a>> {
    match (&key.target, key.operation) {
        (ExternBindingTarget::Function(key), ExternBindingOp::Call)
            if key.module == module.path =>
        {
            module
                .functions
                .iter()
                .find(|function| function.name == key.name)
                .map(DescriptorBinding::Function)
        }
        (ExternBindingTarget::Member(member), operation) if member.owner.module == module.path => {
            let owner = module
                .types
                .iter()
                .find(|ty| ty.name == member.owner.name)?;
            match (&member.selector, operation) {
                (
                    ExternMemberSelector::Field(name),
                    ExternBindingOp::Get | ExternBindingOp::Set,
                ) => {
                    let writable = operation == ExternBindingOp::Set;
                    owner
                        .fields
                        .iter()
                        .find(|field| {
                            field.name == *name
                                && if writable {
                                    field.writable
                                } else {
                                    field.readable
                                }
                        })
                        .map(|field| DescriptorBinding::Field {
                            owner,
                            field,
                            writable,
                        })
                }
                (ExternMemberSelector::Method(name), ExternBindingOp::Call) => owner
                    .methods
                    .iter()
                    .find(|method| method.name == *name)
                    .map(|method| DescriptorBinding::Method { owner, method }),
                (ExternMemberSelector::Static(name), ExternBindingOp::Call) => owner
                    .statics
                    .iter()
                    .find(|static_method| static_method.name == *name)
                    .map(DescriptorBinding::Static),
                (ExternMemberSelector::Init, ExternBindingOp::Call) => {
                    owner.init.as_ref().map(DescriptorBinding::Init)
                }
                (ExternMemberSelector::Operator(op), ExternBindingOp::Call) => owner
                    .operators
                    .iter()
                    .find(|operator| operator.op == *op)
                    .map(|operator| DescriptorBinding::Operator { owner, operator }),
                _ => None,
            }
        }
        _ => None,
    }
}

fn descriptor_matches_air(
    program: &air::Program,
    decl: &air::ExternDecl,
    module: &ExternModuleDescriptor,
    binding: &RustExternBinding,
) -> Result<Option<usize>, ()> {
    let descriptor = descriptor_binding(module, &binding.key).ok_or(())?;
    match descriptor {
        DescriptorBinding::Function(function) => signature_matches_air(
            program,
            decl,
            &module.path,
            &function.signature,
            function.effects,
            None,
        ),
        DescriptorBinding::Field {
            owner,
            field,
            writable,
        } => signature_matches_air(
            program,
            decl,
            &module.path,
            &ExternSignature {
                params: writable
                    .then(|| value_param(field.ty.clone()))
                    .into_iter()
                    .collect(),
                ret: if writable {
                    ExternTypeExpr::Void
                } else {
                    field.ty.clone()
                },
            },
            ExternEffects::default(),
            Some((
                owner,
                if writable {
                    field.set_receiver
                } else {
                    field.get_receiver
                },
            )),
        ),
        DescriptorBinding::Method { owner, method } => signature_matches_air(
            program,
            decl,
            &module.path,
            &method.signature,
            method.effects,
            Some((owner, method.receiver)),
        ),
        DescriptorBinding::Static(static_method) => signature_matches_air(
            program,
            decl,
            &module.path,
            &static_method.signature,
            static_method.effects,
            None,
        ),
        DescriptorBinding::Init(init) => signature_matches_air(
            program,
            decl,
            &module.path,
            &init_signature(init),
            init.effects,
            None,
        ),
        DescriptorBinding::Operator { owner, operator } => signature_matches_air(
            program,
            decl,
            &module.path,
            &operator.signature,
            operator.effects,
            Some((owner, operator.receiver)),
        ),
    }
}

fn init_signature(init: &ExternInitDescriptor) -> ExternSignature {
    ExternSignature {
        params: init.params.clone(),
        ret: init.ret.clone(),
    }
}

fn signature_matches_air(
    program: &air::Program,
    decl: &air::ExternDecl,
    descriptor_module: &anvyx_externs::ModulePath,
    signature: &ExternSignature,
    effects: ExternEffects,
    receiver: Option<(&ExternTypeDescriptor, ReceiverMode)>,
) -> Result<Option<usize>, ()> {
    let receiver_abi = receiver
        .map(|_| receiver_abi(decl.binding.as_ref().expect("attested binding")))
        .transpose()?
        .flatten();
    let expected_arity = signature.params.len() + usize::from(receiver.is_some());
    if decl.call_arity() != expected_arity
        || decl.params.len() != signature.params.len()
        || decl.abi.params.len() != expected_arity
        || decl.abi.ret != signature.ret
        || decl.effects != effects
        || !decl
            .params
            .iter()
            .zip(&signature.params)
            .all(|(actual, expected)| {
                actual.mode == param_mode(expected.flow)
                    && actual.escape == param_escape(expected.escape)
                    && air_type_matches(program, actual.ty, &expected.ty, descriptor_module, decl)
            })
        || !air_type_matches(
            program,
            decl.return_type,
            &signature.ret,
            descriptor_module,
            decl,
        )
        || !decl
            .abi
            .params
            .iter()
            .skip(usize::from(receiver.is_some()))
            .zip(&signature.params)
            .all(|(actual, expected)| actual == &expected.ty)
    {
        return Err(());
    }
    match (
        receiver,
        receiver_abi,
        decl.call_param(0),
        decl.abi.params.first(),
    ) {
        (None, None, _, _) => Ok(None),
        (Some((_, mode)), Some(expected), Some(actual), Some(actual_abi))
            if actual.mode == receiver_mode(mode)
                && actual.escape == air::ParamEscape::NonEscaping
                && air_type_matches(program, actual.ty, &expected, descriptor_module, decl)
                && actual_abi == &expected =>
        {
            Ok(Some(0))
        }
        _ => Err(()),
    }
}

fn air_type_matches(
    program: &air::Program,
    actual: air::TypeId,
    expected: &ExternTypeExpr,
    descriptor_module: &anvyx_externs::ModulePath,
    callable: &air::ExternDecl,
) -> bool {
    use air::TypeData;
    use anvyx_externs::ExternTypeExpr;

    match (program.type_arena.data(actual), expected) {
        (TypeData::Void, ExternTypeExpr::Void)
        | (TypeData::Int, ExternTypeExpr::Int)
        | (TypeData::Float, ExternTypeExpr::Float)
        | (TypeData::Bool, ExternTypeExpr::Bool)
        | (TypeData::String, ExternTypeExpr::String)
        | (TypeData::Char, ExternTypeExpr::Char)
        | (TypeData::Any, ExternTypeExpr::Any) => true,
        (TypeData::Tuple(items), ExternTypeExpr::Unit) => items.is_empty(),
        (TypeData::Optional(actual), ExternTypeExpr::Option(expected))
        | (TypeData::List(actual), ExternTypeExpr::List(expected))
        | (TypeData::Slice(actual), ExternTypeExpr::Slice(expected)) => {
            air_type_matches(program, *actual, expected, descriptor_module, callable)
        }
        (
            TypeData::Array { elem, len },
            ExternTypeExpr::Array {
                elem: expected,
                len: expected_len,
            },
        ) => {
            *len as u64 == *expected_len
                && air_type_matches(program, *elem, expected, descriptor_module, callable)
        }
        (TypeData::Map { key, value, .. }, ExternTypeExpr::Map(expected_key, expected_value)) => {
            air_type_matches(program, *key, expected_key, descriptor_module, callable)
                && air_type_matches(program, *value, expected_value, descriptor_module, callable)
        }
        (TypeData::Tuple(actual), ExternTypeExpr::Tuple(expected)) => {
            actual.len() == expected.len()
                && actual.iter().zip(expected).all(|(actual, expected)| {
                    air_type_matches(program, *actual, expected, descriptor_module, callable)
                })
        }
        (TypeData::Function(actual), ExternTypeExpr::Callback(expected)) => {
            expected.policy.thread == anvyx_externs::CallbackThread::SameThread
                && actual.params.len() == expected.params.len()
                && actual
                    .params
                    .iter()
                    .zip(&expected.params)
                    .all(|(actual, expected)| {
                        actual.mode == air::ParamMode::Value
                            && actual.escape == param_escape(expected.escape)
                            && air_type_matches(
                                program,
                                actual.ty,
                                &expected.ty,
                                descriptor_module,
                                callable,
                            )
                    })
                && matches!(actual.ret, air::ReturnMode::Value(actual)
                    if air_type_matches(program, actual, &expected.ret, descriptor_module, callable))
        }
        (TypeData::Enum(id), ExternTypeExpr::Option(expected))
            if program.enums[id.index()].core == Some(air::CoreEnumKind::Option) =>
        {
            matches!(program.enums[id.index()].type_args.as_slice(), [inner]
                if air_type_matches(program, *inner, expected, descriptor_module, callable))
        }
        (TypeData::Enum(id), ExternTypeExpr::Result(expected_ok, expected_err))
            if program.enums[id.index()].core == Some(air::CoreEnumKind::Result) =>
        {
            matches!(program.enums[id.index()].type_args.as_slice(), [ok, err]
                if air_type_matches(program, *ok, expected_ok, descriptor_module, callable)
                    && air_type_matches(program, *err, expected_err, descriptor_module, callable))
        }
        (TypeData::Extern(id), ExternTypeExpr::Named { module, name, args }) => {
            let ty = program.extern_type(*id);
            let Some(type_binding) = &ty.binding else {
                return false;
            };
            let Some(call_binding) = &callable.binding else {
                return false;
            };
            let expected_module = module.as_ref().unwrap_or(descriptor_module);
            type_binding.package == call_binding.package
                && module_matches(program, ty.module, &type_binding.key.module)
                && ty.name.as_str() == type_binding.key.name
                && type_binding.key.module == *expected_module
                && type_binding.key.name == *name
                && args.len() == ty.type_args.len()
                && args.iter().zip(&ty.type_args).all(|(expected, actual)| {
                    air_type_matches(program, *actual, expected, descriptor_module, callable)
                })
        }
        _ => false,
    }
}

fn receiver_abi(binding: &air::ExternBindingDecl) -> Result<Option<ExternTypeExpr>, ()> {
    let ExternBindingTarget::Member(member) = &binding.key.target else {
        return Err(());
    };
    Ok(Some(ExternTypeExpr::Named {
        module: Some(member.owner.module.clone()),
        name: member.owner.name.clone(),
        args: vec![],
    }))
}

fn member_identity_matches(
    program: &air::Program,
    decl: &air::ExternDecl,
    key: &ExternBindingKey,
) -> bool {
    let Some(binding) = decl.binding.as_ref() else {
        return false;
    };
    match (&decl.member, &key.target, key.operation) {
        (
            air::ExternMember::FreeFunction,
            ExternBindingTarget::Function(function),
            ExternBindingOp::Call,
        ) => function.name == decl.name.as_str(),
        (
            air::ExternMember::FieldGetter { owner, .. },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Get,
        )
        | (
            air::ExternMember::FieldSetter { owner, .. },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Set,
        ) => {
            member.selector == ExternMemberSelector::Field(decl.name.to_string())
                && owner_matches(program, *owner, &member.owner, binding)
        }
        (
            air::ExternMember::Method { owner, .. },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Call,
        ) => {
            member.selector == ExternMemberSelector::Method(decl.name.to_string())
                && owner_matches(program, *owner, &member.owner, binding)
        }
        (
            air::ExternMember::StaticMethod { owner },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Call,
        ) => {
            member.selector == ExternMemberSelector::Static(decl.name.to_string())
                && owner_matches(program, *owner, &member.owner, binding)
        }
        (
            air::ExternMember::Init { owner },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Call,
        ) => {
            member.selector == ExternMemberSelector::Init
                && owner_matches(program, *owner, &member.owner, binding)
        }
        (
            air::ExternMember::UnaryOperator {
                owner,
                op: air::UnaryOp::Neg,
                ..
            },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Call,
        ) => {
            member.selector
                == ExternMemberSelector::Operator(anvyx_externs::ExternOperator::Unary(
                    anvyx_externs::UnaryOp::Neg,
                ))
                && owner_matches(program, *owner, &member.owner, binding)
        }
        (
            air::ExternMember::BinaryOperator {
                owner,
                op,
                self_on_right,
                ..
            },
            ExternBindingTarget::Member(member),
            ExternBindingOp::Call,
        ) => {
            extern_binary_op(*op).is_some_and(|op| {
                member.selector
                    == ExternMemberSelector::Operator(anvyx_externs::ExternOperator::Binary {
                        op,
                        self_on_right: *self_on_right,
                    })
            }) && owner_matches(program, *owner, &member.owner, binding)
        }
        _ => false,
    }
}

fn owner_matches(
    program: &air::Program,
    owner: air::ExternTypeId,
    expected: &anvyx_externs::ExternTypeKey,
    callable: &air::ExternBindingDecl,
) -> bool {
    program
        .extern_type(owner)
        .binding
        .as_ref()
        .is_some_and(|binding| {
            binding.key == *expected
                && binding.package == callable.package
                && binding.provider == callable.provider
        })
}

fn extern_binary_op(op: air::BinaryOp) -> Option<anvyx_externs::BinaryOp> {
    Some(match op {
        air::BinaryOp::Add => anvyx_externs::BinaryOp::Add,
        air::BinaryOp::Sub => anvyx_externs::BinaryOp::Sub,
        air::BinaryOp::Mul => anvyx_externs::BinaryOp::Mul,
        air::BinaryOp::Div => anvyx_externs::BinaryOp::Div,
        air::BinaryOp::Rem => anvyx_externs::BinaryOp::Rem,
        air::BinaryOp::Eq => anvyx_externs::BinaryOp::Eq,
        air::BinaryOp::NotEq => anvyx_externs::BinaryOp::NotEq,
        air::BinaryOp::LessThan => anvyx_externs::BinaryOp::LessThan,
        air::BinaryOp::GreaterThan => anvyx_externs::BinaryOp::GreaterThan,
        air::BinaryOp::LessThanEq => anvyx_externs::BinaryOp::LessThanEq,
        air::BinaryOp::GreaterThanEq => anvyx_externs::BinaryOp::GreaterThanEq,
        _ => return None,
    })
}

fn param_mode(flow: anvyx_externs::ParamFlow) -> air::ParamMode {
    match flow {
        anvyx_externs::ParamFlow::Value => air::ParamMode::Value,
        anvyx_externs::ParamFlow::Borrow => air::ParamMode::SharedBorrow,
        anvyx_externs::ParamFlow::MutBorrow => air::ParamMode::MutBorrow,
    }
}

fn receiver_mode(mode: ReceiverMode) -> air::ParamMode {
    match mode {
        ReceiverMode::Value => air::ParamMode::Value,
        ReceiverMode::Shared => air::ParamMode::SharedBorrow,
        ReceiverMode::Mutable => air::ParamMode::MutBorrow,
    }
}

fn param_escape(escape: CallbackEscape) -> air::ParamEscape {
    match escape {
        CallbackEscape::NonEscaping => air::ParamEscape::NonEscaping,
        CallbackEscape::Escaping => air::ParamEscape::Escaping,
    }
}

fn value_param(ty: ExternTypeExpr) -> ExternParam {
    ExternParam {
        name: None,
        ty,
        flow: anvyx_externs::ParamFlow::Value,
        escape: CallbackEscape::NonEscaping,
    }
}
