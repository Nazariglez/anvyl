use anvyx_externs::{
    CallbackEscape, CallbackPolicy, CallbackThread, ExternCallbackSignature, ExternEffects,
    ExternFieldDescriptor, ExternFunctionDescriptor, ExternInitDescriptor, ExternMethodDescriptor,
    ExternOperator, ExternOperatorDescriptor, ExternParam, ExternRep, ExternSignature,
    ExternStaticDescriptor, ExternTypeExpr, FieldAccess, ModulePath, ParamFlow, ReceiverMode,
};

use super::raw::*;
use crate::{
    ast::{
        self, BinaryOp, ExternFuncNode, ExternKind, ExternTypeMember, ExternTypeNode, GenericArg,
        MethodReceiver, Mutability, Param, Program, Stmt, Type, UnaryOp,
    },
    resolve::{ModuleKey, ResolveResult},
    span::Span,
};

pub(crate) fn collect_source_externs(
    root: &Program,
    resolved: &ResolveResult,
) -> Result<RawExterns, Vec<ExternInputError>> {
    let mut groups = vec![];
    let mut errors = vec![];
    collect_source_program(RawExternScope::Root, root, &mut groups, &mut errors);

    for module in resolved.module_groups.iter().flatten() {
        let ModuleKey::Named(path) = &module.key else {
            continue;
        };
        collect_source_program(
            RawExternScope::Named(module_path_from_resolve(path)),
            &module.program,
            &mut groups,
            &mut errors,
        );
    }

    if errors.is_empty() {
        Ok(RawExterns { groups })
    } else {
        Err(errors)
    }
}

fn collect_source_program(
    scope: RawExternScope,
    program: &Program,
    groups: &mut Vec<RawExternGroup>,
    errors: &mut Vec<ExternInputError>,
) {
    let mut types = vec![];
    let mut functions = vec![];

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::ExternFunc(func) => match normalize_source_function(func) {
                Ok(func) => functions.push(func),
                Err(error) => errors.push(error),
            },
            Stmt::ExternType(ty) => match normalize_source_type(ty) {
                Ok(ty) => types.push(ty),
                Err(type_errors) => errors.extend(type_errors),
            },
            _ => {}
        }
    }

    if types.is_empty() && functions.is_empty() {
        return;
    }

    groups.push(RawExternGroup {
        provenance: ExternProvenance::Source {
            module: scope.clone(),
        },
        modules: vec![RawExternModule {
            scope,
            types,
            functions,
        }],
    });
}

fn module_path_from_resolve(path: &crate::resolve::ModulePath) -> ModulePath {
    ModulePath {
        segments: path.segments().to_vec(),
    }
}

fn normalize_source_function(func: &ExternFuncNode) -> Result<RawExternFunction, ExternInputError> {
    let span = func.span;
    let params = source_params(&func.node.params, span)?;
    let ret = source_type_expr(&func.node.ret, span)?;
    Ok(RawExternFunction {
        decl: ExternFunctionDescriptor {
            name: func.node.name.to_string(),
            doc: func.node.doc.clone(),
            signature: ExternSignature { params, ret },
            effects: ExternEffects::default(),
        },
        site: RawExternSite { span: Some(span) },
    })
}

fn normalize_source_type(ty: &ExternTypeNode) -> Result<RawExternType, Vec<ExternInputError>> {
    let mut errors = vec![];
    let span = ty.span;
    let mut fields = vec![];
    let mut methods = vec![];
    let mut statics = vec![];
    let mut operators = vec![];

    for member in &ty.node.members {
        match normalize_source_member(member, span) {
            Ok(RawSourceMember::Field(field)) => fields.push(field),
            Ok(RawSourceMember::Method(method)) => methods.push(method),
            Ok(RawSourceMember::Static(static_method)) => statics.push(static_method),
            Ok(RawSourceMember::Operator(operator)) => operators.push(operator),
            Err(error) => errors.push(error),
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(RawExternType {
        name: ty.node.name.to_string(),
        doc: ty.node.doc.clone(),
        rep: match ty.node.kind {
            ExternKind::SharedIdentity => ExternRep::Shared,
            ExternKind::InlineCopy => ExternRep::Inline,
        },
        fields,
        init: ty.node.has_init.then(|| RawExternInit {
            decl: ExternInitDescriptor {
                params: vec![],
                field_init: vec![],
            },
            site: RawExternSite { span: Some(span) },
        }),
        methods,
        statics,
        operators,
        site: RawExternSite { span: Some(span) },
    })
}

enum RawSourceMember {
    Field(RawExternField),
    Method(RawExternMethod),
    Static(RawExternStatic),
    Operator(RawExternOperator),
}

fn normalize_source_member(
    member: &ExternTypeMember,
    span: Span,
) -> Result<RawSourceMember, ExternInputError> {
    match member {
        ExternTypeMember::Field {
            doc,
            name,
            ty,
            computed,
        } => Ok(RawSourceMember::Field(RawExternField {
            decl: ExternFieldDescriptor {
                name: name.to_string(),
                ty: source_type_expr(ty, span)?,
                access: FieldAccess::ReadWrite {
                    computed: *computed,
                },
                doc: doc.clone(),
            },
            site: RawExternSite { span: Some(span) },
        })),
        ExternTypeMember::Method {
            doc,
            name,
            receiver,
            params,
            ret,
        } => Ok(RawSourceMember::Method(RawExternMethod {
            decl: ExternMethodDescriptor {
                name: name.to_string(),
                doc: doc.clone(),
                receiver: match receiver {
                    MethodReceiver::Value => ReceiverMode::Value,
                    MethodReceiver::Var => ReceiverMode::Mutable,
                },
                signature: ExternSignature {
                    params: source_params(params, span)?,
                    ret: source_type_expr(ret, span)?,
                },
                effects: ExternEffects::default(),
            },
            site: RawExternSite { span: Some(span) },
        })),
        ExternTypeMember::StaticMethod {
            doc,
            name,
            params,
            ret,
        } => Ok(RawSourceMember::Static(RawExternStatic {
            decl: ExternStaticDescriptor {
                name: name.to_string(),
                doc: doc.clone(),
                signature: ExternSignature {
                    params: source_params(params, span)?,
                    ret: source_type_expr(ret, span)?,
                },
                effects: ExternEffects::default(),
            },
            site: RawExternSite { span: Some(span) },
        })),
        ExternTypeMember::Operator {
            op,
            other_ty,
            ret,
            self_on_right,
        } => Ok(RawSourceMember::Operator(RawExternOperator {
            decl: ExternOperatorDescriptor {
                op: ExternOperator::Binary {
                    op: source_binary_op(*op, span)?,
                    self_on_right: *self_on_right,
                },
                signature: ExternSignature {
                    params: vec![ExternParam {
                        name: None,
                        ty: source_type_expr(other_ty, span)?,
                        flow: ParamFlow::Value,
                    }],
                    ret: source_type_expr(ret, span)?,
                },
                effects: ExternEffects::default(),
            },
            site: RawExternSite { span: Some(span) },
        })),
        ExternTypeMember::UnaryOperator { op, ret } => {
            Ok(RawSourceMember::Operator(RawExternOperator {
                decl: ExternOperatorDescriptor {
                    op: ExternOperator::Unary(source_unary_op(*op, span)?),
                    signature: ExternSignature {
                        params: vec![],
                        ret: source_type_expr(ret, span)?,
                    },
                    effects: ExternEffects::default(),
                },
                site: RawExternSite { span: Some(span) },
            }))
        }
    }
}

fn source_params(params: &[Param], span: Span) -> Result<Vec<ExternParam>, ExternInputError> {
    params
        .iter()
        .map(|param| source_param(param, span))
        .collect()
}

fn source_param(param: &Param, span: Span) -> Result<ExternParam, ExternInputError> {
    if matches!(param.mutability, Mutability::Mutable) {
        return Err(unsupported_source_param(
            param,
            span,
            UnsupportedSourceParamReason::Mutable,
        ));
    }
    if param.cast_accept {
        return Err(unsupported_source_param(
            param,
            span,
            UnsupportedSourceParamReason::CastAccept,
        ));
    }
    if param.default.is_some() {
        return Err(unsupported_source_param(
            param,
            span,
            UnsupportedSourceParamReason::Default,
        ));
    }

    Ok(ExternParam {
        name: Some(param.name.to_string()),
        ty: source_type_expr(&param.ty, span)?,
        flow: ParamFlow::Value,
    })
}

fn unsupported_source_param(
    param: &Param,
    span: Span,
    reason: UnsupportedSourceParamReason,
) -> ExternInputError {
    ExternInputError::UnsupportedSource {
        span,
        kind: UnsupportedSourceKind::Param {
            name: param.name.to_string(),
            reason,
        },
    }
}

fn source_type_expr(ty: &Type, span: Span) -> Result<ExternTypeExpr, ExternInputError> {
    if let Some(inner) = ty.option_inner() {
        return Ok(ExternTypeExpr::Option(Box::new(source_type_expr(
            inner, span,
        )?)));
    }

    match ty {
        Type::Void => Ok(ExternTypeExpr::Void),
        Type::Bool => Ok(ExternTypeExpr::Bool),
        Type::Int => Ok(ExternTypeExpr::Int),
        Type::Float => Ok(ExternTypeExpr::Float),
        Type::String => Ok(ExternTypeExpr::String),
        Type::Any => Ok(ExternTypeExpr::Any),
        Type::List { elem } => Ok(ExternTypeExpr::List(Box::new(source_type_expr(
            elem, span,
        )?))),
        Type::Map { key, value } => Ok(ExternTypeExpr::Map(
            Box::new(source_type_expr(key, span)?),
            Box::new(source_type_expr(value, span)?),
        )),
        Type::Func { params, ret } => Ok(ExternTypeExpr::Callback(ExternCallbackSignature {
            params: params
                .iter()
                .map(|param| source_type_expr(&param.ty, span))
                .collect::<Result<Vec<_>, _>>()?,
            ret: Box::new(source_type_expr(ret, span)?),
            policy: CallbackPolicy {
                escape: CallbackEscape::NonEscaping,
                thread: CallbackThread::SameThread,
            },
        })),
        Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        } => Ok(ExternTypeExpr::Named {
            module: qualifier.map(|qualifier| ModulePath {
                segments: vec![qualifier.to_string()],
            }),
            name: name.to_string(),
            args: source_generic_args(generic_args, span)?,
        }),
        Type::Nominal(nominal) if nominal.const_args.is_empty() => Ok(ExternTypeExpr::Named {
            module: nominal.origin.as_ref().map(|path| ModulePath {
                segments: path.iter().cloned().collect(),
            }),
            name: nominal.name.to_string(),
            args: nominal
                .type_args
                .iter()
                .map(|arg| source_type_expr(arg, span))
                .collect::<Result<Vec<_>, _>>()?,
        }),
        _ => Err(unsupported_source_type(ty, span)),
    }
}

fn source_generic_args(
    args: &[GenericArg],
    span: Span,
) -> Result<Vec<ExternTypeExpr>, ExternInputError> {
    args.iter()
        .map(|arg| match arg {
            GenericArg::Type(ty) => source_type_expr(ty, span),
            GenericArg::Const(_) => Err(unsupported_source_type(
                &Type::UnresolvedNominal {
                    qualifier: None,
                    name: ast::Ident::new("const generic"),
                    generic_args: vec![],
                },
                span,
            )),
        })
        .collect()
}

fn unsupported_source_type(ty: &Type, span: Span) -> ExternInputError {
    ExternInputError::UnsupportedSource {
        span,
        kind: UnsupportedSourceKind::Type(ty.to_string()),
    }
}

fn source_binary_op(op: BinaryOp, span: Span) -> Result<anvyx_externs::BinaryOp, ExternInputError> {
    match op {
        BinaryOp::Add => Ok(anvyx_externs::BinaryOp::Add),
        BinaryOp::Sub => Ok(anvyx_externs::BinaryOp::Sub),
        BinaryOp::Mul => Ok(anvyx_externs::BinaryOp::Mul),
        BinaryOp::Div => Ok(anvyx_externs::BinaryOp::Div),
        BinaryOp::Rem => Ok(anvyx_externs::BinaryOp::Rem),
        BinaryOp::Eq => Ok(anvyx_externs::BinaryOp::Eq),
        BinaryOp::NotEq => Ok(anvyx_externs::BinaryOp::NotEq),
        BinaryOp::LessThan => Ok(anvyx_externs::BinaryOp::LessThan),
        BinaryOp::GreaterThan => Ok(anvyx_externs::BinaryOp::GreaterThan),
        BinaryOp::LessThanEq => Ok(anvyx_externs::BinaryOp::LessThanEq),
        BinaryOp::GreaterThanEq => Ok(anvyx_externs::BinaryOp::GreaterThanEq),
        _ => Err(unsupported_source_operator(op, span)),
    }
}

fn source_unary_op(op: UnaryOp, span: Span) -> Result<anvyx_externs::UnaryOp, ExternInputError> {
    match op {
        UnaryOp::Neg => Ok(anvyx_externs::UnaryOp::Neg),
        _ => Err(unsupported_source_operator(op, span)),
    }
}

fn unsupported_source_operator(op: impl std::fmt::Display, span: Span) -> ExternInputError {
    ExternInputError::UnsupportedSource {
        span,
        kind: UnsupportedSourceKind::Operator(op.to_string()),
    }
}
