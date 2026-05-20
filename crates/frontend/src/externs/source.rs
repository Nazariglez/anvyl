use anvyx_externs::{
    CallbackEscape, CallbackPolicy, CallbackThread, ExternCallbackParam, ExternCallbackSignature,
    ExternEffects, ExternFieldDescriptor, ExternFunctionDescriptor, ExternInitDescriptor,
    ExternMethodDescriptor, ExternOperator, ExternOperatorDescriptor, ExternParam, ExternRep,
    ExternSignature, ExternStaticDescriptor, ExternTypeExpr, ModulePath, ParamFlow, ReceiverMode,
};

use super::raw::*;
use crate::{
    ast::{
        self, BinaryOp, EscapeMode, ExternFuncNode, ExternReceiverMode, ExternTypeMember,
        ExternTypeNode, ExternTypeRep, GenericArg, Mutability, Param, Program, ReturnSpec, Stmt,
        Type, UnaryOp, Visibility,
    },
    resolve::{ModuleId, ResolveResult},
    source::SourceId,
    span::{SourceSpan, Span},
};

type SourceError = Box<ExternInputError>;
type SourceResult<T> = Result<T, SourceError>;

pub(crate) fn collect_source_externs(
    root: &Program,
    resolved: &ResolveResult,
) -> Result<RawExterns, Vec<ExternInputError>> {
    let mut groups = vec![];
    let mut errors = vec![];
    collect_source_program(
        RawExternScope::Module(resolved.root.clone()),
        resolved.root_source,
        root,
        &mut groups,
        &mut errors,
    );

    for module in resolved.module_groups.iter().flatten() {
        if module.key == resolved.root {
            continue;
        }
        collect_source_program(
            source_scope(&module.key),
            module.source,
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

fn source_scope(module: &ModuleId) -> RawExternScope {
    RawExternScope::Module(module.clone())
}

fn collect_source_program(
    scope: RawExternScope,
    source: SourceId,
    program: &Program,
    groups: &mut Vec<RawExternGroup>,
    errors: &mut Vec<ExternInputError>,
) {
    let mut types = vec![];
    let mut functions = vec![];

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::ExternFunc(func) => match normalize_function(source, func) {
                Ok(func) => functions.push(func),
                Err(error) => errors.push(*error),
            },
            Stmt::ExternType(ty) => match normalize_type(source, ty) {
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

fn normalize_function(source: SourceId, func: &ExternFuncNode) -> SourceResult<RawExternFunction> {
    let span = func.span;
    Ok(RawExternFunction {
        decl: ExternFunctionDescriptor {
            name: func.node.name.to_string(),
            doc: func.node.doc.clone(),
            signature: signature(source, &func.node.params, &func.node.ret, span)?,
            effects: ExternEffects::default(),
        },
        exported: matches!(func.node.visibility, Visibility::Public),
        site: site(source, span),
    })
}

fn normalize_type(
    source: SourceId,
    ty: &ExternTypeNode,
) -> Result<RawExternType, Vec<ExternInputError>> {
    let mut errors = vec![];
    let span = ty.span;
    let mut fields = vec![];
    let mut methods = vec![];
    let mut statics = vec![];
    let mut operators = vec![];

    for member in &ty.node.members {
        match normalize_member(source, member, span) {
            Ok(RawSourceMember::Field(field)) => fields.push(field),
            Ok(RawSourceMember::Method(method)) => methods.push(method),
            Ok(RawSourceMember::Static(static_method)) => statics.push(static_method),
            Ok(RawSourceMember::Operator(operator)) => operators.push(operator),
            Err(error) => errors.push(*error),
        }
    }

    let init = ty.node.init.as_ref().map(|init| RawExternInit {
        decl: ExternInitDescriptor {
            params: vec![
                ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Int,
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                };
                init.params.len()
            ],
            field_init: vec![],
        },
        site: site(source, span),
    });

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(RawExternType {
        name: ty.node.name.to_string(),
        doc: ty.node.doc.clone(),
        exported: matches!(ty.node.visibility, Visibility::Public),
        rep: match ty.node.rep {
            ExternTypeRep::Shared => ExternRep::Shared,
            ExternTypeRep::Inline => ExternRep::Inline,
        },
        fields,
        init,
        methods,
        statics,
        operators,
        site: site(source, span),
    })
}

enum RawSourceMember {
    Field(RawExternField),
    Method(RawExternMethod),
    Static(RawExternStatic),
    Operator(RawExternOperator),
}

fn normalize_member(
    source: SourceId,
    member: &ExternTypeMember,
    span: Span,
) -> SourceResult<RawSourceMember> {
    match member {
        ExternTypeMember::Field {
            doc,
            name,
            ty,
            computed,
        } => Ok(RawSourceMember::Field(RawExternField {
            decl: ExternFieldDescriptor {
                name: name.to_string(),
                ty: type_expr(source, ty, span)?,
                computed: *computed,
                doc: doc.clone(),
            },
            site: site(source, span),
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
                    ExternReceiverMode::Value => ReceiverMode::Value,
                    ExternReceiverMode::Shared => ReceiverMode::Shared,
                    ExternReceiverMode::Mutable => ReceiverMode::Mutable,
                },
                signature: signature(source, params, ret, span)?,
                effects: ExternEffects::default(),
            },
            site: site(source, span),
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
                signature: signature(source, params, ret, span)?,
                effects: ExternEffects::default(),
            },
            site: site(source, span),
        })),
        ExternTypeMember::Operator {
            op,
            other_ty,
            ret,
            self_on_right,
        } => Ok(RawSourceMember::Operator(RawExternOperator {
            decl: ExternOperatorDescriptor {
                op: ExternOperator::Binary {
                    op: binary_op(source, *op, span)?,
                    self_on_right: *self_on_right,
                },
                signature: ExternSignature {
                    params: vec![ExternParam {
                        name: None,
                        ty: type_expr(source, other_ty, span)?,
                        flow: ParamFlow::Value,
                        escape: CallbackEscape::NonEscaping,
                    }],
                    ret: type_expr(source, ret, span)?,
                },
                effects: ExternEffects::default(),
            },
            site: site(source, span),
        })),
        ExternTypeMember::UnaryOperator { op, ret } => {
            Ok(RawSourceMember::Operator(RawExternOperator {
                decl: ExternOperatorDescriptor {
                    op: ExternOperator::Unary(unary_op(source, *op, span)?),
                    signature: ExternSignature {
                        params: vec![],
                        ret: type_expr(source, ret, span)?,
                    },
                    effects: ExternEffects::default(),
                },
                site: site(source, span),
            }))
        }
    }
}

fn signature(
    source: SourceId,
    params: &[Param],
    ret: &ReturnSpec,
    span: Span,
) -> SourceResult<ExternSignature> {
    Ok(ExternSignature {
        params: param_list(source, params, span)?,
        ret: type_expr(source, &ret.ty, span)?,
    })
}

fn param_list(source: SourceId, params: &[Param], span: Span) -> SourceResult<Vec<ExternParam>> {
    params
        .iter()
        .map(|param| lower_param(source, param, span))
        .collect()
}

fn lower_param(source: SourceId, param: &Param, span: Span) -> SourceResult<ExternParam> {
    if matches!(param.mutability, Mutability::Mutable) {
        return Err(unsupported_param(
            source,
            param,
            span,
            UnsupportedSourceParamReason::Mutable,
        ));
    }
    if param.cast_accept {
        return Err(unsupported_param(
            source,
            param,
            span,
            UnsupportedSourceParamReason::CastAccept,
        ));
    }
    if param.default.is_some() {
        return Err(unsupported_param(
            source,
            param,
            span,
            UnsupportedSourceParamReason::Default,
        ));
    }

    Ok(ExternParam {
        name: Some(param.name.to_string()),
        ty: type_expr(source, &param.ty, span)?,
        flow: ParamFlow::Value,
        escape: callback_escape(param.escape),
    })
}

fn unsupported_param(
    source: SourceId,
    param: &Param,
    span: Span,
    reason: UnsupportedSourceParamReason,
) -> SourceError {
    Box::new(ExternInputError::UnsupportedSource {
        span: SourceSpan::from_byte_span(source, span),
        kind: UnsupportedSourceKind::Param {
            name: param.name.to_string(),
            reason,
        },
    })
}

fn callback_escape(escape: EscapeMode) -> CallbackEscape {
    match escape {
        EscapeMode::NonEscaping => CallbackEscape::NonEscaping,
        EscapeMode::Escaping => CallbackEscape::Escaping,
    }
}

fn type_expr(source: SourceId, ty: &Type, span: Span) -> SourceResult<ExternTypeExpr> {
    match ty {
        Type::InferReturn => Err(Box::new(ExternInputError::UnsupportedSource {
            span: SourceSpan::from_byte_span(source, span),
            kind: UnsupportedSourceKind::InferReturn,
        })),
        Type::Void => Ok(ExternTypeExpr::Void),
        Type::Bool => Ok(ExternTypeExpr::Bool),
        Type::Int => Ok(ExternTypeExpr::Int),
        Type::Float => Ok(ExternTypeExpr::Float),
        Type::String => Ok(ExternTypeExpr::String),
        Type::Any => Ok(ExternTypeExpr::Any),
        Type::Optional { inner } => Ok(ExternTypeExpr::Option(Box::new(type_expr(
            source, inner, span,
        )?))),
        Type::List { elem } => Ok(ExternTypeExpr::List(Box::new(type_expr(
            source, elem, span,
        )?))),
        Type::Map { key, value } => Ok(ExternTypeExpr::Map(
            Box::new(type_expr(source, key, span)?),
            Box::new(type_expr(source, value, span)?),
        )),
        Type::Func { params, ret } => {
            if ret.is_place() {
                return Err(Box::new(ExternInputError::UnsupportedSource {
                    span: SourceSpan::from_byte_span(source, span),
                    kind: UnsupportedSourceKind::Type(
                        "callback function returning a mutable place".to_string(),
                    ),
                }));
            }
            if params.iter().any(|param| param.mutable) {
                return Err(unsupported_callback_param(
                    source,
                    span,
                    UnsupportedSourceParamReason::Mutable,
                ));
            }

            Ok(ExternTypeExpr::Callback(ExternCallbackSignature {
                params: params
                    .iter()
                    .map(|param| {
                        Ok(ExternCallbackParam {
                            ty: type_expr(source, &param.ty, span)?,
                            escape: callback_escape(param.escape),
                        })
                    })
                    .collect::<SourceResult<Vec<_>>>()?,
                ret: Box::new(type_expr(source, &ret.ty, span)?),
                policy: CallbackPolicy {
                    escape: CallbackEscape::NonEscaping,
                    thread: CallbackThread::SameThread,
                },
            }))
        }
        Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        } => Ok(ExternTypeExpr::Named {
            module: qualifier.map(|qualifier| ModulePath {
                segments: vec![qualifier.to_string()],
            }),
            name: name.to_string(),
            args: type_args(source, generic_args, span)?,
        }),
        Type::Nominal(nominal) if nominal.const_args.is_empty() => Ok(ExternTypeExpr::Named {
            module: nominal
                .origin
                .as_ref()
                .and_then(ast::ModuleOrigin::module_path)
                .map(|path| ModulePath {
                    segments: path.to_vec(),
                }),
            name: nominal.name.to_string(),
            args: nominal
                .type_args
                .iter()
                .map(|arg| type_expr(source, arg, span))
                .collect::<SourceResult<Vec<_>>>()?,
        }),
        _ => Err(unsupported_type(source, ty, span)),
    }
}

fn type_args(
    source: SourceId,
    args: &[GenericArg],
    span: Span,
) -> SourceResult<Vec<ExternTypeExpr>> {
    args.iter()
        .map(|arg| match arg {
            GenericArg::Type(ty) => type_expr(source, ty, span),
            GenericArg::Const(_) => Err(unsupported_type(
                source,
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

fn unsupported_type(source: SourceId, ty: &Type, span: Span) -> SourceError {
    Box::new(ExternInputError::UnsupportedSource {
        span: SourceSpan::from_byte_span(source, span),
        kind: UnsupportedSourceKind::Type(ty.to_string()),
    })
}

fn unsupported_callback_param(
    source: SourceId,
    span: Span,
    reason: UnsupportedSourceParamReason,
) -> SourceError {
    Box::new(ExternInputError::UnsupportedSource {
        span: SourceSpan::from_byte_span(source, span),
        kind: UnsupportedSourceKind::CallbackParam { reason },
    })
}

fn binary_op(source: SourceId, op: BinaryOp, span: Span) -> SourceResult<anvyx_externs::BinaryOp> {
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
        _ => Err(unsupported_operator(source, op, span)),
    }
}

fn unary_op(source: SourceId, op: UnaryOp, span: Span) -> SourceResult<anvyx_externs::UnaryOp> {
    match op {
        UnaryOp::Neg => Ok(anvyx_externs::UnaryOp::Neg),
        _ => Err(unsupported_operator(source, op, span)),
    }
}

fn unsupported_operator(source: SourceId, op: impl std::fmt::Display, span: Span) -> SourceError {
    Box::new(ExternInputError::UnsupportedSource {
        span: SourceSpan::from_byte_span(source, span),
        kind: UnsupportedSourceKind::Operator(op.to_string()),
    })
}

fn site(source: SourceId, span: Span) -> RawExternSite {
    RawExternSite {
        span: Some(SourceSpan::from_byte_span(source, span)),
    }
}
