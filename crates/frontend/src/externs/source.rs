use anvyx_externs::{
    CallbackEscape, CallbackPolicy, CallbackThread, ExternCallbackSignature, ExternEffects,
    ExternFieldDescriptor, ExternFunctionDescriptor, ExternInitDescriptor, ExternMethodDescriptor,
    ExternOperator, ExternOperatorDescriptor, ExternParam, ExternRep, ExternSignature,
    ExternStaticDescriptor, ExternTypeExpr, FieldAccess, ModulePath, ParamFlow, ReceiverMode,
};

use super::raw::*;
use crate::{
    ast::{
        self, BinaryOp, ExternFieldAccess, ExternFuncNode, ExternReceiverMode, ExternTypeMember,
        ExternTypeNode, ExternTypeRep, GenericArg, Mutability, Param, Program, Stmt, Type, UnaryOp,
    },
    resolve::{ModuleId, ResolveResult},
    span::Span,
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
    program: &Program,
    groups: &mut Vec<RawExternGroup>,
    errors: &mut Vec<ExternInputError>,
) {
    let mut types = vec![];
    let mut functions = vec![];

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::ExternFunc(func) => match normalize_function(func) {
                Ok(func) => functions.push(func),
                Err(error) => errors.push(*error),
            },
            Stmt::ExternType(ty) => match normalize_type(ty) {
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

fn normalize_function(func: &ExternFuncNode) -> SourceResult<RawExternFunction> {
    let span = func.span;
    Ok(RawExternFunction {
        decl: ExternFunctionDescriptor {
            name: func.node.name.to_string(),
            doc: func.node.doc.clone(),
            signature: signature(&func.node.params, &func.node.ret, span)?,
            effects: ExternEffects::default(),
        },
        site: RawExternSite { span: Some(span) },
    })
}

fn normalize_type(ty: &ExternTypeNode) -> Result<RawExternType, Vec<ExternInputError>> {
    let mut errors = vec![];
    let span = ty.span;
    let mut fields = vec![];
    let mut methods = vec![];
    let mut statics = vec![];
    let mut operators = vec![];

    for member in &ty.node.members {
        match normalize_member(member, span) {
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
                };
                init.params.len()
            ],
            field_init: vec![],
        },
        site: RawExternSite { span: Some(span) },
    });

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(RawExternType {
        name: ty.node.name.to_string(),
        doc: ty.node.doc.clone(),
        rep: match ty.node.rep {
            ExternTypeRep::Shared => ExternRep::Shared,
            ExternTypeRep::Inline => ExternRep::Inline,
        },
        fields,
        init,
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

fn normalize_member(member: &ExternTypeMember, span: Span) -> SourceResult<RawSourceMember> {
    match member {
        ExternTypeMember::Field {
            doc,
            name,
            ty,
            access,
            computed,
        } => Ok(RawSourceMember::Field(RawExternField {
            decl: ExternFieldDescriptor {
                name: name.to_string(),
                ty: type_expr(ty, span)?,
                access: match access {
                    ExternFieldAccess::ReadOnly => FieldAccess::ReadOnly {
                        computed: *computed,
                    },
                    ExternFieldAccess::ReadWrite => FieldAccess::ReadWrite {
                        computed: *computed,
                    },
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
                    ExternReceiverMode::Value => ReceiverMode::Value,
                    ExternReceiverMode::Shared => ReceiverMode::Shared,
                    ExternReceiverMode::Mutable => ReceiverMode::Mutable,
                },
                signature: signature(params, ret, span)?,
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
                signature: signature(params, ret, span)?,
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
                    op: binary_op(*op, span)?,
                    self_on_right: *self_on_right,
                },
                signature: ExternSignature {
                    params: vec![ExternParam {
                        name: None,
                        ty: type_expr(other_ty, span)?,
                        flow: ParamFlow::Value,
                    }],
                    ret: type_expr(ret, span)?,
                },
                effects: ExternEffects::default(),
            },
            site: RawExternSite { span: Some(span) },
        })),
        ExternTypeMember::UnaryOperator { op, ret } => {
            Ok(RawSourceMember::Operator(RawExternOperator {
                decl: ExternOperatorDescriptor {
                    op: ExternOperator::Unary(unary_op(*op, span)?),
                    signature: ExternSignature {
                        params: vec![],
                        ret: type_expr(ret, span)?,
                    },
                    effects: ExternEffects::default(),
                },
                site: RawExternSite { span: Some(span) },
            }))
        }
    }
}

fn signature(params: &[Param], ret: &Type, span: Span) -> SourceResult<ExternSignature> {
    Ok(ExternSignature {
        params: param_list(params, span)?,
        ret: type_expr(ret, span)?,
    })
}

fn param_list(params: &[Param], span: Span) -> SourceResult<Vec<ExternParam>> {
    params
        .iter()
        .map(|param| lower_param(param, span))
        .collect()
}

fn lower_param(param: &Param, span: Span) -> SourceResult<ExternParam> {
    if matches!(param.mutability, Mutability::Mutable) {
        return Err(unsupported_param(
            param,
            span,
            UnsupportedSourceParamReason::Mutable,
        ));
    }
    if param.cast_accept {
        return Err(unsupported_param(
            param,
            span,
            UnsupportedSourceParamReason::CastAccept,
        ));
    }
    if param.default.is_some() {
        return Err(unsupported_param(
            param,
            span,
            UnsupportedSourceParamReason::Default,
        ));
    }

    Ok(ExternParam {
        name: Some(param.name.to_string()),
        ty: type_expr(&param.ty, span)?,
        flow: ParamFlow::Value,
    })
}

fn unsupported_param(
    param: &Param,
    span: Span,
    reason: UnsupportedSourceParamReason,
) -> SourceError {
    Box::new(ExternInputError::UnsupportedSource {
        span,
        kind: UnsupportedSourceKind::Param {
            name: param.name.to_string(),
            reason,
        },
    })
}

fn type_expr(ty: &Type, span: Span) -> SourceResult<ExternTypeExpr> {
    if let Some(inner) = ty.option_inner() {
        return Ok(ExternTypeExpr::Option(Box::new(type_expr(inner, span)?)));
    }

    match ty {
        Type::Void => Ok(ExternTypeExpr::Void),
        Type::Bool => Ok(ExternTypeExpr::Bool),
        Type::Int => Ok(ExternTypeExpr::Int),
        Type::Float => Ok(ExternTypeExpr::Float),
        Type::String => Ok(ExternTypeExpr::String),
        Type::Any => Ok(ExternTypeExpr::Any),
        Type::List { elem } => Ok(ExternTypeExpr::List(Box::new(type_expr(elem, span)?))),
        Type::Map { key, value } => Ok(ExternTypeExpr::Map(
            Box::new(type_expr(key, span)?),
            Box::new(type_expr(value, span)?),
        )),
        Type::Func { params, ret } => {
            if params.iter().any(|param| param.mutable) {
                return Err(unsupported_callback_param(
                    span,
                    UnsupportedSourceParamReason::Mutable,
                ));
            }

            Ok(ExternTypeExpr::Callback(ExternCallbackSignature {
                params: params
                    .iter()
                    .map(|param| type_expr(&param.ty, span))
                    .collect::<SourceResult<Vec<_>>>()?,
                ret: Box::new(type_expr(ret, span)?),
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
            args: type_args(generic_args, span)?,
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
                .map(|arg| type_expr(arg, span))
                .collect::<SourceResult<Vec<_>>>()?,
        }),
        _ => Err(unsupported_type(ty, span)),
    }
}

fn type_args(args: &[GenericArg], span: Span) -> SourceResult<Vec<ExternTypeExpr>> {
    args.iter()
        .map(|arg| match arg {
            GenericArg::Type(ty) => type_expr(ty, span),
            GenericArg::Const(_) => Err(unsupported_type(
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

fn unsupported_type(ty: &Type, span: Span) -> SourceError {
    Box::new(ExternInputError::UnsupportedSource {
        span,
        kind: UnsupportedSourceKind::Type(ty.to_string()),
    })
}

fn unsupported_callback_param(span: Span, reason: UnsupportedSourceParamReason) -> SourceError {
    Box::new(ExternInputError::UnsupportedSource {
        span,
        kind: UnsupportedSourceKind::CallbackParam { reason },
    })
}

fn binary_op(op: BinaryOp, span: Span) -> SourceResult<anvyx_externs::BinaryOp> {
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
        _ => Err(unsupported_operator(op, span)),
    }
}

fn unary_op(op: UnaryOp, span: Span) -> SourceResult<anvyx_externs::UnaryOp> {
    match op {
        UnaryOp::Neg => Ok(anvyx_externs::UnaryOp::Neg),
        _ => Err(unsupported_operator(op, span)),
    }
}

fn unsupported_operator(op: impl std::fmt::Display, span: Span) -> SourceError {
    Box::new(ExternInputError::UnsupportedSource {
        span,
        kind: UnsupportedSourceKind::Operator(op.to_string()),
    })
}
