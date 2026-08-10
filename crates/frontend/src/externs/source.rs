use std::collections::{HashMap, hash_map::Entry};

use anvyx_externs::{
    CallbackEscape, CallbackPolicy, CallbackThread, ExternCallbackParam, ExternCallbackSignature,
    ExternDescriptorError, ExternEffects, ExternFieldDescriptor, ExternFunctionDescriptor,
    ExternInitDescriptor, ExternMemberSelector, ExternMethodDescriptor, ExternModuleDescriptor,
    ExternOperator, ExternOperatorDescriptor, ExternParam, ExternRep, ExternSignature,
    ExternStaticDescriptor, ExternTypeDescriptor, ExternTypeExpr, ModulePath, ParamFlow,
    ReceiverMode, validate_module_contents,
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
    let mut collector = SourceExternCollector::default();
    collect_source_program(
        resolved.root.clone(),
        resolved.root_source,
        root,
        &mut collector,
    );

    for module in resolved.module_groups.iter().flatten() {
        if module.key != resolved.root {
            collect_source_program(
                module.key.clone(),
                module.source,
                &module.program,
                &mut collector,
            );
        }
    }

    if collector.errors.is_empty() {
        Ok(RawExterns {
            modules: collector.modules,
        })
    } else {
        Err(collector.errors)
    }
}

#[derive(Default)]
struct SourceExternCollector {
    modules: Vec<RawExternModule>,
    identities: HashMap<(ModuleId, RawExternIdentityKey), RawExternDecl>,
    errors: Vec<ExternInputError>,
}

impl SourceExternCollector {
    fn validate(&mut self, module: RawExternModule) {
        self.validate_shape(&module);
        self.validate_identities(&module);
        self.modules.push(module);
    }

    fn validate_shape(&mut self, module: &RawExternModule) {
        let descriptor = ExternModuleDescriptor {
            path: ModulePath {
                segments: module
                    .scope
                    .module_path()
                    .map_or_else(Vec::new, |path| path.segments().to_vec()),
            },
            types: module.types.iter().map(|ty| ty.decl.clone()).collect(),
            functions: module
                .functions
                .iter()
                .map(|function| function.decl.clone())
                .collect(),
        };
        let Err(errors) = validate_module_contents(&descriptor) else {
            return;
        };
        let decl = RawExternDecl {
            scope: module.scope.clone(),
            site: module_site(module),
        };
        self.errors.extend(
            errors
                .into_iter()
                .filter(|error| !is_duplicate_error(error))
                .map(|error| ExternInputError::InvalidRawDescriptor {
                    decl: decl.clone(),
                    error,
                }),
        );
    }

    fn validate_identities(&mut self, module: &RawExternModule) {
        for function in &module.functions {
            self.insert_identity(
                module.scope.clone(),
                RawExternIdentityKey::Function(function.decl.name.clone()),
                RawExternDecl {
                    scope: module.scope.clone(),
                    site: function.site,
                },
            );
        }
        for ty in &module.types {
            let owner = ty.decl.name.clone();
            let decl = RawExternDecl {
                scope: module.scope.clone(),
                site: ty.site,
            };
            self.insert_identity(
                module.scope.clone(),
                RawExternIdentityKey::Type(owner.clone()),
                decl.clone(),
            );
            for field in &ty.decl.fields {
                self.insert_member(
                    &owner,
                    ExternMemberSelector::Field(field.name.clone()),
                    decl.clone(),
                );
            }
            if ty.decl.init.is_some() {
                self.insert_member(&owner, ExternMemberSelector::Init, decl.clone());
            }
            for method in &ty.decl.methods {
                self.insert_member(
                    &owner,
                    ExternMemberSelector::Method(method.name.clone()),
                    decl.clone(),
                );
            }
            for method in &ty.decl.statics {
                self.insert_member(
                    &owner,
                    ExternMemberSelector::Static(method.name.clone()),
                    decl.clone(),
                );
            }
            for operator in &ty.decl.operators {
                self.insert_member(
                    &owner,
                    ExternMemberSelector::Operator(operator.op),
                    decl.clone(),
                );
            }
        }
    }

    fn insert_member(&mut self, owner: &str, selector: ExternMemberSelector, decl: RawExternDecl) {
        self.insert_identity(
            decl.scope.clone(),
            RawExternIdentityKey::Member {
                owner: owner.to_string(),
                selector,
            },
            decl,
        );
    }

    fn insert_identity(
        &mut self,
        scope: ModuleId,
        key: RawExternIdentityKey,
        duplicate: RawExternDecl,
    ) {
        match self.identities.entry((scope, key.clone())) {
            Entry::Vacant(entry) => {
                entry.insert(duplicate);
            }
            Entry::Occupied(entry) => self.errors.push(ExternInputError::DuplicateRawIdentity {
                key,
                first: entry.get().clone(),
                duplicate,
            }),
        }
    }
}

fn module_site(module: &RawExternModule) -> RawExternSite {
    module
        .functions
        .first()
        .map(|function| function.site)
        .or_else(|| module.types.first().map(|ty| ty.site))
        .unwrap_or_default()
}

fn is_duplicate_error(error: &ExternDescriptorError) -> bool {
    matches!(
        error,
        ExternDescriptorError::DuplicateModule(_)
            | ExternDescriptorError::DuplicateType { .. }
            | ExternDescriptorError::DuplicateFunction { .. }
            | ExternDescriptorError::DuplicateField { .. }
            | ExternDescriptorError::DuplicateMethod { .. }
            | ExternDescriptorError::DuplicateStatic { .. }
            | ExternDescriptorError::DuplicateOperator { .. }
    )
}

fn collect_source_program(
    scope: ModuleId,
    source: SourceId,
    program: &Program,
    collector: &mut SourceExternCollector,
) {
    let mut types = vec![];
    let mut functions = vec![];

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::ExternFunc(func) => match normalize_function(source, func) {
                Ok(func) => functions.push(func),
                Err(error) => collector.errors.push(*error),
            },
            Stmt::ExternType(ty) => match normalize_type(source, ty) {
                Ok(ty) => types.push(ty),
                Err(type_errors) => collector.errors.extend(type_errors),
            },
            _ => {}
        }
    }

    if types.is_empty() && functions.is_empty() {
        return;
    }

    collector.validate(RawExternModule {
        scope,
        types,
        functions,
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

    let init = ty.node.init.as_ref().map(|init| {
        let mut params = vec![];
        let mut field_init = vec![];
        for param in &init.params {
            match lower_param(source, param, span) {
                Ok(param) => {
                    field_init.push(param.name.clone().expect("source params are named"));
                    params.push(param);
                }
                Err(error) => errors.push(*error),
            }
        }
        ExternInitDescriptor {
            params,
            field_init,
            presence_init: vec![],
            ret: ExternTypeExpr::Void,
            effects: ExternEffects::default(),
        }
    });

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(RawExternType {
        decl: ExternTypeDescriptor {
            name: ty.node.name.to_string(),
            doc: ty.node.doc.clone(),
            rep: match ty.node.rep {
                ExternTypeRep::Shared => ExternRep::Shared,
                ExternTypeRep::Inline => ExternRep::Inline,
            },
            layout: None,
            materialization: None,
            owns_heap_edges: None,
            fields,
            variants: vec![],
            init,
            methods,
            statics,
            operators,
        },
        exported: matches!(ty.node.visibility, Visibility::Public),
        site: site(source, span),
    })
}

enum RawSourceMember {
    Field(ExternFieldDescriptor),
    Method(ExternMethodDescriptor),
    Static(ExternStaticDescriptor),
    Operator(ExternOperatorDescriptor),
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
        } => Ok(RawSourceMember::Field(ExternFieldDescriptor {
            name: name.to_string(),
            ty: type_expr(source, ty, span, SourceTypePosition::Field)?,
            computed: *computed,
            readable: true,
            writable: true,
            get_receiver: ReceiverMode::Shared,
            set_receiver: ReceiverMode::Mutable,
            doc: doc.clone(),
        })),
        ExternTypeMember::Method {
            doc,
            name,
            receiver,
            params,
            ret,
        } => Ok(RawSourceMember::Method(ExternMethodDescriptor {
            name: name.to_string(),
            doc: doc.clone(),
            receiver: match receiver {
                ExternReceiverMode::Value => ReceiverMode::Value,
                ExternReceiverMode::Shared => ReceiverMode::Shared,
                ExternReceiverMode::Mutable => ReceiverMode::Mutable,
            },
            signature: signature(source, params, ret, span)?,
            effects: ExternEffects::default(),
        })),
        ExternTypeMember::StaticMethod {
            doc,
            name,
            params,
            ret,
        } => Ok(RawSourceMember::Static(ExternStaticDescriptor {
            name: name.to_string(),
            doc: doc.clone(),
            signature: signature(source, params, ret, span)?,
            effects: ExternEffects::default(),
        })),
        ExternTypeMember::Operator {
            op,
            other_ty,
            ret,
            self_on_right,
        } => Ok(RawSourceMember::Operator(ExternOperatorDescriptor {
            op: ExternOperator::Binary {
                op: binary_op(source, *op, span)?,
                self_on_right: *self_on_right,
            },
            receiver: ReceiverMode::Shared,
            signature: ExternSignature {
                params: vec![ExternParam {
                    name: None,
                    ty: type_expr(source, other_ty, span, SourceTypePosition::Param)?,
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                }],
                ret: type_expr(source, ret, span, SourceTypePosition::Return)?,
            },
            effects: ExternEffects::default(),
        })),
        ExternTypeMember::UnaryOperator { op, ret } => {
            Ok(RawSourceMember::Operator(ExternOperatorDescriptor {
                op: ExternOperator::Unary(unary_op(source, *op, span)?),
                receiver: ReceiverMode::Shared,
                signature: ExternSignature {
                    params: vec![],
                    ret: type_expr(source, ret, span, SourceTypePosition::Return)?,
                },
                effects: ExternEffects::default(),
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
    if ret.is_place() {
        return Err(Box::new(ExternInputError::UnsupportedSource {
            span: SourceSpan::from_byte_span(source, span),
            kind: UnsupportedSourceKind::ReturnPlace,
        }));
    }
    Ok(ExternSignature {
        params: param_list(source, params, span)?,
        ret: type_expr(source, &ret.ty(), span, SourceTypePosition::Return)?,
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

    let escape = callback_escape(param.escape);
    Ok(ExternParam {
        name: Some(param.name.to_string()),
        ty: callback_param_type(
            type_expr(source, &param.ty, span, SourceTypePosition::Param)?,
            escape,
        ),
        flow: ParamFlow::Value,
        escape,
    })
}

fn callback_param_type(mut ty: ExternTypeExpr, escape: CallbackEscape) -> ExternTypeExpr {
    if let ExternTypeExpr::Callback(callback) = &mut ty {
        callback.policy.escape = escape;
    }
    ty
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum SourceTypePosition {
    Param,
    Return,
    CallbackParam,
    CallbackReturn,
    Field,
    Nested,
}

impl SourceTypePosition {
    fn allows_slice(self) -> bool {
        matches!(self, Self::Param | Self::CallbackParam)
    }

    fn allows_callback(self) -> bool {
        matches!(self, Self::Param | Self::CallbackParam)
    }
}

fn type_expr(
    source: SourceId,
    ty: &Type,
    span: Span,
    position: SourceTypePosition,
) -> SourceResult<ExternTypeExpr> {
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
        Type::Char => Ok(ExternTypeExpr::Char),
        Type::Any => Ok(ExternTypeExpr::Any),
        Type::Tuple(fields) if fields.is_empty() => Ok(ExternTypeExpr::Unit),
        Type::Tuple(fields) => fields
            .iter()
            .map(|field| type_expr(source, field, span, SourceTypePosition::Nested))
            .collect::<SourceResult<Vec<_>>>()
            .map(ExternTypeExpr::Tuple),
        Type::Array { elem, len } => match len {
            ast::ArrayLen::Fixed(len) => Ok(ExternTypeExpr::array(
                type_expr(source, elem, span, SourceTypePosition::Nested)?,
                *len.value() as u64,
            )),
            ast::ArrayLen::Infer
            | ast::ArrayLen::Named(_)
            | ast::ArrayLen::Param(_)
            | ast::ArrayLen::Expr(_) => Err(unsupported_type(source, ty, span)),
        },
        Type::Slice { elem } => {
            if !position.allows_slice() {
                return Err(unsupported_type(source, ty, span));
            }
            Ok(ExternTypeExpr::slice(type_expr(
                source,
                elem,
                span,
                SourceTypePosition::Nested,
            )?))
        }
        Type::Optional { inner } => Ok(ExternTypeExpr::option(type_expr(
            source,
            inner,
            span,
            SourceTypePosition::Nested,
        )?)),
        Type::List { elem } => Ok(ExternTypeExpr::list(type_expr(
            source,
            elem,
            span,
            SourceTypePosition::Nested,
        )?)),
        Type::Map { key, value } => Ok(ExternTypeExpr::map(
            type_expr(source, key, span, SourceTypePosition::Nested)?,
            type_expr(source, value, span, SourceTypePosition::Nested)?,
        )),
        Type::Func { params, ret } => {
            if matches!(
                position,
                SourceTypePosition::Return | SourceTypePosition::CallbackReturn
            ) {
                return Err(Box::new(ExternInputError::UnsupportedSource {
                    span: SourceSpan::from_byte_span(source, span),
                    kind: UnsupportedSourceKind::CallbackReturn,
                }));
            }
            if !position.allows_callback() {
                return Err(unsupported_type(source, ty, span));
            }
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
                        let escape = callback_escape(param.escape);
                        Ok(ExternCallbackParam {
                            ty: callback_param_type(
                                type_expr(
                                    source,
                                    &param.ty,
                                    span,
                                    SourceTypePosition::CallbackParam,
                                )?,
                                escape,
                            ),
                            escape,
                        })
                    })
                    .collect::<SourceResult<Vec<_>>>()?,
                ret: Box::new(type_expr(
                    source,
                    &ret.ty(),
                    span,
                    SourceTypePosition::CallbackReturn,
                )?),
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
                .map(|arg| type_expr(source, arg, span, SourceTypePosition::Nested))
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
            GenericArg::Type(ty) => type_expr(source, ty, span, SourceTypePosition::Nested),
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
