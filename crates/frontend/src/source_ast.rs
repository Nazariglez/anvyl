use std::collections::{HashMap, HashSet};

use crate::{
    ast::{self, ExprId, ExprKind, ExprNode, Stmt, StmtNode},
    resolve::ResolveResult,
    semantic_id::SourceDeclId,
    source::SourceId,
    typecheck::{DeclarationIndex, DefaultExprSite, ExtendId, GlobalKey, ModuleScope},
};

#[derive(Debug, Clone)]
pub(crate) enum SourceDecl {
    Function(ast::FuncNode),
    Aggregate(ast::AggregateDeclNode),
    Enum(ast::EnumDeclNode),
    Extend(ast::ExtendDeclNode),
}

#[derive(Debug, Clone)]
struct SourceDeclaration {
    module: usize,
    node: SourceDecl,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum SourceCallableSite {
    Function(SourceDeclId),
    AggregateMethod {
        owner: SourceDeclId,
        method: SourceDeclId,
    },
    ExtendMethod {
        owner: SourceDeclId,
        method: SourceDeclId,
    },
}

impl SourceCallableSite {
    pub(crate) fn owner(self) -> SourceDeclId {
        match self {
            Self::Function(id) => id,
            Self::AggregateMethod { owner, .. } | Self::ExtendMethod { owner, .. } => owner,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum SourceCallable<'a> {
    Function(&'a ast::FuncNode),
    AggregateMethod {
        owner: ast::Ident,
        method: &'a ast::Method,
    },
    ExtendMethod(&'a ast::ExtendMethod),
}

impl<'a> SourceCallable<'a> {
    pub(crate) fn name(self) -> ast::Ident {
        match self {
            Self::Function(func) => func.node.name,
            Self::AggregateMethod { method, .. } => method.sig.name,
            Self::ExtendMethod(method) => method.sig.name,
        }
    }

    pub(crate) fn body(self) -> &'a ast::BlockNode {
        match self {
            Self::Function(func) => &func.node.body,
            Self::AggregateMethod { method, .. } => &method.body,
            Self::ExtendMethod(method) => &method.body,
        }
    }

    pub(crate) fn params(self) -> Vec<ast::Ident> {
        let params = match self {
            Self::Function(func) => &func.node.params,
            Self::AggregateMethod { method, .. } => &method.sig.params,
            Self::ExtendMethod(method) => &method.sig.params,
        };
        params.iter().map(|param| param.name).collect()
    }

    pub(crate) fn has_generics(self) -> bool {
        let (types, consts) = match self {
            Self::Function(func) => (&func.node.type_params, &func.node.const_params),
            Self::AggregateMethod { method, .. } => {
                (&method.sig.type_params, &method.sig.const_params)
            }
            Self::ExtendMethod(method) => (&method.sig.type_params, &method.sig.const_params),
        };
        !types.is_empty() || !consts.is_empty()
    }

    pub(crate) fn receiver(self) -> Option<ast::MethodReceiver> {
        match self {
            Self::Function(_) => None,
            Self::AggregateMethod { method, .. } => method.sig.receiver,
            Self::ExtendMethod(method) => method.sig.receiver,
        }
    }

    pub(crate) fn owner(self) -> Option<ast::Ident> {
        match self {
            Self::AggregateMethod { owner, .. } => Some(owner),
            Self::Function(_) | Self::ExtendMethod(_) => None,
        }
    }

    pub(crate) fn is_method(self) -> bool {
        !matches!(self, Self::Function(_))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum SourceExprOwner {
    Declaration(SourceDeclId),
    Global(GlobalKey),
}

#[derive(Debug, Clone)]
pub(crate) struct SourceGlobal {
    pub(crate) module: usize,
    pub(crate) source: SourceId,
    pub(crate) node: ast::GlobalDeclNode,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct SourceCastFrom<'a> {
    pub(crate) module: usize,
    pub(crate) source: SourceId,
    pub(crate) node: &'a ast::CastFromNode,
}

#[derive(Debug, Clone)]
pub(crate) struct SourceAstIndex {
    pub(crate) modules: Vec<ModuleScope>,
    declarations: HashMap<SourceDeclId, SourceDeclaration>,
    pub(crate) globals: HashMap<GlobalKey, SourceGlobal>,
    cast_froms: HashMap<(ExtendId, usize), SourceDeclId>,
    lambdas: HashMap<(SourceId, ExprId), SourceExprOwner>,
    default_exprs: HashMap<DefaultExprSite, SourceExprOwner>,
    expression_ids: HashMap<DefaultExprSite, HashSet<ExprId>>,
    default_expr_stack: Vec<DefaultExprSite>,
    expression_owner: Option<SourceExprOwner>,
    module_declarations: HashSet<SourceDeclId>,
}

impl SourceAstIndex {
    pub(crate) fn new(root: &ast::Program, resolved: &ResolveResult) -> Self {
        let source_modules = DeclarationIndex::source_modules(root, resolved);
        let modules = source_modules
            .iter()
            .map(|module| module.scope.clone())
            .collect();
        let mut index = Self {
            modules,
            declarations: HashMap::new(),
            globals: HashMap::new(),
            cast_froms: HashMap::new(),
            lambdas: HashMap::new(),
            default_exprs: HashMap::new(),
            expression_ids: HashMap::new(),
            default_expr_stack: vec![],
            expression_owner: None,
            module_declarations: HashSet::new(),
        };
        for (module, source_module) in source_modules.into_iter().enumerate() {
            let mut extend_index = 0;
            for stmt in source_module.program.stmts.clone() {
                let declaration_span = match &stmt.node {
                    Stmt::Func(node) => Some(node.span),
                    Stmt::Aggregate(node) => Some(node.span),
                    Stmt::Enum(node) => Some(node.span),
                    Stmt::Extend(node) => Some(node.span),
                    _ => None,
                };
                if let Some(span) = declaration_span {
                    index
                        .module_declarations
                        .insert(SourceDeclId::new(source_module.source, span));
                }
                index.index_stmt(
                    stmt,
                    module,
                    source_module.source,
                    &source_module.scope,
                    &mut extend_index,
                );
            }
        }
        index
    }

    pub(crate) fn declaration_module(&self, id: &SourceDeclId) -> Option<usize> {
        self.declarations.get(id).map(|decl| decl.module)
    }

    pub(crate) fn callable(&self, site: &SourceCallableSite) -> Option<SourceCallable<'_>> {
        match *site {
            SourceCallableSite::Function(id) => {
                let SourceDecl::Function(func) = &self.declarations.get(&id)?.node else {
                    return None;
                };
                Some(SourceCallable::Function(func))
            }
            SourceCallableSite::AggregateMethod { owner, method } => {
                let SourceDecl::Aggregate(aggregate) = &self.declarations.get(&owner)?.node else {
                    return None;
                };
                let method = aggregate
                    .node
                    .methods
                    .iter()
                    .find(|candidate| candidate.span == method.span())?;
                Some(SourceCallable::AggregateMethod {
                    owner: aggregate.node.name,
                    method,
                })
            }
            SourceCallableSite::ExtendMethod { owner, method } => {
                let SourceDecl::Extend(extend) = &self.declarations.get(&owner)?.node else {
                    return None;
                };
                let method = extend
                    .node
                    .methods
                    .iter()
                    .find(|candidate| candidate.span == method.span())?;
                Some(SourceCallable::ExtendMethod(&method.node))
            }
        }
    }

    pub(crate) fn is_module_declaration(&self, id: &SourceDeclId) -> bool {
        self.module_declarations.contains(id)
    }

    pub(crate) fn cast_from(&self, id: &ExtendId, slot: usize) -> Option<SourceCastFrom<'_>> {
        let owner = self.cast_froms.get(&(id.clone(), slot))?;
        let declaration = self.declarations.get(owner)?;
        let SourceDecl::Extend(extend) = &declaration.node else {
            return None;
        };
        Some(SourceCastFrom {
            module: declaration.module,
            source: owner.source(),
            node: extend.node.cast_froms.get(slot)?,
        })
    }

    pub(crate) fn default_expr(&self, site: DefaultExprSite) -> Option<&ExprNode> {
        let owner = self.default_exprs.get(&site)?;
        self.find_expr(owner, site.expr)
    }

    pub(crate) fn lambda(&self, source: SourceId, id: ExprId) -> Option<&ast::LambdaNode> {
        let owner = self.lambdas.get(&(source, id))?;
        let expr = self.find_expr(owner, id)?;
        let ExprKind::Lambda(lambda) = &expr.node.kind else {
            unreachable!("lambda selector must resolve to a lambda expression")
        };
        Some(lambda)
    }

    pub(crate) fn expression_ids(&self, site: DefaultExprSite) -> Option<&HashSet<ExprId>> {
        self.expression_ids.get(&site)
    }

    pub(crate) fn declarations(&self) -> impl Iterator<Item = (SourceDeclId, usize, &SourceDecl)> {
        let mut ids = self.declarations.keys().cloned().collect::<Vec<_>>();
        ids.sort_by_key(|id| {
            let span = id.span();
            (id.source().index(), span.start, span.end)
        });
        ids.into_iter().map(|id| {
            let decl = &self.declarations[&id];
            (id, decl.module, &decl.node)
        })
    }

    fn insert_declaration(&mut self, id: SourceDeclId, module: usize, node: SourceDecl) {
        let old = self
            .declarations
            .insert(id, SourceDeclaration { module, node });
        debug_assert!(old.is_none());
    }

    fn index_stmt(
        &mut self,
        mut stmt: StmtNode,
        module: usize,
        source: SourceId,
        scope: &ModuleScope,
        extend_index: &mut usize,
    ) -> Option<StmtNode> {
        let owned_body = matches!(
            &stmt.node,
            Stmt::Func(_) | Stmt::Aggregate(_) | Stmt::Enum(_) | Stmt::Extend(_) | Stmt::Global(_)
        );
        let saved_default_stack = owned_body.then(|| std::mem::take(&mut self.default_expr_stack));
        let saved_owner = owned_body.then(|| self.expression_owner.take());
        if owned_body {
            self.expression_owner = match &stmt.node {
                Stmt::Func(func) => Some(SourceExprOwner::Declaration(SourceDeclId::new(
                    source, func.span,
                ))),
                Stmt::Aggregate(aggregate) => Some(SourceExprOwner::Declaration(
                    SourceDeclId::new(source, aggregate.span),
                )),
                Stmt::Enum(enm) => Some(SourceExprOwner::Declaration(SourceDeclId::new(
                    source, enm.span,
                ))),
                Stmt::Extend(extend) => Some(SourceExprOwner::Declaration(SourceDeclId::new(
                    source,
                    extend.span,
                ))),
                Stmt::Global(global) => Some(SourceExprOwner::Global(GlobalKey {
                    module: scope.clone(),
                    name: global.node.name,
                })),
                _ => None,
            };
        }
        match &mut stmt.node {
            Stmt::Func(func) => {
                self.index_param_defaults(
                    source,
                    &mut func.node.params,
                    module,
                    scope,
                    extend_index,
                );
                self.index_block(&mut func.node.body, module, source, scope, extend_index);
            }
            Stmt::Aggregate(aggregate) => {
                for field in &mut aggregate.node.fields {
                    if let Some(default) = &mut field.default {
                        self.index_default(source, default, module, scope, extend_index);
                    }
                }
                for method in &mut aggregate.node.methods {
                    self.index_param_defaults(
                        source,
                        &mut method.sig.params,
                        module,
                        scope,
                        extend_index,
                    );
                    self.index_block(&mut method.body, module, source, scope, extend_index);
                }
            }
            Stmt::Enum(enm) => {
                for variant in &mut enm.node.variants {
                    if let Some(value) = &mut variant.raw_value {
                        self.index_expr(value, module, source, scope, extend_index);
                    }
                }
            }
            Stmt::Extend(extend) => {
                let id = SourceDeclId::new(source, extend.span);
                let extend_id = ExtendId {
                    module: scope.clone(),
                    index: *extend_index,
                };
                *extend_index += 1;
                for (slot, cast) in extend.node.cast_froms.iter_mut().enumerate() {
                    self.cast_froms.insert((extend_id.clone(), slot), id);
                    self.index_block(&mut cast.node.body, module, source, scope, extend_index);
                }
                for method in &mut extend.node.methods {
                    self.index_param_defaults(
                        source,
                        &mut method.node.sig.params,
                        module,
                        scope,
                        extend_index,
                    );
                    self.index_block(&mut method.node.body, module, source, scope, extend_index);
                }
            }
            Stmt::Global(global) => {
                self.index_expr(&mut global.node.value, module, source, scope, extend_index)
            }
            Stmt::Expr(expr) => self.index_expr(expr, module, source, scope, extend_index),
            Stmt::Binding(binding) => {
                self.index_expr(&mut binding.node.value, module, source, scope, extend_index)
            }
            Stmt::LetElse(let_else) => {
                self.index_expr(
                    &mut let_else.node.value,
                    module,
                    source,
                    scope,
                    extend_index,
                );
                match &mut let_else.node.fallback.node {
                    ast::LetElseFallback::Block(block) => {
                        self.index_block(block, module, source, scope, extend_index)
                    }
                    ast::LetElseFallback::Return(ret) => {
                        if let Some(value) = &mut ret.node.value {
                            self.index_expr(value, module, source, scope, extend_index);
                        }
                    }
                    ast::LetElseFallback::Break | ast::LetElseFallback::Continue => {}
                }
            }
            Stmt::Return(ret) => {
                if let Some(value) = &mut ret.node.value {
                    self.index_expr(value, module, source, scope, extend_index);
                }
            }
            Stmt::While(while_) => {
                self.index_expr(&mut while_.node.cond, module, source, scope, extend_index);
                self.index_block(&mut while_.node.body, module, source, scope, extend_index);
            }
            Stmt::WhileLet(while_) => {
                self.index_expr(&mut while_.node.value, module, source, scope, extend_index);
                self.index_block(&mut while_.node.body, module, source, scope, extend_index);
            }
            Stmt::For(for_) => {
                self.index_expr(&mut for_.node.iterable, module, source, scope, extend_index);
                self.index_block(&mut for_.node.body, module, source, scope, extend_index);
            }
            Stmt::Defer(defer) => match &mut defer.node.body {
                ast::DeferBody::Expr(expr) => {
                    self.index_expr(expr, module, source, scope, extend_index)
                }
                ast::DeferBody::Block(block) => {
                    self.index_block(block, module, source, scope, extend_index)
                }
            },
            Stmt::Import(_)
            | Stmt::ExternFunc(_)
            | Stmt::ExternType(_)
            | Stmt::Const(_)
            | Stmt::TypeAlias(_)
            | Stmt::Contract(_)
            | Stmt::Break
            | Stmt::Continue => {}
        }
        if let Some(stack) = saved_default_stack {
            debug_assert!(self.default_expr_stack.is_empty());
            self.default_expr_stack = stack;
            self.expression_owner = saved_owner.flatten();
        }

        match stmt.node {
            Stmt::Func(node) => {
                self.insert_declaration(
                    SourceDeclId::new(source, node.span),
                    module,
                    SourceDecl::Function(node),
                );
                None
            }
            Stmt::Aggregate(node) => {
                self.insert_declaration(
                    SourceDeclId::new(source, node.span),
                    module,
                    SourceDecl::Aggregate(node),
                );
                None
            }
            Stmt::Enum(node) => {
                self.insert_declaration(
                    SourceDeclId::new(source, node.span),
                    module,
                    SourceDecl::Enum(node),
                );
                None
            }
            Stmt::Extend(node) => {
                self.insert_declaration(
                    SourceDeclId::new(source, node.span),
                    module,
                    SourceDecl::Extend(node),
                );
                None
            }
            Stmt::Global(node) => {
                self.globals.insert(
                    GlobalKey {
                        module: scope.clone(),
                        name: node.node.name,
                    },
                    SourceGlobal {
                        module,
                        source,
                        node,
                    },
                );
                None
            }
            node => {
                stmt.node = node;
                Some(stmt)
            }
        }
    }

    fn index_block(
        &mut self,
        block: &mut ast::BlockNode,
        module: usize,
        source: SourceId,
        scope: &ModuleScope,
        extend_index: &mut usize,
    ) {
        block.node.stmts = std::mem::take(&mut block.node.stmts)
            .into_iter()
            .filter_map(|stmt| self.index_stmt(stmt, module, source, scope, extend_index))
            .collect();
        if let Some(tail) = &mut block.node.tail {
            self.index_expr(tail, module, source, scope, extend_index);
        }
    }

    fn index_default(
        &mut self,
        source: SourceId,
        expr: &mut ExprNode,
        module: usize,
        scope: &ModuleScope,
        extend_index: &mut usize,
    ) {
        let site = DefaultExprSite {
            source,
            expr: expr.node.id,
        };
        let owner = self
            .expression_owner
            .clone()
            .expect("default expression must belong to a source declaration");
        let old = self.default_exprs.insert(site, owner);
        debug_assert!(old.is_none());
        self.expression_ids
            .entry(site)
            .or_default()
            .insert(expr.node.id);
        self.default_expr_stack.push(site);
        self.index_expr(expr, module, source, scope, extend_index);
        let popped = self.default_expr_stack.pop();
        debug_assert_eq!(popped, Some(site));
    }

    fn index_param_defaults(
        &mut self,
        source: SourceId,
        params: &mut [ast::Param],
        module: usize,
        scope: &ModuleScope,
        extend_index: &mut usize,
    ) {
        for param in params {
            if let Some(default) = &mut param.default {
                self.index_default(source, default, module, scope, extend_index);
            }
        }
    }

    fn find_expr(&self, owner: &SourceExprOwner, id: ExprId) -> Option<&ExprNode> {
        match owner {
            SourceExprOwner::Declaration(declaration) => {
                find_expr_in_declaration(&self.declarations[declaration].node, id)
            }
            SourceExprOwner::Global(key) => find_expr(&self.globals[key].node.node.value, id),
        }
    }

    fn index_expr(
        &mut self,
        expr: &mut ExprNode,
        module: usize,
        source: SourceId,
        scope: &ModuleScope,
        extend_index: &mut usize,
    ) {
        for site in &self.default_expr_stack {
            self.expression_ids
                .entry(*site)
                .or_default()
                .insert(expr.node.id);
        }
        if matches!(&expr.node.kind, ExprKind::Lambda(_)) {
            let owner = self
                .expression_owner
                .clone()
                .expect("lambda must belong to a source declaration");
            let old = self.lambdas.insert((source, expr.node.id), owner);
            debug_assert!(old.is_none());
        }
        match &mut expr.node.kind {
            ExprKind::Block(block) => self.index_block(block, module, source, scope, extend_index),
            ExprKind::Call(call) => {
                self.index_expr(&mut call.node.func, module, source, scope, extend_index);
                for arg in &mut call.node.args {
                    self.index_expr(arg, module, source, scope, extend_index);
                }
            }
            ExprKind::IterSource(iter) => {
                self.index_expr(&mut iter.node.source, module, source, scope, extend_index)
            }
            ExprKind::Binary(binary) => {
                self.index_expr(&mut binary.node.left, module, source, scope, extend_index);
                self.index_expr(&mut binary.node.right, module, source, scope, extend_index);
            }
            ExprKind::Unary(unary) => {
                self.index_expr(&mut unary.node.expr, module, source, scope, extend_index)
            }
            ExprKind::Assign(assign) => {
                self.index_expr(&mut assign.node.target, module, source, scope, extend_index);
                self.index_expr(&mut assign.node.value, module, source, scope, extend_index);
            }
            ExprKind::If(if_) => {
                self.index_expr(&mut if_.node.cond, module, source, scope, extend_index);
                self.index_block(
                    &mut if_.node.then_block,
                    module,
                    source,
                    scope,
                    extend_index,
                );
                if let Some(block) = &mut if_.node.else_block {
                    self.index_block(block, module, source, scope, extend_index);
                }
            }
            ExprKind::Ternary(ternary) => {
                self.index_expr(&mut ternary.node.cond, module, source, scope, extend_index);
                self.index_expr(
                    &mut ternary.node.then_expr,
                    module,
                    source,
                    scope,
                    extend_index,
                );
                self.index_expr(
                    &mut ternary.node.else_expr,
                    module,
                    source,
                    scope,
                    extend_index,
                );
            }
            ExprKind::IfLet(if_) => {
                self.index_expr(&mut if_.node.value, module, source, scope, extend_index);
                self.index_block(
                    &mut if_.node.then_block,
                    module,
                    source,
                    scope,
                    extend_index,
                );
                if let Some(block) = &mut if_.node.else_block {
                    self.index_block(block, module, source, scope, extend_index);
                }
            }
            ExprKind::Tuple(items) => {
                for item in items {
                    self.index_expr(item, module, source, scope, extend_index);
                }
            }
            ExprKind::TupleIndex(tuple) => {
                self.index_expr(&mut tuple.node.target, module, source, scope, extend_index)
            }
            ExprKind::Field(field) => {
                self.index_expr(&mut field.node.target, module, source, scope, extend_index)
            }
            ExprKind::StructLiteral(literal) => {
                for (_, value) in &mut literal.node.fields {
                    self.index_expr(value, module, source, scope, extend_index);
                }
            }
            ExprKind::Range(range) => match &mut range.node {
                ast::Range::Bounded { start, end, .. } => {
                    self.index_expr(start, module, source, scope, extend_index);
                    self.index_expr(end, module, source, scope, extend_index);
                }
                ast::Range::From { start } => {
                    self.index_expr(start, module, source, scope, extend_index)
                }
                ast::Range::To { end, .. } => {
                    self.index_expr(end, module, source, scope, extend_index)
                }
            },
            ExprKind::ArrayLiteral(array) => {
                for item in &mut array.node.elements {
                    self.index_expr(item, module, source, scope, extend_index);
                }
            }
            ExprKind::ArrayFill(fill) => {
                self.index_expr(&mut fill.node.value, module, source, scope, extend_index);
                self.index_expr(&mut fill.node.len, module, source, scope, extend_index);
            }
            ExprKind::MapLiteral(map) => {
                for (key, value) in &mut map.node.entries {
                    self.index_expr(key, module, source, scope, extend_index);
                    self.index_expr(value, module, source, scope, extend_index);
                }
            }
            ExprKind::Index(index) => {
                self.index_expr(&mut index.node.target, module, source, scope, extend_index);
                self.index_expr(&mut index.node.index, module, source, scope, extend_index);
            }
            ExprKind::Match(match_) => {
                self.index_expr(
                    &mut match_.node.scrutinee,
                    module,
                    source,
                    scope,
                    extend_index,
                );
                for arm in &mut match_.node.arms {
                    self.index_expr(&mut arm.node.body, module, source, scope, extend_index);
                }
            }
            ExprKind::StringInterp(parts) => {
                for part in parts {
                    if let ast::StringPart::Expr(expr, _) = part {
                        self.index_expr(expr, module, source, scope, extend_index);
                    }
                }
            }
            ExprKind::Cast(cast) | ExprKind::FailableCast(cast) => {
                self.index_expr(&mut cast.node.expr, module, source, scope, extend_index)
            }
            ExprKind::Try(try_) => {
                self.index_expr(&mut try_.node.expr, module, source, scope, extend_index)
            }
            ExprKind::Lambda(lambda) => {
                self.index_expr(&mut lambda.node.body, module, source, scope, extend_index)
            }
            ExprKind::InferredEnum(inferred) => match &mut inferred.node.args {
                ast::InferredEnumArgs::Unit => {}
                ast::InferredEnumArgs::Tuple(args) => {
                    for arg in args {
                        self.index_expr(arg, module, source, scope, extend_index);
                    }
                }
                ast::InferredEnumArgs::Struct(fields) => {
                    for (_, value) in fields {
                        self.index_expr(value, module, source, scope, extend_index);
                    }
                }
            },
            ExprKind::IntrinsicCall(call) => {
                for arg in &mut call.node.args {
                    self.index_expr(arg, module, source, scope, extend_index);
                }
            }
            ExprKind::Ident(_) | ExprKind::TypeSubject(_) | ExprKind::Lit(_) => {}
        }
    }
}

fn find_expr_in_declaration(decl: &SourceDecl, id: ExprId) -> Option<&ExprNode> {
    match decl {
        SourceDecl::Function(func) => find_expr_in_params(&func.node.params, id)
            .or_else(|| find_expr_in_block(&func.node.body, id)),
        SourceDecl::Aggregate(aggregate) => {
            for field in &aggregate.node.fields {
                if let Some(expr) = field.default.as_ref().and_then(|expr| find_expr(expr, id)) {
                    return Some(expr);
                }
            }
            for method in &aggregate.node.methods {
                if let Some(expr) = find_expr_in_params(&method.sig.params, id)
                    .or_else(|| find_expr_in_block(&method.body, id))
                {
                    return Some(expr);
                }
            }
            None
        }
        SourceDecl::Enum(enm) => enm
            .node
            .variants
            .iter()
            .filter_map(|variant| variant.raw_value.as_ref())
            .find_map(|expr| find_expr(expr, id)),
        SourceDecl::Extend(extend) => {
            for cast in &extend.node.cast_froms {
                if let Some(expr) = find_expr_in_block(&cast.node.body, id) {
                    return Some(expr);
                }
            }
            for method in &extend.node.methods {
                if let Some(expr) = find_expr_in_params(&method.node.sig.params, id)
                    .or_else(|| find_expr_in_block(&method.node.body, id))
                {
                    return Some(expr);
                }
            }
            None
        }
    }
}

fn find_expr_in_params(params: &[ast::Param], id: ExprId) -> Option<&ExprNode> {
    params
        .iter()
        .filter_map(|param| param.default.as_ref())
        .find_map(|expr| find_expr(expr, id))
}

fn find_expr_in_block(block: &ast::BlockNode, id: ExprId) -> Option<&ExprNode> {
    block
        .node
        .stmts
        .iter()
        .find_map(|stmt| find_expr_in_stmt(stmt, id))
        .or_else(|| {
            block
                .node
                .tail
                .as_ref()
                .and_then(|expr| find_expr(expr, id))
        })
}

fn find_expr_in_stmt(stmt: &StmtNode, id: ExprId) -> Option<&ExprNode> {
    match &stmt.node {
        Stmt::Expr(expr) => find_expr(expr, id),
        Stmt::Binding(binding) => find_expr(&binding.node.value, id),
        Stmt::LetElse(let_else) => {
            find_expr(&let_else.node.value, id).or_else(|| match &let_else.node.fallback.node {
                ast::LetElseFallback::Block(block) => find_expr_in_block(block, id),
                ast::LetElseFallback::Return(ret) => {
                    ret.node.value.as_ref().and_then(|expr| find_expr(expr, id))
                }
                ast::LetElseFallback::Break | ast::LetElseFallback::Continue => None,
            })
        }
        Stmt::Return(ret) => ret.node.value.as_ref().and_then(|expr| find_expr(expr, id)),
        Stmt::While(while_) => {
            find_expr(&while_.node.cond, id).or_else(|| find_expr_in_block(&while_.node.body, id))
        }
        Stmt::WhileLet(while_) => {
            find_expr(&while_.node.value, id).or_else(|| find_expr_in_block(&while_.node.body, id))
        }
        Stmt::For(for_) => {
            find_expr(&for_.node.iterable, id).or_else(|| find_expr_in_block(&for_.node.body, id))
        }
        Stmt::Defer(defer) => match &defer.node.body {
            ast::DeferBody::Expr(expr) => find_expr(expr, id),
            ast::DeferBody::Block(block) => find_expr_in_block(block, id),
        },
        Stmt::Func(_)
        | Stmt::Aggregate(_)
        | Stmt::Enum(_)
        | Stmt::Extend(_)
        | Stmt::Global(_)
        | Stmt::Import(_)
        | Stmt::ExternFunc(_)
        | Stmt::ExternType(_)
        | Stmt::Const(_)
        | Stmt::TypeAlias(_)
        | Stmt::Contract(_)
        | Stmt::Break
        | Stmt::Continue => None,
    }
}

fn find_expr(expr: &ExprNode, id: ExprId) -> Option<&ExprNode> {
    if expr.node.id == id {
        return Some(expr);
    }
    match &expr.node.kind {
        ExprKind::Block(block) => find_expr_in_block(block, id),
        ExprKind::Call(call) => find_expr(&call.node.func, id)
            .or_else(|| call.node.args.iter().find_map(|arg| find_expr(arg, id))),
        ExprKind::IterSource(iter) => find_expr(&iter.node.source, id),
        ExprKind::Binary(binary) => {
            find_expr(&binary.node.left, id).or_else(|| find_expr(&binary.node.right, id))
        }
        ExprKind::Unary(unary) => find_expr(&unary.node.expr, id),
        ExprKind::Assign(assign) => {
            find_expr(&assign.node.target, id).or_else(|| find_expr(&assign.node.value, id))
        }
        ExprKind::If(if_) => find_expr(&if_.node.cond, id)
            .or_else(|| find_expr_in_block(&if_.node.then_block, id))
            .or_else(|| {
                if_.node
                    .else_block
                    .as_ref()
                    .and_then(|block| find_expr_in_block(block, id))
            }),
        ExprKind::Ternary(ternary) => find_expr(&ternary.node.cond, id)
            .or_else(|| find_expr(&ternary.node.then_expr, id))
            .or_else(|| find_expr(&ternary.node.else_expr, id)),
        ExprKind::IfLet(if_) => find_expr(&if_.node.value, id)
            .or_else(|| find_expr_in_block(&if_.node.then_block, id))
            .or_else(|| {
                if_.node
                    .else_block
                    .as_ref()
                    .and_then(|block| find_expr_in_block(block, id))
            }),
        ExprKind::Tuple(items) => items.iter().find_map(|item| find_expr(item, id)),
        ExprKind::TupleIndex(tuple) => find_expr(&tuple.node.target, id),
        ExprKind::Field(field) => find_expr(&field.node.target, id),
        ExprKind::StructLiteral(literal) => literal
            .node
            .fields
            .iter()
            .find_map(|(_, value)| find_expr(value, id)),
        ExprKind::Range(range) => match &range.node {
            ast::Range::Bounded { start, end, .. } => {
                find_expr(start, id).or_else(|| find_expr(end, id))
            }
            ast::Range::From { start } => find_expr(start, id),
            ast::Range::To { end, .. } => find_expr(end, id),
        },
        ExprKind::ArrayLiteral(array) => array
            .node
            .elements
            .iter()
            .find_map(|item| find_expr(item, id)),
        ExprKind::ArrayFill(fill) => {
            find_expr(&fill.node.value, id).or_else(|| find_expr(&fill.node.len, id))
        }
        ExprKind::MapLiteral(map) => map
            .node
            .entries
            .iter()
            .find_map(|(key, value)| find_expr(key, id).or_else(|| find_expr(value, id))),
        ExprKind::Index(index) => {
            find_expr(&index.node.target, id).or_else(|| find_expr(&index.node.index, id))
        }
        ExprKind::Match(match_) => find_expr(&match_.node.scrutinee, id).or_else(|| {
            match_
                .node
                .arms
                .iter()
                .find_map(|arm| find_expr(&arm.node.body, id))
        }),
        ExprKind::StringInterp(parts) => parts.iter().find_map(|part| match part {
            ast::StringPart::Expr(expr, _) => find_expr(expr, id),
            ast::StringPart::Text(_) => None,
        }),
        ExprKind::Cast(cast) | ExprKind::FailableCast(cast) => find_expr(&cast.node.expr, id),
        ExprKind::Try(try_) => find_expr(&try_.node.expr, id),
        ExprKind::Lambda(lambda) => find_expr(&lambda.node.body, id),
        ExprKind::InferredEnum(inferred) => match &inferred.node.args {
            ast::InferredEnumArgs::Unit => None,
            ast::InferredEnumArgs::Tuple(args) => args.iter().find_map(|arg| find_expr(arg, id)),
            ast::InferredEnumArgs::Struct(fields) => {
                fields.iter().find_map(|(_, value)| find_expr(value, id))
            }
        },
        ExprKind::IntrinsicCall(call) => call.node.args.iter().find_map(|arg| find_expr(arg, id)),
        ExprKind::Ident(_) | ExprKind::TypeSubject(_) | ExprKind::Lit(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::{parse_program, resolved_modules};

    #[test]
    fn declaration_owners_do_not_retain_nested_declaration_trees() {
        let root =
            parse_program("fn one() { fn two() { fn three() { fn four() { fn five() {} } } } }");
        let resolved = resolved_modules(&root, &[]);
        let index = SourceAstIndex::new(&root, &resolved);

        assert_eq!(index.declarations.len(), 5);
        for declaration in index.declarations.values() {
            let SourceDecl::Function(func) = &declaration.node else {
                panic!("expected function declaration")
            };
            assert!(
                func.node
                    .body
                    .node
                    .stmts
                    .iter()
                    .all(|stmt| !matches!(stmt.node, Stmt::Func(_)))
            );
        }
    }

    #[test]
    fn expression_lookup_keeps_source_identity() {
        let root = parse_program("fn main(value: int = (|| 1)()) {}");
        let mut resolved = resolved_modules(&root, &[("dep", "")]);
        resolved.module_groups[0][0].program = root.clone();
        let module_source = resolved.module_groups[0][0].source;
        let index = SourceAstIndex::new(&root, &resolved);
        let root_site = index
            .default_exprs
            .keys()
            .find(|site| site.source == resolved.root_source)
            .copied()
            .expect("missing root default");
        let module_site = DefaultExprSite {
            source: module_source,
            expr: root_site.expr,
        };
        let lambda = index
            .expression_ids(root_site)
            .expect("missing root descendants")
            .iter()
            .copied()
            .find(|id| index.lambda(resolved.root_source, *id).is_some())
            .expect("missing root lambda");

        assert!(index.expression_ids(module_site).is_some());
        assert!(index.lambda(module_source, lambda).is_some());
    }

    #[test]
    fn expression_descendants_are_indexed_only_for_default_roots() {
        let root = parse_program(
            "fn main(value: int = { fn nested() -> int { 1 } nested() }) {
                let ignored = 2 + 3;
                let read = || ignored;
            }",
        );
        let resolved = resolved_modules(&root, &[]);
        let index = SourceAstIndex::new(&root, &resolved);
        let main = index
            .declarations
            .values()
            .find_map(|decl| match &decl.node {
                SourceDecl::Function(func) if func.node.name == ast::Ident::new("main") => {
                    Some(func)
                }
                _ => None,
            })
            .expect("missing main declaration");
        let nested = index
            .declarations
            .values()
            .find_map(|decl| match &decl.node {
                SourceDecl::Function(func) if func.node.name == ast::Ident::new("nested") => {
                    Some(func)
                }
                _ => None,
            })
            .expect("missing nested declaration");
        let default = main.node.params[0]
            .default
            .as_ref()
            .expect("missing default expression");
        let Stmt::Binding(read) = &main.node.body.node.stmts[1].node else {
            panic!("expected lambda binding")
        };
        let nested_tail = nested
            .node
            .body
            .node
            .tail
            .as_ref()
            .expect("missing nested tail")
            .node
            .id;
        let site = *index
            .default_exprs
            .keys()
            .next()
            .expect("missing default site");
        let descendants = index
            .expression_ids(site)
            .expect("missing default descendants");

        assert!(descendants.contains(&default.node.id));
        assert!(!descendants.contains(&nested_tail));
        assert!(index.lambda(site.source, read.node.value.node.id).is_some());
        assert_eq!(
            index.default_expr(site).map(|expr| expr.node.id),
            Some(default.node.id)
        );
    }
}
