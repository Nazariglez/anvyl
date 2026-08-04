use anvyx_frontend::{
    air::{
        self, AirBody, Local, LocalKind, Mutability, Param, ParamEscape, ParamMode, ParamRole,
        Place, PlaceRoot,
    },
    ast::Ident,
};

pub(crate) fn structured_body(stmts: Vec<air::AirStmt>, tail: air::AirTail) -> AirBody {
    AirBody {
        block: air::AirBlock { stmts, tail },
    }
}

pub(crate) fn param(name: &str, ty: air::TypeId, mode: ParamMode, local_id: air::LocalId) -> Param {
    Param {
        name: Some(Ident::new(name)),
        ty,
        mode,
        escape: ParamEscape::NonEscaping,
        role: ParamRole::Normal,
        local_id,
    }
}

pub(crate) fn local(ty: air::TypeId, mutability: Mutability, kind: LocalKind) -> Local {
    Local {
        name: None,
        binding: None,
        ty,
        mutability,
        kind,
    }
}

pub(crate) fn immutable_local(ty: air::TypeId, kind: LocalKind) -> Local {
    local(ty, Mutability::Immutable, kind)
}

pub(crate) fn mutable_local(ty: air::TypeId, kind: LocalKind) -> Local {
    local(ty, Mutability::Mutable, kind)
}

pub(crate) fn place(local: air::LocalId, ty: air::TypeId) -> Place {
    Place {
        root: PlaceRoot::Local(local),
        projection: vec![],
        ty,
    }
}

pub(crate) fn root_module() -> air::Module {
    air::Module::default()
}
