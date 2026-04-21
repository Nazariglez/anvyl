use std::collections::{HashMap, HashSet, VecDeque};

use super::{
    error::{Diagnostic, DiagnosticKind},
    infer::{build_subst, subst_type},
    types::{EnumDef, StructDef},
    visit::type_any,
};
use crate::ast::{Ident, StructField, Type, VariantKind};

pub(super) fn analyze_cyclicity(
    struct_defs: &HashMap<Ident, StructDef>,
    enum_defs: &HashMap<Ident, EnumDef>,
) -> HashSet<Ident> {
    let datarefs: Vec<Ident> = struct_defs
        .iter()
        .filter(|(_, def)| def.kind.is_dataref())
        .map(|(name, _)| *name)
        .collect();

    if datarefs.is_empty() {
        return HashSet::new();
    }

    let sccs = find_type_sccs(&datarefs, |name| {
        let def = &struct_defs[&name];
        let mut edges = vec![];
        for field in &def.fields {
            extract_dataref_refs(&field.ty, struct_defs, enum_defs, &mut edges);
        }
        edges
    });

    // build the adjacency map we need for cycle detection
    let adj: HashMap<Ident, Vec<Ident>> = datarefs
        .iter()
        .map(|&name| {
            let def = &struct_defs[&name];
            let mut edges = vec![];
            for field in &def.fields {
                extract_dataref_refs(&field.ty, struct_defs, enum_defs, &mut edges);
            }
            (name, edges)
        })
        .collect();

    // cycle-capable, multi-node SCC or a single node with a self-loop
    let mut cycle_core: HashSet<Ident> = HashSet::new();
    for scc in &sccs {
        let is_cycle_capable = scc.len() > 1
            || scc
                .first()
                .is_some_and(|v| adj.get(v).is_some_and(|nb| nb.contains(v)));
        if is_cycle_capable {
            cycle_core.extend(scc.iter().copied());
        }
    }

    // bfs backwards on the reverse graph, any type that can reach a cycle-capable node is itself cycle-capable
    let mut reverse_adj: HashMap<Ident, Vec<Ident>> = HashMap::new();
    for (src, dests) in &adj {
        for &dest in dests {
            reverse_adj.entry(dest).or_default().push(*src);
        }
    }

    let mut cycle_capable = cycle_core.clone();
    let mut queue: VecDeque<Ident> = cycle_core.iter().copied().collect();

    // any dataref with a fn(...) field is unconditionally cycle-capable, a closure stored
    // in it might capture a reference back to the owning dataref at runtime
    for &name in &datarefs {
        let def = &struct_defs[&name];
        let has_fn_field = def
            .fields
            .iter()
            .any(|f| type_contains_func(&f.ty, enum_defs));
        if has_fn_field && cycle_capable.insert(name) {
            queue.push_back(name);
        }
    }

    while let Some(node) = queue.pop_front() {
        if let Some(predecessors) = reverse_adj.get(&node) {
            for &pred in predecessors {
                if cycle_capable.insert(pred) {
                    queue.push_back(pred);
                }
            }
        }
    }

    cycle_capable
}

fn for_each_resolved_variant_field<F>(enum_def: &EnumDef, type_args: &[Type], mut callback: F)
where
    F: FnMut(&Type),
{
    let subst = build_subst(&enum_def.type_params, type_args);
    for variant in &enum_def.variants {
        match &variant.kind {
            VariantKind::Unit => {}
            VariantKind::Tuple(tys) => {
                for field_ty in tys {
                    callback(&subst_type(field_ty, &subst, &HashMap::new()));
                }
            }
            VariantKind::Struct(fields) => {
                for field in fields {
                    callback(&subst_type(&field.ty, &subst, &HashMap::new()));
                }
            }
        }
    }
}

fn extract_dataref_refs(
    ty: &Type,
    struct_defs: &HashMap<Ident, StructDef>,
    enum_defs: &HashMap<Ident, EnumDef>,
    out: &mut Vec<Ident>,
) {
    type_any(ty, &mut |t| {
        match t {
            Type::DataRef { name, .. } | Type::Struct { name, .. } | Type::UnresolvedName(name)
                if struct_defs.get(name).is_some_and(|d| d.kind.is_dataref()) =>
            {
                out.push(*name);
            }
            Type::Enum {
                name, type_args, ..
            } => {
                // type_any handles type_args recursion automatically
                // expand variant fields which are not part of the structural tree
                if let Some(enum_def) = enum_defs.get(name) {
                    for_each_resolved_variant_field(enum_def, type_args, |resolved| {
                        extract_dataref_refs(resolved, struct_defs, enum_defs, out);
                    });
                }
            }
            _ => {}
        }
        false // never short-circuit, collecting, not searching
    });
}

fn type_contains_func(ty: &Type, enum_defs: &HashMap<Ident, EnumDef>) -> bool {
    type_any(ty, &mut |t| match t {
        Type::Func { .. } => true,
        Type::Enum {
            name, type_args, ..
        } => {
            // type_any handles type_args recursion automatically
            // expand variant fields which are not part of the structural tree
            enum_defs.get(name).is_some_and(|def| {
                let mut found = false;
                for_each_resolved_variant_field(def, type_args, |resolved| {
                    if !found && type_contains_func(resolved, enum_defs) {
                        found = true;
                    }
                });
                found
            })
        }
        _ => false,
    })
}

pub(super) fn check_value_type_cycles(
    struct_defs: &HashMap<Ident, StructDef>,
    enum_defs: &HashMap<Ident, EnumDef>,
) -> Vec<Diagnostic> {
    let value_structs: Vec<Ident> = struct_defs
        .iter()
        .filter(|(_, def)| !def.kind.is_dataref())
        .map(|(name, _)| *name)
        .collect();

    if value_structs.is_empty() {
        return vec![];
    }

    let adj: HashMap<Ident, Vec<Ident>> = value_structs
        .iter()
        .map(|&name| {
            let def = &struct_defs[&name];
            let mut edges = vec![];
            for field in &def.fields {
                extract_value_type_refs(&field.ty, struct_defs, enum_defs, &mut edges);
            }
            (name, edges)
        })
        .collect();

    let sccs = find_type_sccs(&value_structs, |name| {
        adj.get(&name).cloned().unwrap_or_default()
    });

    let mut diagnostics = vec![];
    for scc in &sccs {
        let is_cycle = scc.len() > 1
            || scc
                .first()
                .is_some_and(|v| adj.get(v).is_some_and(|nb| nb.contains(v)));

        if !is_cycle {
            continue;
        }

        let scc_set: HashSet<Ident> = scc.iter().copied().collect();
        for &type_name in scc {
            let def = &struct_defs[&type_name];
            if let Some((cycle_field, cycle_target)) =
                find_cycle_field(&def.fields, &scc_set, struct_defs, enum_defs)
            {
                diagnostics.push(
                    Diagnostic::new(
                        def.span,
                        DiagnosticKind::InfiniteSizeType {
                            type_name,
                            cycle_field,
                            cycle_target,
                        },
                    )
                    .with_help("use 'dataref' instead of 'struct', or wrap the field in a list"),
                );
            }
        }
    }

    diagnostics
}

fn find_cycle_field(
    fields: &[StructField],
    scc_set: &HashSet<Ident>,
    struct_defs: &HashMap<Ident, StructDef>,
    enum_defs: &HashMap<Ident, EnumDef>,
) -> Option<(Ident, Ident)> {
    for field in fields {
        let mut refs = vec![];
        extract_value_type_refs(&field.ty, struct_defs, enum_defs, &mut refs);
        for target in refs {
            if scc_set.contains(&target) {
                return Some((field.name, target));
            }
        }
    }
    None
}

fn extract_value_type_refs(
    ty: &Type,
    struct_defs: &HashMap<Ident, StructDef>,
    enum_defs: &HashMap<Ident, EnumDef>,
    out: &mut Vec<Ident>,
) {
    match ty {
        Type::Struct { name, .. }
            if struct_defs.get(name).is_some_and(|d| !d.kind.is_dataref()) =>
        {
            out.push(*name);
        }
        Type::UnresolvedName(name) => {
            if struct_defs.get(name).is_some_and(|d| !d.kind.is_dataref()) {
                out.push(*name);
            } else if !struct_defs.contains_key(name) {
                // unresolved names may refer to an enum, expand its variants inline
                expand_enum_value_refs(*name, &[], struct_defs, enum_defs, out);
            }
            // dataref structs are skipped, they introduce pointer indirection breaking the cycle
        }
        Type::Enum {
            name, type_args, ..
        } => {
            expand_enum_value_refs(*name, type_args, struct_defs, enum_defs, out);
        }
        Type::Tuple(elems) => {
            for elem in elems {
                extract_value_type_refs(elem, struct_defs, enum_defs, out);
            }
        }
        Type::NamedTuple(fields) => {
            for (_, field_ty) in fields {
                extract_value_type_refs(field_ty, struct_defs, enum_defs, out);
            }
        }
        Type::Array { elem, .. } => {
            extract_value_type_refs(elem, struct_defs, enum_defs, out);
        }
        _ => {}
    }
}

fn expand_enum_value_refs(
    name: Ident,
    type_args: &[Type],
    struct_defs: &HashMap<Ident, StructDef>,
    enum_defs: &HashMap<Ident, EnumDef>,
    out: &mut Vec<Ident>,
) {
    let Some(enum_def) = enum_defs.get(&name) else {
        return;
    };
    for_each_resolved_variant_field(enum_def, type_args, |resolved| {
        extract_value_type_refs(resolved, struct_defs, enum_defs, out);
    });
}

fn find_type_sccs<F>(names: &[Ident], mut edge_fn: F) -> Vec<Vec<Ident>>
where
    F: FnMut(Ident) -> Vec<Ident>,
{
    let adj: HashMap<Ident, Vec<Ident>> = names.iter().map(|&n| (n, edge_fn(n))).collect();

    let mut state = TarjanState {
        index_counter: 0,
        stack: vec![],
        on_stack: HashSet::new(),
        indices: HashMap::new(),
        lowlinks: HashMap::new(),
        sccs: vec![],
        adj,
    };

    for &name in names {
        if !state.indices.contains_key(&name) {
            state.strongconnect(name);
        }
    }

    state.sccs
}

struct TarjanState {
    index_counter: u32,
    stack: Vec<Ident>,
    on_stack: HashSet<Ident>,
    indices: HashMap<Ident, u32>,
    lowlinks: HashMap<Ident, u32>,
    sccs: Vec<Vec<Ident>>,
    adj: HashMap<Ident, Vec<Ident>>,
}

impl TarjanState {
    fn strongconnect(&mut self, v: Ident) {
        self.indices.insert(v, self.index_counter);
        self.lowlinks.insert(v, self.index_counter);
        self.index_counter += 1;
        self.stack.push(v);
        self.on_stack.insert(v);

        let neighbors = self.adj.get(&v).cloned().unwrap_or_default();
        for w in neighbors {
            if !self.indices.contains_key(&w) {
                self.strongconnect(w);
                let lowlink_w = self.lowlinks[&w];
                let lowlink_v = self.lowlinks[&v];
                self.lowlinks.insert(v, lowlink_v.min(lowlink_w));
            } else if self.on_stack.contains(&w) {
                let index_w = self.indices[&w];
                let lowlink_v = self.lowlinks[&v];
                self.lowlinks.insert(v, lowlink_v.min(index_w));
            }
        }

        if self.lowlinks[&v] == self.indices[&v] {
            let mut scc = vec![];
            loop {
                let w = self.stack.pop().unwrap();
                self.on_stack.remove(&w);
                scc.push(w);
                if w == v {
                    break;
                }
            }
            self.sccs.push(scc);
        }
    }
}
