use anvyx_frontend::ast;

use super::Printer;

impl Printer<'_> {
    pub(super) fn format_generic_args(&mut self, args: &[ast::GenericArg]) {
        self.format_comma_list("<", ">", args, Self::format_generic_arg);
    }

    pub(super) fn format_generic_arg(&mut self, arg: &ast::GenericArg) {
        match arg {
            ast::GenericArg::Type(ty) => self.format_type(ty),
            ast::GenericArg::Const(arg) => self.format_const_arg(arg),
        }
    }

    fn format_const_arg(&mut self, arg: &ast::ConstArg) {
        match arg {
            ast::ConstArg::Value(value) => self.write_fmt(value),
            ast::ConstArg::Name(name) => self.write_fmt(name),
            ast::ConstArg::Param(id) => {
                if let Some(name) = self.const_param_names.get(id) {
                    self.buf.push_str(name);
                } else {
                    self.write_fmt(id);
                }
            }
        }
    }

    pub(super) fn format_type(&mut self, ty: &ast::Type) {
        match ty {
            ast::Type::Int => self.write("int"),
            ast::Type::Float => self.write("float"),
            ast::Type::Bool => self.write("bool"),
            ast::Type::String => self.write("string"),
            ast::Type::Void => self.write("void"),
            ast::Type::Infer | ast::Type::InferReturn => self.write("_"),
            ast::Type::Any => self.write("any"),
            ast::Type::Var(id) => {
                if let Some(name) = self.type_var_names.get(id) {
                    self.buf.push_str(name);
                } else {
                    self.write_fmt(id);
                }
            }
            ast::Type::UnresolvedName(ident) => self.write_fmt(ident),
            ast::Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => {
                if let Some(qualifier) = qualifier {
                    self.write_fmt(qualifier);
                    self.write(".");
                }
                self.write_fmt(name);
                if !generic_args.is_empty() {
                    self.format_generic_args(generic_args);
                }
            }
            ast::Type::Nominal(nominal) => self.format_nominal_type(nominal),
            ast::Type::Func { params, ret } => {
                self.write("fn(");
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    if p.mutable {
                        self.write("ref ");
                    }
                    self.format_param_prefix(p.escape, p.cast_accept);
                    self.format_type(&p.ty);
                }
                self.write(")");
                if !ret.is_implicit_void() {
                    self.write(" -> ");
                    self.format_return_spec_tail(ret);
                }
            }
            ast::Type::Dyn(contract) => {
                self.write("dyn ");
                self.format_contract_ref(contract);
            }
            ast::Type::Tuple(elems) => {
                self.write("(");
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_type(elem);
                }
                self.write(")");
            }
            ast::Type::List { elem } => {
                self.write("[");
                self.format_type(elem);
                self.write("]");
            }
            ast::Type::Array { elem, len } => {
                self.write("[");
                self.format_type(elem);
                self.write("; ");
                self.format_array_len(len);
                self.write("]");
            }
            ast::Type::Map { key, value } => {
                self.write("[");
                self.format_type(key);
                self.write(": ");
                self.format_type(value);
                self.write("]");
            }
            ast::Type::Slice { elem } => {
                self.write("slice[");
                self.format_type(elem);
                self.write("]");
            }
            ast::Type::Optional { inner } => self.format_optional_type(inner),
        }
    }

    pub(super) fn format_contract_ref(&mut self, contract: &ast::ContractRef) {
        match contract {
            ast::ContractRef::Named {
                qualifier, name, ..
            } => {
                if let Some(qualifier) = qualifier {
                    self.write_fmt(qualifier);
                    self.write(".");
                }
                self.write_fmt(name);
            }
            ast::ContractRef::Anonymous(_) => self.write("_"),
            ast::ContractRef::Intersection(contracts) => {
                for (i, contract) in contracts.iter().enumerate() {
                    if i > 0 {
                        self.write(" + ");
                    }
                    self.format_contract_ref(contract);
                }
            }
            ast::ContractRef::Infer | ast::ContractRef::Hole(_) => self.write("_"),
        }
    }

    fn format_nominal_type(&mut self, nominal: &ast::NominalType) {
        self.write_fmt(nominal.name);
        let args = nominal
            .type_args
            .iter()
            .cloned()
            .map(ast::GenericArg::Type)
            .chain(
                nominal
                    .const_args
                    .iter()
                    .cloned()
                    .map(ast::GenericArg::Const),
            )
            .collect::<Vec<_>>();
        if !args.is_empty() {
            self.format_generic_args(&args);
        }
    }

    fn format_optional_type(&mut self, inner: &ast::Type) {
        if matches!(inner, ast::Type::Func { .. }) {
            self.write("(");
            self.format_type(inner);
            self.write(")");
        } else {
            self.format_type(inner);
        }
        self.write("?");
    }

    fn format_array_len(&mut self, len: &ast::ArrayLen) {
        match len {
            ast::ArrayLen::Fixed(n) => self.write_fmt(n),
            ast::ArrayLen::Infer => self.write("_"),
            ast::ArrayLen::Named(ident) => self.write_fmt(ident),
            ast::ArrayLen::Param(id) => {
                if let Some(name) = self.const_param_names.get(id) {
                    self.buf.push_str(name);
                } else {
                    self.write_fmt(id);
                }
            }
        }
    }

    pub(super) fn format_type_params(
        &mut self,
        type_params: &[ast::TypeParam],
        const_params: &[ast::ConstParam],
    ) {
        if type_params.is_empty() && const_params.is_empty() {
            return;
        }
        self.write("<");
        for (i, tp) in type_params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write_fmt(tp.name);
            if !tp.bounds.is_empty() {
                self.write(": ");
                for (j, bound) in tp.bounds.iter().enumerate() {
                    if j > 0 {
                        self.write(" + ");
                    }
                    self.format_contract_ref(bound);
                }
            }
        }
        for (i, cp) in const_params.iter().enumerate() {
            if i > 0 || !type_params.is_empty() {
                self.write(", ");
            }
            self.write_fmt(cp.name);
            self.write(": int");
        }
        self.write(">");
    }
}
