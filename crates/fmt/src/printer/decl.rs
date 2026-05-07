use anvyx_frontend::ast;

use super::Printer;

impl Printer<'_> {
    fn write_receiver(&mut self, recv: ast::MethodReceiver) {
        match recv {
            ast::MethodReceiver::Value => self.write("self"),
            ast::MethodReceiver::Var => self.write("var self"),
        }
    }

    fn write_extern_receiver(&mut self, recv: ast::ExternReceiverMode) {
        match recv {
            ast::ExternReceiverMode::Value => self.write("self"),
            ast::ExternReceiverMode::Shared => self.write("shared self"),
            ast::ExternReceiverMode::Mutable => self.write("var self"),
        }
    }

    pub(super) fn format_annotations(&mut self, annotations: &[ast::AnnotationNode]) {
        for ann in annotations {
            self.write_indent();
            self.write("@");
            self.write_fmt(ann.node.name);
            match &ann.node.args {
                ast::AnnotationArgs::None => {}
                ast::AnnotationArgs::Positional(lit) => {
                    self.write("(");
                    self.format_lit(lit);
                    self.write(")");
                }
                ast::AnnotationArgs::Named(pairs) => {
                    self.write("(");
                    for (i, (name, lit)) in pairs.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write_fmt(name);
                        // bare flags like `@foo(flag)` are stored as Bool(true), skip the `= true`
                        if !matches!(lit, ast::Lit::Bool(true)) {
                            self.write(" = ");
                            self.format_lit(lit);
                        }
                    }
                    self.write(")");
                }
            }
            self.writeln();
        }
    }

    pub(super) fn format_doc_comment(&mut self, doc: Option<&String>) {
        if let Some(doc) = doc {
            for line in doc.split('\n') {
                self.write_indent();
                self.write("///");
                if !line.is_empty() {
                    self.write(" ");
                    self.write(line);
                }
                self.writeln();
            }
        }
    }

    pub(super) fn format_param(&mut self, param: &ast::Param) {
        if matches!(param.mutability, ast::Mutability::Mutable) {
            self.write("var ");
        }
        self.write_fmt(param.name);
        self.write(": ");
        if param.cast_accept {
            self.write("as ");
        }
        self.format_type(&param.ty);
        if let Some(default) = &param.default {
            self.write(" = ");
            self.format_expr(&default.node);
        }
    }

    pub(super) fn format_inline_params(&mut self, params: &[ast::Param]) {
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.format_param(param);
        }
    }

    pub(super) fn format_param_list(&mut self, params: &[ast::Param]) {
        if params.is_empty() {
            self.write("()");
            return;
        }
        self.format_comma_list("(", ")", params, Self::format_param);
    }

    pub(super) fn format_func(&mut self, func: &ast::Func) {
        self.populate_type_param_names(&func.type_params, &func.const_params);
        self.format_annotations(&func.annotations);
        self.format_doc_comment(func.doc.as_ref());
        self.write_indent();
        self.format_visibility(func.visibility);
        self.write("fn ");
        self.write_fmt(func.name);
        self.format_type_params(&func.type_params, &func.const_params);
        self.format_param_list(&func.params);
        self.format_return_type(&func.ret);
        self.write(" ");
        self.format_block(&func.body);
        self.writeln();
    }

    pub(super) fn format_extern_func(&mut self, ef: &ast::ExternFunc) {
        self.format_annotations(&ef.annotations);
        self.format_doc_comment(ef.doc.as_ref());
        self.write_indent();
        self.format_visibility(ef.visibility);
        self.write("extern fn ");
        self.write_fmt(ef.name);
        self.format_param_list(&ef.params);
        self.format_return_type(&ef.ret);
        self.write(";");
        self.writeln();
    }

    fn format_extern_type_member(&mut self, member: &ast::ExternTypeMember) {
        match member {
            ast::ExternTypeMember::Field {
                doc,
                name,
                ty,
                computed,
            } => {
                self.format_doc_comment(doc.as_ref());
                self.write_indent();
                if *computed {
                    self.write("computed ");
                }
                self.write_fmt(name);
                self.write(": ");
                self.format_type(ty);
                self.write(";");
                self.writeln();
            }
            ast::ExternTypeMember::Method {
                doc,
                name,
                receiver,
                params,
                ret,
            } => {
                self.format_doc_comment(doc.as_ref());
                self.write_indent();
                self.write("fn ");
                self.write_fmt(name);
                self.write("(");
                self.write_extern_receiver(*receiver);
                if !params.is_empty() {
                    self.write(", ");
                    self.format_inline_params(params);
                }
                self.write(")");
                self.format_return_type(ret);
                self.write(";");
                self.writeln();
            }
            ast::ExternTypeMember::StaticMethod {
                doc,
                name,
                params,
                ret,
            } => {
                self.format_doc_comment(doc.as_ref());
                self.write_indent();
                self.write("fn ");
                self.write_fmt(name);
                self.write("(");
                self.format_inline_params(params);
                self.write(")");
                self.format_return_type(ret);
                self.write(";");
                self.writeln();
            }
            ast::ExternTypeMember::Operator {
                op,
                other_ty,
                ret,
                self_on_right,
            } => {
                self.write_indent();
                self.write("op ");
                if *self_on_right {
                    self.format_type(other_ty);
                    self.write(" ");
                    self.write_fmt(op);
                    self.write(" Self");
                } else {
                    self.write("Self ");
                    self.write_fmt(op);
                    self.write(" ");
                    self.format_type(other_ty);
                }
                self.write(" -> ");
                self.format_type(ret);
                self.write(";");
                self.writeln();
            }
            ast::ExternTypeMember::UnaryOperator { op, ret } => {
                self.write_indent();
                self.write("op ");
                self.write_fmt(op);
                self.write("Self -> ");
                self.format_type(ret);
                self.write(";");
                self.writeln();
            }
        }
    }

    pub(super) fn format_extern_type(&mut self, et: &ast::ExternType) {
        self.format_annotations(&et.annotations);
        self.format_doc_comment(et.doc.as_ref());
        self.write_indent();
        self.format_visibility(et.visibility);
        self.write("extern type ");
        self.write_fmt(et.name);
        if et.init.is_none() && et.members.is_empty() {
            self.write(";");
            self.writeln();
            return;
        }
        self.write(" {");
        self.writeln();
        self.indent();
        if let Some(init) = &et.init {
            self.write_indent();
            self.write("init");
            if !init.params.is_empty() {
                self.format_param_list(&init.params);
            }
            self.write(";");
            self.writeln();
        }
        for member in &et.members {
            self.format_extern_type_member(member);
        }
        self.dedent();
        self.write_indent();
        self.write("}");
        self.writeln();
    }

    fn format_import_target(&mut self, target: &ast::ImportTarget) {
        match &target.root {
            ast::ImportRoot::Local { ascend } => {
                if *ascend > 0 {
                    for _ in 0..=*ascend {
                        self.write(".");
                    }
                }
            }
            ast::ImportRoot::Package(alias) => {
                self.write("pkg:");
                self.write_fmt(alias);
                if !matches!(target.path, ast::PackageModulePath::Root) {
                    self.write(".");
                }
            }
            ast::ImportRoot::NativeProvider => self.write("ext:"),
            ast::ImportRoot::Std => self.write("std:"),
        }

        match &target.path {
            ast::PackageModulePath::Root => {}
            ast::PackageModulePath::Named(path) => {
                for (i, segment) in path.iter().enumerate() {
                    if i > 0 {
                        self.write(".");
                    }
                    self.write_fmt(segment);
                }
            }
        }
    }

    fn format_import_item(&mut self, item: &ast::ImportItem) {
        match item.kind {
            ast::ImportItemKind::Name(name) => self.write_fmt(name),
            ast::ImportItemKind::SelfModule => self.write("self"),
        }
        if let Some(alias) = &item.alias {
            self.write(" as ");
            self.write_fmt(alias);
        }
    }

    pub(super) fn format_import(&mut self, import: &ast::Import) {
        self.write_indent();
        self.format_visibility(import.visibility);
        self.write("import ");
        self.format_import_target(&import.target);
        match &import.kind {
            ast::ImportKind::Module => {
                self.write(";");
            }
            ast::ImportKind::ModuleAs(alias) => {
                self.write(" as ");
                self.write_fmt(alias);
                self.write(";");
            }
            ast::ImportKind::Selective(items) => {
                let fits = self.try_single_line(|p| {
                    p.write(" { ");
                    for (i, item) in items.iter().enumerate() {
                        if i > 0 {
                            p.write(", ");
                        }
                        p.format_import_item(item);
                    }
                    p.write(" }");
                });
                if fits {
                    self.write(";");
                } else {
                    self.write(" {");
                    self.writeln();
                    self.indent();
                    for item in items {
                        self.write_indent();
                        self.format_import_item(item);
                        self.write(",");
                        self.writeln();
                    }
                    self.dedent();
                    self.write_indent();
                    self.write("};");
                }
            }
            ast::ImportKind::Wildcard => {
                self.write(" { * };");
            }
        }
        self.writeln();
    }

    pub(super) fn format_const(&mut self, cd: &ast::ConstDecl) {
        self.format_annotations(&cd.annotations);
        self.format_doc_comment(cd.doc.as_ref());
        self.write_indent();
        self.format_visibility(cd.visibility);
        self.write("const ");
        self.write_fmt(cd.name);
        if let Some(ty) = &cd.ty {
            self.write(": ");
            self.format_type(ty);
        }
        self.write(" = ");
        self.format_expr(&cd.value.node);
        self.write(";");
        self.writeln();
    }

    fn format_struct_field(&mut self, field: &ast::StructField) {
        self.format_annotations(&field.annotations);
        self.format_doc_comment(field.doc.as_ref());
        self.write_indent();
        self.write_fmt(field.name);
        self.write(": ");
        self.format_type(&field.ty);
        if let Some(default) = &field.default {
            self.write(" = ");
            self.format_expr(&default.node);
        }
        self.write(",");
        self.writeln();
    }

    fn format_method_sig(&mut self, sig: &ast::MethodSig) {
        self.write_fmt(sig.name);
        self.format_type_params(&sig.type_params, &sig.const_params);
        let has_receiver = sig.receiver.is_some();
        if !has_receiver && sig.params.is_empty() {
            self.write("()");
            return;
        }

        let single = self.try_single_line(|p| {
            p.write("(");
            if let Some(recv) = sig.receiver {
                p.write_receiver(recv);
            }
            if has_receiver && !sig.params.is_empty() {
                p.write(", ");
            }
            p.format_inline_params(&sig.params);
            p.write(")");
        });
        if single {
            return;
        }

        self.write("(");
        self.writeln();
        self.indent();
        if let Some(recv) = sig.receiver {
            self.write_indent();
            self.write_receiver(recv);
            self.write(",");
            self.writeln();
        }
        for param in &sig.params {
            self.write_indent();
            self.format_param(param);
            self.write(",");
            self.writeln();
        }
        self.dedent();
        self.write_indent();
        self.write(")");
    }

    pub(super) fn format_method(&mut self, method: &ast::Method) {
        let saved_type_vars = self.type_var_names.clone();
        let saved_const_params = self.const_param_names.clone();
        self.extend_type_param_names(&method.sig.type_params, &method.sig.const_params);

        self.format_annotations(&method.annotations);
        self.format_doc_comment(method.doc.as_ref());
        self.write_indent();
        self.format_visibility(method.visibility);
        self.write("fn ");
        self.format_method_sig(&method.sig);
        self.format_return_type(&method.sig.ret);
        self.write(" ");
        self.format_block(&method.body);
        self.writeln();

        self.type_var_names = saved_type_vars;
        self.const_param_names = saved_const_params;
    }

    pub(super) fn format_aggregate(&mut self, decl: &ast::StructDecl) {
        self.populate_type_param_names(&decl.type_params, &decl.const_params);
        self.format_annotations(&decl.annotations);
        self.format_doc_comment(decl.doc.as_ref());
        self.write_indent();
        self.format_visibility(decl.visibility);
        self.write(decl.kind.keyword());
        self.write(" ");
        self.write_fmt(decl.name);
        self.format_type_params(&decl.type_params, &decl.const_params);
        self.write(" {");
        self.writeln();
        self.indent();
        for field in &decl.fields {
            self.format_struct_field(field);
        }
        if !decl.fields.is_empty() && !decl.methods.is_empty() {
            self.writeln();
        }
        for method in &decl.methods {
            self.format_method(method);
        }
        self.dedent();
        self.write_indent();
        self.write("}");
        self.writeln();
    }

    fn format_enum_variant(&mut self, variant: &ast::EnumVariant) {
        self.format_annotations(&variant.annotations);
        self.format_doc_comment(variant.doc.as_ref());
        self.write_indent();
        self.write_fmt(variant.name);
        match &variant.kind {
            ast::VariantKind::Unit => {}
            ast::VariantKind::Tuple(types) => {
                self.write("(");
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.format_type(ty);
                }
                self.write(")");
            }
            ast::VariantKind::Struct(fields) => {
                self.write(" { ");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write_fmt(field.name);
                    self.write(": ");
                    self.format_type(&field.ty);
                }
                self.write(" }");
            }
        }
        self.write(",");
        self.writeln();
    }

    pub(super) fn format_enum(&mut self, decl: &ast::EnumDecl) {
        self.populate_type_param_names(&decl.type_params, &decl.const_params);
        self.format_annotations(&decl.annotations);
        self.format_doc_comment(decl.doc.as_ref());
        self.write_indent();
        self.format_visibility(decl.visibility);
        self.write("enum ");
        self.write_fmt(decl.name);
        self.format_type_params(&decl.type_params, &decl.const_params);
        self.write(" {");
        self.writeln();
        self.indent();
        for variant in &decl.variants {
            self.format_enum_variant(variant);
        }
        self.dedent();
        self.write_indent();
        self.write("}");
        self.writeln();
    }

    fn format_extend_method(&mut self, method: &ast::ExtendMethod) {
        self.format_annotations(&method.annotations);
        self.format_doc_comment(method.doc.as_ref());
        self.write_indent();
        self.write("fn ");
        self.format_method_sig(&method.sig);
        self.format_return_type(&method.sig.ret);
        self.write(" ");
        self.format_block(&method.body);
        self.writeln();
    }

    fn format_cast_from(&mut self, cf: &ast::CastFrom) {
        self.write_indent();
        self.write("cast from(");
        self.format_param(&cf.param);
        self.write(")");
        if let Some(ret) = &cf.ret {
            self.write(" -> ");
            self.format_type(ret);
        }
        self.write(" ");
        self.format_block(&cf.body);
        self.writeln();
    }

    fn format_extend_type(&mut self, ty: &ast::Type) {
        if let ast::Type::Nominal(nominal) = ty
            && matches!(nominal.kind, ast::NominalKind::DataRef)
            && nominal.type_args.is_empty()
            && nominal.const_args.is_empty()
        {
            self.write("dataref ");
            self.write_fmt(nominal.name);
            return;
        }
        self.format_type(ty);
    }

    pub(super) fn format_extend(&mut self, decl: &ast::ExtendDecl) {
        self.populate_type_param_names(&decl.type_params, &decl.const_params);
        self.write_indent();
        self.format_visibility(decl.visibility);
        self.write("extend");
        if !decl.type_params.is_empty() || !decl.const_params.is_empty() {
            self.format_type_params(&decl.type_params, &decl.const_params);
        }
        self.write(" ");
        self.format_extend_type(&decl.ty);

        self.write(" {");
        self.writeln();
        self.indent();
        for method in &decl.methods {
            self.format_extend_method(&method.node);
        }
        for cf in &decl.cast_froms {
            self.format_cast_from(&cf.node);
        }
        self.dedent();
        self.write_indent();
        self.write("}");
        self.writeln();
    }
}
