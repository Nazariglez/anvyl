use crate::{
    ast::Ident,
    resolve::{ModuleId, PackageModulePath},
    source::SourceId,
    span::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct SourceDeclId {
    source: SourceId,
    span: Span,
}

impl SourceDeclId {
    pub(crate) fn new(source: SourceId, span: Span) -> Self {
        Self { source, span }
    }

    pub(crate) fn source(&self) -> SourceId {
        self.source
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ExternalNominalId {
    module: ModuleId,
    name: Ident,
}

impl ExternalNominalId {
    pub(crate) fn new(module: ModuleId, name: Ident) -> Self {
        Self { module, name }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum NominalId {
    Source(SourceDeclId),
    External(ExternalNominalId),
}

impl NominalId {
    pub(crate) fn encode(&self, out: &mut Vec<u8>) {
        fn push_usize(out: &mut Vec<u8>, value: usize) {
            out.extend_from_slice(&value.to_le_bytes());
        }

        fn push_str(out: &mut Vec<u8>, value: &str) {
            push_usize(out, value.len());
            out.extend_from_slice(value.as_bytes());
        }

        match self {
            Self::Source(id) => {
                out.push(0);
                push_usize(out, id.source.index());
                push_usize(out, id.span.start);
                push_usize(out, id.span.end);
            }
            Self::External(id) => {
                out.push(1);
                match id.module.package_context() {
                    Some(package) => {
                        out.push(1);
                        push_str(out, package.as_str());
                    }
                    None => out.push(0),
                }
                match id.module.path() {
                    PackageModulePath::Root => out.push(0),
                    PackageModulePath::Named(path) => {
                        out.push(1);
                        push_usize(out, path.segments().len());
                        for segment in path.segments() {
                            push_str(out, segment);
                        }
                    }
                    PackageModulePath::Provider(path) => {
                        out.push(2);
                        push_usize(out, path.segments().len());
                        for segment in path.segments() {
                            push_str(out, segment);
                        }
                    }
                    PackageModulePath::Source(file) => {
                        out.push(3);
                        push_str(out, &file.to_string());
                    }
                }
                push_str(out, id.name.as_str());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::resolve::{ModulePath, PackageId, SourceFileId};

    fn encoded(module: ModuleId, name: &str) -> Vec<u8> {
        let id = NominalId::External(ExternalNominalId::new(module, Ident::new(name)));
        let mut bytes = vec![];
        id.encode(&mut bytes);
        bytes
    }

    #[test]
    fn external_encoding_preserves_module_identity() {
        let package = PackageId::synthetic_root();
        let named = ModuleId::named(
            package.clone(),
            ModulePath::new(vec!["foo".to_string()]).unwrap(),
        );
        let source_a = ModuleId::source(package.clone(), SourceFileId::new("/a/foo.anv").unwrap());
        let source_b = ModuleId::source(package.clone(), SourceFileId::new("/b/foo.anv").unwrap());

        assert_ne!(
            encoded(named.clone(), "Thing"),
            encoded(source_a.clone(), "Thing")
        );
        assert_ne!(encoded(source_a, "Thing"), encoded(source_b, "Thing"));

        let other_package = PackageId::new("other");
        assert_ne!(
            encoded(named.clone(), "Thing"),
            encoded(
                ModuleId::named(
                    other_package,
                    ModulePath::new(vec!["foo".to_string()]).unwrap(),
                ),
                "Thing",
            )
        );
        assert_ne!(
            encoded(
                ModuleId::named(
                    package.clone(),
                    ModulePath::new(vec!["ab".to_string(), "c".to_string()]).unwrap(),
                ),
                "Thing",
            ),
            encoded(
                ModuleId::named(
                    package.clone(),
                    ModulePath::new(vec!["a".to_string(), "bc".to_string()]).unwrap(),
                ),
                "Thing",
            )
        );
        assert_ne!(
            encoded(named.clone(), "Thing"),
            encoded(
                ModuleId::provider(
                    package.clone(),
                    ModulePath::new(vec!["foo".to_string()]).unwrap(),
                ),
                "Thing",
            )
        );
        assert_ne!(
            encoded(named, "Thing"),
            encoded(ModuleId::root(package), "foo.Thing")
        );
    }
}
