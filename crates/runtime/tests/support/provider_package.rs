use anvyx_externs::{ProviderCatalog, ProviderPackageKey, RustExternBinding, RustTypeBinding};
use anvyx_runtime::{
    ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternFunctionDescriptor,
    ExternFunctionKey, ExternMemberKey, ExternMemberSelector, ExternModuleDescriptor,
    ExternTypeDescriptor, ExternTypeKey, ModuleExport, ModulePath, ProviderId, RawProviderPackage,
};

pub struct TestCatalog {
    catalog: ProviderCatalog,
    package: ProviderPackageKey,
    provider: ProviderId,
    module: ModulePath,
}

impl TestCatalog {
    pub fn from_export(export: ModuleExport) -> Self {
        let package = ProviderPackageKey("runtime-test".to_string());
        let provider = ProviderId {
            name: "runtime_test".to_string(),
        };
        let module = ModulePath {
            segments: vec!["test".to_string()],
        };
        let mut raw = RawProviderPackage::default();
        export.finalize(&mut raw, provider.clone(), module.clone(), &[], None);
        let catalog = ProviderCatalog::try_new(vec![(
            package.clone(),
            raw,
            Some("runtime_test".to_string()),
        )])
        .expect("generated package must validate");
        Self {
            catalog,
            package,
            provider,
            module,
        }
    }

    pub fn module(&self) -> &ExternModuleDescriptor {
        self.catalog
            .modules()
            .find_map(|(package, provider, module)| {
                (package == &self.package && provider == &self.provider).then_some(module)
            })
            .expect("generated package must contain its module")
    }

    pub fn function(&self, name: &str) -> (&ExternFunctionDescriptor, &RustExternBinding) {
        let descriptor = self
            .module()
            .functions
            .iter()
            .find(|function| function.name == name)
            .expect("generated package must contain its function");
        let binding = self.binding(ExternBindingKey {
            target: ExternBindingTarget::Function(ExternFunctionKey {
                module: self.module.clone(),
                name: name.to_string(),
            }),
            operation: ExternBindingOp::Call,
        });
        (descriptor, binding)
    }

    pub fn ty(&self, name: &str) -> (&RustTypeBinding, &ExternTypeDescriptor) {
        self.catalog
            .native_type_parts(
                &self.package,
                &self.provider,
                &ExternTypeKey {
                    module: self.module.clone(),
                    name: name.to_string(),
                },
            )
            .map(|(native, _, descriptor)| (native, descriptor))
            .expect("generated package must contain its type")
    }

    pub fn member(
        &self,
        owner: &str,
        selector: ExternMemberSelector,
        operation: ExternBindingOp,
    ) -> &RustExternBinding {
        self.binding(ExternBindingKey {
            target: ExternBindingTarget::Member(ExternMemberKey {
                owner: ExternTypeKey {
                    module: self.module.clone(),
                    name: owner.to_string(),
                },
                selector,
            }),
            operation,
        })
    }

    fn binding(&self, key: ExternBindingKey) -> &RustExternBinding {
        self.catalog
            .binding(&self.package, &self.provider, &key)
            .expect("generated package must contain its binding")
            .0
    }
}
