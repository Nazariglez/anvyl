use std::collections::{HashMap, hash_map::Entry};

use anvyx_externs::ExternMemberSelector;

use super::raw::*;

pub(crate) fn validate_raw_identities(raw: &RawExterns) -> Result<(), Vec<ExternInputError>> {
    let mut validator = RawIdentityValidator::default();

    for group in &raw.groups {
        for module in &group.modules {
            validator.check_module(module, &group.provenance);
        }
    }

    if validator.errors.is_empty() {
        Ok(())
    } else {
        Err(validator.errors)
    }
}

#[derive(Default)]
struct RawIdentityValidator {
    functions: HashMap<RawExternFunctionKey, RawExternDecl>,
    types: HashMap<RawExternTypeKey, RawExternDecl>,
    members: HashMap<RawExternMemberKey, RawExternDecl>,
    errors: Vec<ExternInputError>,
}

impl RawIdentityValidator {
    fn check_module(&mut self, module: &RawExternModule, provenance: &ExternProvenance) {
        for function in &module.functions {
            let key = RawExternFunctionKey {
                module: module.scope.clone(),
                name: function.decl.name.clone(),
            };
            let duplicate = RawExternDecl {
                provenance: provenance.clone(),
                site: function.site,
            };
            match self.functions.entry(key.clone()) {
                Entry::Vacant(entry) => {
                    entry.insert(duplicate);
                }
                Entry::Occupied(entry) => self.errors.push(duplicate_identity(
                    RawExternIdentityKey::Function(key),
                    entry.get().clone(),
                    duplicate,
                )),
            }
        }

        for ty in &module.types {
            self.check_type(module, provenance, ty);
        }
    }

    fn check_type(
        &mut self,
        module: &RawExternModule,
        provenance: &ExternProvenance,
        ty: &RawExternType,
    ) {
        let owner = RawExternTypeKey {
            module: module.scope.clone(),
            name: ty.name.clone(),
        };
        let duplicate = RawExternDecl {
            provenance: provenance.clone(),
            site: ty.site,
        };
        match self.types.entry(owner.clone()) {
            Entry::Vacant(entry) => {
                entry.insert(duplicate);
            }
            Entry::Occupied(entry) => self.errors.push(duplicate_identity(
                RawExternIdentityKey::Type(owner.clone()),
                entry.get().clone(),
                duplicate,
            )),
        }

        for field in &ty.fields {
            self.check_member_site(
                &owner,
                ExternMemberSelector::Field(field.decl.name.clone()),
                provenance,
                field.site,
                ty.site,
            );
        }
        if let Some(init) = &ty.init {
            self.check_member_site(
                &owner,
                ExternMemberSelector::Init,
                provenance,
                init.site,
                ty.site,
            );
        }
        for method in &ty.methods {
            self.check_member_site(
                &owner,
                ExternMemberSelector::Method(method.decl.name.clone()),
                provenance,
                method.site,
                ty.site,
            );
        }
        for method in &ty.statics {
            self.check_member_site(
                &owner,
                ExternMemberSelector::Static(method.decl.name.clone()),
                provenance,
                method.site,
                ty.site,
            );
        }
        for operator in &ty.operators {
            self.check_member_site(
                &owner,
                ExternMemberSelector::Operator(operator.decl.op),
                provenance,
                operator.site,
                ty.site,
            );
        }
    }

    fn check_member_site(
        &mut self,
        owner: &RawExternTypeKey,
        selector: ExternMemberSelector,
        provenance: &ExternProvenance,
        member_site: RawExternSite,
        owner_site: RawExternSite,
    ) {
        self.check_member(
            RawExternMemberKey {
                owner: owner.clone(),
                selector,
            },
            provenance,
            site_or_owner(member_site, owner_site),
        );
    }

    fn check_member(
        &mut self,
        key: RawExternMemberKey,
        provenance: &ExternProvenance,
        site: RawExternSite,
    ) {
        let duplicate = RawExternDecl {
            provenance: provenance.clone(),
            site,
        };
        match self.members.entry(key.clone()) {
            Entry::Vacant(entry) => {
                entry.insert(duplicate);
            }
            Entry::Occupied(entry) => self.errors.push(duplicate_identity(
                RawExternIdentityKey::Member(key),
                entry.get().clone(),
                duplicate,
            )),
        }
    }
}

fn duplicate_identity(
    key: RawExternIdentityKey,
    first: RawExternDecl,
    duplicate: RawExternDecl,
) -> ExternInputError {
    ExternInputError::DuplicateRawIdentity {
        key,
        first,
        duplicate,
    }
}

fn site_or_owner(site: RawExternSite, owner: RawExternSite) -> RawExternSite {
    if site.span.is_some() { site } else { owner }
}
