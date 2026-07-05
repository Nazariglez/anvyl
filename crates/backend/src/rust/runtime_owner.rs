use super::target;

pub(super) struct RuntimeOwnerEmit;

impl RuntimeOwnerEmit {
    pub(super) fn attach_line(owner: &str, ptr: &str) -> String {
        format!(
            "{}.expect(\"runtime owner attach failed\");",
            target::owner_attach(owner, ptr)
        )
    }

    pub(super) fn enter_current_line(binding: &str, owner: &str) -> String {
        format!("let {binding} = {}?;", target::owner_enter_current(owner))
    }

    pub(super) fn enter_line(
        binding: &str,
        owner: &str,
        owner_id: &str,
        shutdown_generation: &str,
    ) -> String {
        format!(
            "let {binding} = {}?;",
            target::owner_enter(owner, owner_id, shutdown_generation)
        )
    }

    pub(super) fn entry_ptr_cast_line(binding: &str, entry: &str, ty: &str) -> String {
        format!(
            "let mut {binding} = {}.cast::<{ty}>();",
            target::owner_entry_ptr(entry)
        )
    }

    pub(super) fn reentry_gate_line(inner_ptr: &str) -> String {
        format!("{};", target::runtime_inner_validate_reentry(inner_ptr))
    }

    pub(super) fn callback_entry_lines(
        entry: &str,
        inner_ptr: &str,
        owner_id: &str,
        shutdown_generation: &str,
        inner_ty: &str,
    ) -> [String; 3] {
        [
            Self::enter_line(entry, "owner", owner_id, shutdown_generation),
            Self::entry_ptr_cast_line(inner_ptr, entry, inner_ty),
            Self::reentry_gate_line(inner_ptr),
        ]
    }

    pub(super) fn provider_suspended_call(
        owner: &str,
        entry: &str,
        result: &str,
        call: &str,
    ) -> String {
        let suspend = target::owner_suspend_for_provider(owner);
        format!("{{ let {entry} = {suspend}?; let {result} = {call}; drop({entry}); {result} }}")
    }

    pub(super) fn drop_line(entry: &str) -> String {
        format!("drop({entry});")
    }
}
