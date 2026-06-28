use super::{rir::RirLambdaSigId, target};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct RetainedCallbackSigPlan {
    sig: RirLambdaSigId,
}

impl RetainedCallbackSigPlan {
    pub(super) fn new(sig: RirLambdaSigId) -> Self {
        Self { sig }
    }

    pub(super) fn sig_index(self) -> usize {
        self.sig.index()
    }

    pub(super) fn table_id(self) -> usize {
        self.sig.index() + 1
    }

    pub(super) fn signature_id(self) -> usize {
        self.sig.index() + 1
    }

    pub(super) fn table_field(self) -> String {
        target::callback_table_field(self.sig.index())
    }

    pub(super) fn record_symbol(self) -> String {
        target::callback_record_symbol(self.sig.index())
    }

    pub(super) fn call_thunk_symbol(self) -> String {
        target::callback_call_thunk_symbol(self.sig.index())
    }

    pub(super) fn close_thunk_symbol(self) -> String {
        target::callback_close_thunk_symbol(self.sig.index())
    }

    pub(super) fn heap_type_field(self) -> String {
        target::callback_record_heap_type_field(self.sig.index())
    }
}
