mod anv_callback;
mod callback_registry;
mod check;
pub mod collection;
mod collection_storage;
pub mod cow_storage;

pub mod ctx;
mod cycle_collector;
pub mod error;
mod escaping_lambda;
mod global_slot;
mod init_field;
mod lambda_cell;
mod managed_rc;
mod mutable_place;
pub mod provider;
mod resource;
mod runtime_owner;
mod safepoint;
mod scoped_lambda;
mod suspect_buffer;
pub mod value;

pub mod legacy_gc {
    //! Legacy VM-only GC API. Clean runtime/provider code must use `anvyx_heap` handles.

    pub use crate::{
        cycle_collector::{
            clear_suspects, collect_cycles, reset_collect_threshold, set_auto_collect,
            suspect_count,
        },
        managed_rc::{
            ChildrenVisitorFn, CycleColor, CycleVtable, ManagedRc, ManagedRcInner, RcHeader,
            managed_alloc_count, managed_alloc_details, rc_dec_count, rc_inc_count,
            reset_rc_counts, typed_dropper,
        },
    };
}

pub use anv_callback::AnvCallback;
pub use anvyx_externs::{self, CALLBACK_WRAPPER_MAX_ARITY};
pub use anvyx_heap::{
    AccessError, CollectOutcome, CycleStatus, ErasedHandle, Handle, Heap, HeapConfig, HeapStats,
    HeapType, HeapTypeId, LeakReport, LeakTypeReport, Trace, TraceDriver, TraceMode, Visitor,
};
pub use anvyx_macros::{
    AnvyxEnum, AnvyxInline, AnvyxRef, builtin_module, function, methods, module, provider_package,
};
#[doc(hidden)]
pub use callback_registry::{
    CallbackCloseResult, CallbackInvocationGuard, CallbackSlot, CallbackSlotState,
};
pub use check::{checked_index, checked_index_result, checked_range};
pub use collection::{CollectionLoanState, ShapeLoanGuard, ValueLoanGuard};
pub use collection_storage::{ListStorage, MapStorage};
pub use ctx::{Ctx, HeapBorrowMut, HeapBorrowRef, TraceRootSet};
pub use error::{RuntimeError, RuntimeResult, heap_access_error};
pub use escaping_lambda::{CallbackKey, EscapingLambda, EscapingLambdaCall, EscapingLambdaClose};
pub use global_slot::{
    GlobalProjectedLoanGuard, GlobalRef, GlobalRefMut, GlobalSlot, GlobalSlotState,
};
pub use init_field::AnvInitField;
pub use inventory;
pub use lambda_cell::{LambdaCell, StackLambdaCell};
pub use mutable_place::{
    DataRefPlace, DataRefPlaceOps, MapValueOps, MutPlace, OptionalPayloadOps, ProjectedPlace,
    ProjectionOps, ScopedMutPlaceCell,
};
pub use provider::{
    AnvyxEnumExport, AnvyxInlineExport, AnvyxRefExport, BinaryOp, CallbackEscape, CallbackPolicy,
    CallbackThread, ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternCallbackParam,
    ExternCallbackSignature, ExternEffects, ExternEnumVariantDescriptor,
    ExternEnumVariantFieldDescriptor, ExternFieldDescriptor, ExternFunctionDescriptor,
    ExternFunctionKey, ExternInitDescriptor, ExternMemberKey, ExternMemberSelector,
    ExternMethodDescriptor, ExternModuleDescriptor, ExternOperator, ExternOperatorDescriptor,
    ExternParam, ExternRep, ExternSignature, ExternStaticDescriptor, ExternTypeDescriptor,
    ExternTypeExpr, ExternTypeKey, FunctionExport, ModuleExport, ModuleExportItem, ModulePath,
    ParamFlow, ProviderDescriptor, ProviderId, ReceiverMode, RustAbiSupport, RustExternAbi,
    RustExternBinding, RustLocalBinding, RustMemberBinding, RustModuleSupport, RustParamAbi,
    RustPath, RustProviderCargo, RustProviderSupport, RustReturnAbi, RustTypeBinding,
    RustWrapperCtx, TypeExport, TypeMemberExport, UnaryOp, merge_type_members,
    validate_rust_provider_support,
};
pub use resource::{AnvRef, AnvRefType};
pub use runtime_owner::{RuntimeOwnerEntry, RuntimeOwnerHandle, RuntimeOwnerShutdownGuard};
pub use safepoint::{SafepointGuard, SafepointGuardKind, SafepointState};
pub use scoped_lambda::ScopedLambda;
pub use value::{AnvList, AnvMap, AnvSlice, AnvString, display_float};
