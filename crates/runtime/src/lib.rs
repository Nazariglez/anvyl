mod callback_registry;
mod check;
pub mod collection;
mod collection_storage;
pub mod cow_storage;

pub mod ctx;
pub mod cycle_collector;
pub mod error;
mod escaping_lambda;
mod global_slot;
mod init_field;
mod lambda_cell;
pub mod managed_rc;
mod mutable_place;
pub mod provider;
mod resource;
mod runtime_owner;
mod scoped_lambda;
pub mod suspect_buffer;
pub mod type_registry;
pub mod value;

pub use anvyx_externs::{self, CALLBACK_WRAPPER_MAX_ARITY};
pub use anvyx_heap::{
    AccessError, CollectOutcome, CycleStatus, ErasedHandle, Handle, Heap, HeapConfig, HeapStats,
    HeapType, HeapTypeId, LeakReport, LeakTypeReport, RootId, Trace, TraceDriver, TraceMode,
    Visitor,
};
pub use anvyx_macros::{
    AnvyxEnum, AnvyxInline, AnvyxRef, builtin_module, function, methods, module, provider_package,
};
#[doc(hidden)]
pub use callback_registry::{
    CallbackCloseAction, CallbackCloseResult, CallbackInvocationGuard, CallbackSlot,
    CallbackSlotMeta, CallbackSlotState,
};
pub use check::{checked_index, checked_index_result, checked_range};
pub use collection::{CollectionLoanState, ShapeLoanGuard, ValueLoanGuard};
pub use collection_storage::{ListStorage, MapStorage};
pub use ctx::{Ctx, CtxRoots};
pub use cycle_collector::{collect_cycles, set_auto_collect};
pub use error::{RuntimeError, RuntimeResult, heap_access_error};
pub use escaping_lambda::{CallbackKey, EscapingLambda, EscapingLambdaCall, EscapingLambdaClose};
pub use global_slot::{
    GlobalProjectedLoanGuard, GlobalRef, GlobalRefMut, GlobalSlot, GlobalSlotState,
};
pub use init_field::AnvInitField;
pub use inventory;
pub use lambda_cell::{LambdaCell, StackLambdaCell};
pub use managed_rc::{
    CycleColor, CycleVtable, ManagedRc, ManagedRcInner, RcHeader, managed_alloc_count,
    managed_alloc_details, typed_dropper,
};
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
pub use scoped_lambda::ScopedLambda;
pub use type_registry::{
    get_type_entry, is_cycle_capable, register_child_traverser, register_cycle_capable,
};
pub use value::{AnvList, AnvMap, AnvSlice, AnvString, display_float};
