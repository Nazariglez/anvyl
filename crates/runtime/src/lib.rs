pub mod ctx;
pub mod cycle_collector;
pub mod error;
pub mod managed_rc;
pub mod provider;
pub mod suspect_buffer;
pub mod type_registry;

pub use anvyx_externs;
pub use anvyx_heap::{
    AccessError, CollectOutcome, CycleStatus, ErasedHandle, Handle, Heap, HeapConfig, HeapStats,
    HeapType, HeapTypeId, LeakReport, LeakTypeReport, RootId, Trace, TraceDriver, TraceMode,
    Visitor,
};
pub use anvyx_macros::{AnvyxInline, AnvyxRef, builtin_module, function, methods, module};
pub use ctx::Ctx;
pub use cycle_collector::{collect_cycles, set_auto_collect};
pub use error::RuntimeError;
pub use inventory;
pub use managed_rc::{
    CycleColor, CycleVtable, ManagedRc, ManagedRcInner, RcHeader, managed_alloc_count,
    managed_alloc_details, typed_dropper,
};
pub use provider::{
    AnvyxEnumExport, AnvyxInlineExport, AnvyxRefExport, BinaryOp, CallbackEscape, CallbackPolicy,
    CallbackThread, ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternCallbackParam,
    ExternCallbackSignature, ExternEffects, ExternFieldDescriptor, ExternFunctionDescriptor,
    ExternFunctionKey, ExternInitDescriptor, ExternMemberKey, ExternMemberSelector,
    ExternMethodDescriptor, ExternModuleDescriptor, ExternOperator, ExternOperatorDescriptor,
    ExternParam, ExternRep, ExternSignature, ExternStaticDescriptor, ExternTypeDescriptor,
    ExternTypeExpr, ExternTypeKey, FunctionExport, ModuleExport, ModuleExportItem, ModulePath,
    ParamFlow, ProviderDescriptor, ProviderId, ReceiverMode, RustAbiSupport, RustExternAbi,
    RustExternBinding, RustLocalBinding, RustMemberBinding, RustModuleSupport, RustParamAbi,
    RustPath, RustProviderCargo, RustProviderSupport, RustReturnAbi, RustTypeBinding, TypeExport,
    TypeMemberExport, UnaryOp, merge_type_members, validate_rust_provider_support,
};
pub use type_registry::{
    get_type_entry, is_cycle_capable, register_child_traverser, register_cycle_capable,
};
