macro_rules! define_id {
    ($name:ident) => {
        #[repr(transparent)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(pub u32);

        impl $name {
            pub const fn from_index(index: usize) -> Self {
                Self(index as u32)
            }

            pub const fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}

define_id!(ModuleId);
define_id!(FunctionId);
define_id!(LambdaId);
define_id!(LambdaCaptureSlotId);
define_id!(BindingId);
define_id!(AggregateId);
define_id!(EnumId);
define_id!(FlagId);
define_id!(FlagMemberId);
define_id!(ExternId);
define_id!(ExternTypeId);
define_id!(TypeId);
define_id!(ConstId);
define_id!(LocalId);
define_id!(ScopedBorrowId);
define_id!(DynBorrowParamId);
define_id!(CaptureCellId);
define_id!(GlobalId);
define_id!(BlockId);
define_id!(AirLoopId);
define_id!(FieldId);
define_id!(VariantId);
define_id!(ContractSurfaceId);
define_id!(ContractSlotId);
define_id!(ContractWitnessId);
define_id!(ContractWeakeningId);
