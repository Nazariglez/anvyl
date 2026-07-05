pub use anvyx_runtime::legacy_gc::{ManagedRc, managed_alloc_count, managed_alloc_details};
#[cfg(test)]
pub use anvyx_runtime::legacy_gc::{rc_dec_count, rc_inc_count, reset_rc_counts};
