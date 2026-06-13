use std::ptr::NonNull;

use anvyx_runtime::{RuntimeError, ScopedLambda};

unsafe fn thunk(_: NonNull<()>, _: ()) -> Result<(), RuntimeError> {
    Ok(())
}

fn main() {
    let mut state = ();
    let _f = ScopedLambda::<'_, '_, (), ()>::__anvyx_from_raw(&mut state, thunk);
}
