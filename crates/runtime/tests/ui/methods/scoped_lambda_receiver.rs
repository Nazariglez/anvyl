use anvyx_runtime::methods;

struct Host;

#[methods]
impl Host {
    pub fn bad(
        &self,
        f: anvyx_runtime::ScopedLambda<'_, '_, (i64,), ()>,
    ) -> Result<(), anvyx_runtime::RuntimeError> {
        let _ = self;
        f.call(1)
    }
}

fn main() {}
