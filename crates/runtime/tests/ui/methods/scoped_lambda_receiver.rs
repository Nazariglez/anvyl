use anvyx_runtime::methods;

struct Host;

#[methods]
impl Host {
    pub fn bad(
        &self,
        f: anvyx_runtime::ScopedLambda<'_, '_, (i64,), ()>,
    ) -> anvyx_runtime::RuntimeResult<()> {
        let _ = self;
        f.call(1)
    }
}

fn main() {}
