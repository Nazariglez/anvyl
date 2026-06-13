use anvyx_runtime::methods;

struct Host;

#[methods]
impl Host {
    #[anvyx(ctx)]
    pub fn bad<'cx>(
        ctx: &mut anvyx_runtime::Ctx<'cx, '_>,
        f: anvyx_runtime::ScopedLambda<'_, '_, (i64,), ()>,
    ) -> Result<(), anvyx_runtime::RuntimeError> {
        let _ = ctx.heap();
        f.call(1)
    }
}

fn main() {}
