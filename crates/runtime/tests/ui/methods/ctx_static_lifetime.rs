use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(ctx)]
    pub fn bad(&self, ctx: &mut anvyx_runtime::Ctx<'static, '_>) -> i64 {
        let _ = ctx.heap();
        0
    }
}

fn main() {}
