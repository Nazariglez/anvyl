use anvyx_runtime::methods;

struct Score;

#[methods]
impl Score {
    #[anvyx(ctx)]
    pub fn bump<'cx>(&mut self, ctx: &mut anvyx_runtime::Ctx<'cx, '_>) {
        let _ = ctx;
    }
}

fn main() {}
