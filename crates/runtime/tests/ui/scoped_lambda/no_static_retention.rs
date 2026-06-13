use anvyx_runtime::ScopedLambda;

fn retain<'call, 'cx>(f: ScopedLambda<'call, 'cx, (), ()>) -> ScopedLambda<'static, 'cx, (), ()> {
    f
}

fn main() {}
