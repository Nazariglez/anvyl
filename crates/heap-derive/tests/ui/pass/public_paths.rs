mod qualified_derive {
    #[derive(anvyx_heap::Trace)]
    struct PathDerive<'cx> {
        child: Option<anvyx_heap::Handle<'cx, PathDerive<'cx>>>,
    }
}

mod imported_derive {
    use anvyx_heap::Trace;

    #[derive(Trace)]
    struct ImportedDerive<'cx> {
        child: Option<anvyx_heap::Handle<'cx, ImportedDerive<'cx>>>,
    }
}

mod shapes {
    use anvyx_heap::Trace;

    #[derive(Trace)]
    struct Named<'cx> {
        child: Option<anvyx_heap::Handle<'cx, Named<'cx>>>,
    }

    #[derive(Trace)]
    struct Tuple<'cx>(Option<anvyx_heap::Handle<'cx, Tuple<'cx>>>);

    #[derive(Trace)]
    struct Unit;

    #[derive(Trace)]
    enum Shape<'cx> {
        Named {
            child: Option<anvyx_heap::Handle<'cx, Shape<'cx>>>,
            visitor: i32,
        },
        Tuple(Option<anvyx_heap::Handle<'cx, Shape<'cx>>>),
        Unit,
    }
}

mod lifetimes {
    use anvyx_heap::Trace;

    #[derive(Trace)]
    struct NoLifetime {
        scalar: i32,
    }

    #[derive(Trace)]
    #[trace(ctx = 'cx)]
    struct ExplicitNoLifetime {
        scalar: i32,
    }

    #[derive(Trace)]
    #[trace(ctx = 'cx)]
    struct Explicit<'cx> {
        child: Option<anvyx_heap::Handle<'cx, Explicit<'cx>>>,
    }
}

mod explicit_crate_path {
    mod heap_api {
        pub use anvyx_heap::*;
    }

    #[derive(anvyx_heap::Trace)]
    #[trace(crate = heap_api)]
    struct RelativeCrate<'cx> {
        child: Option<heap_api::Handle<'cx, RelativeCrate<'cx>>>,
    }

    #[derive(anvyx_heap::Trace)]
    #[trace(crate = ::anvyx_heap)]
    struct AbsoluteCrate<'cx> {
        child: Option<anvyx_heap::Handle<'cx, AbsoluteCrate<'cx>>>,
    }
}

mod generic_name_collisions {
    use anvyx_heap::Trace;

    #[derive(Trace)]
    struct TypeD<D> {
        value: D,
    }

    #[derive(Trace)]
    struct TypeDriver<AnvyxTraceDriver> {
        value: AnvyxTraceDriver,
    }

    #[derive(Trace)]
    struct ConstD<const D: usize> {
        values: [i32; D],
    }
}

mod skipped_fields {
    use anvyx_heap::Trace;

    #[derive(Trace)]
    struct SkipUnchecked {
        #[trace(skip_unchecked)]
        not_trace: std::rc::Rc<std::cell::Cell<i32>>,
    }
}

fn main() {}
