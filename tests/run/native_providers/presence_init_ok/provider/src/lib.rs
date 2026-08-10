use anvyx_runtime::{methods, AnvInitField, AnvyxRef};

#[derive(AnvyxRef)]
#[anvyx(name = "Thing")]
pub struct Thing {
    #[anvyx(field)]
    pub value: i64,
    #[anvyx(field)]
    pub tag: std::option::Option<i64>,
    tag_code: i64,
}

#[methods]
impl Thing {
    #[anvyx(init)]
    pub fn new(value: i64, tag: AnvInitField<Option<i64>>) -> Thing {
        let tag_code = match tag {
            AnvInitField::Provided(Some(value)) => value,
            AnvInitField::Provided(None) => -1,
            AnvInitField::Omitted => -2,
        };
        Self {
            value,
            tag: None,
            tag_code,
        }
    }

    pub fn tag_code(&self) -> i64 {
        self.tag_code
    }
}

anvyx_runtime::builtin_module! {
    name: "host",
    exports: [Thing],
}
