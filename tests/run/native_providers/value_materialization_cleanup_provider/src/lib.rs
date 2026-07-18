use anvyx_runtime::{AnvyxEnum, function};

#[derive(PartialEq, Eq, Hash, AnvyxEnum)]
pub enum Probe {
    Value(i64),
}

impl Clone for Probe {
    fn clone(&self) -> Self {
        match self {
            Self::Value(id) => {
                eprintln!("materialize {id}");
                Self::Value(*id)
            }
        }
    }
}

impl Drop for Probe {
    fn drop(&mut self) {
        match self {
            Self::Value(id) => eprintln!("drop {id}"),
        }
    }
}

#[function(ret = "Probe")]
pub fn make_probe(id: i64) -> Probe {
    eprintln!("make {id}");
    Probe::Value(id)
}

#[function(params(probe = "Probe"))]
pub fn probe_id(probe: Probe) -> i64 {
    match probe {
        Probe::Value(id) => id,
    }
}

anvyx_runtime::builtin_module! {
    name: "cleanup",
    source: "",
    exports: [Probe, make_probe, probe_id],
}
