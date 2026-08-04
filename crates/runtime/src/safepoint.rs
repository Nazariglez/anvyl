use std::{cell::Cell, fmt, rc::Rc};

use crate::{RuntimeError, RuntimeResult};

const ERR_GUARD_OVERFLOW: &str = "too many active runtime safepoint guards";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SafepointGuardKind {
    LambdaCell,
    MutPlace,
    Global,
    Collection,
    Collecting,
    HeapBorrow,
}

#[derive(Clone, Default)]
pub struct SafepointState {
    inner: Rc<SafepointCounters>,
}

#[derive(Default)]
struct SafepointCounters {
    lambda_cells: Cell<u32>,
    mut_places: Cell<u32>,
    globals: Cell<u32>,
    collections: Cell<u32>,
    collecting: Cell<u32>,
    heap_borrows: Cell<u32>,
}

pub struct SafepointGuard {
    state: SafepointState,
    kind: SafepointGuardKind,
}

impl SafepointState {
    pub fn enter(&self, kind: SafepointGuardKind) -> RuntimeResult<SafepointGuard> {
        let counter = self.counter(kind);
        counter.set(
            counter
                .get()
                .checked_add(1)
                .ok_or_else(|| RuntimeError::new(ERR_GUARD_OVERFLOW))?,
        );
        Ok(SafepointGuard {
            state: self.clone(),
            kind,
        })
    }

    pub fn validate_collect(&self) -> RuntimeResult<()> {
        if let Some(kind) = self.first_active() {
            return Err(RuntimeError::new(format!(
                "cannot collect while {kind} guard is active"
            )));
        }
        Ok(())
    }

    pub fn validate_reentry(&self) -> RuntimeResult<()> {
        if let Some(kind) = self.first_reentry_blocker() {
            return Err(RuntimeError::new(format!(
                "cannot reenter runtime while {kind} guard is active"
            )));
        }
        Ok(())
    }

    pub fn validate_heap_access(&self) -> RuntimeResult<()> {
        if self.active_count(SafepointGuardKind::HeapBorrow) != 0 {
            return Err(RuntimeError::new(
                "cannot access heap while heap borrow guard is active",
            ));
        }
        Ok(())
    }

    pub fn active_count(&self, kind: SafepointGuardKind) -> u32 {
        self.counter(kind).get()
    }

    fn first_active(&self) -> Option<SafepointGuardKind> {
        [
            SafepointGuardKind::LambdaCell,
            SafepointGuardKind::MutPlace,
            SafepointGuardKind::Global,
            SafepointGuardKind::Collection,
            SafepointGuardKind::Collecting,
            SafepointGuardKind::HeapBorrow,
        ]
        .into_iter()
        .find(|kind| self.active_count(*kind) != 0)
    }

    fn first_reentry_blocker(&self) -> Option<SafepointGuardKind> {
        [
            SafepointGuardKind::Collecting,
            SafepointGuardKind::HeapBorrow,
        ]
        .into_iter()
        .find(|kind| self.active_count(*kind) != 0)
    }

    fn counter(&self, kind: SafepointGuardKind) -> &Cell<u32> {
        match kind {
            SafepointGuardKind::LambdaCell => &self.inner.lambda_cells,
            SafepointGuardKind::MutPlace => &self.inner.mut_places,
            SafepointGuardKind::Global => &self.inner.globals,
            SafepointGuardKind::Collection => &self.inner.collections,
            SafepointGuardKind::Collecting => &self.inner.collecting,
            SafepointGuardKind::HeapBorrow => &self.inner.heap_borrows,
        }
    }
}

impl Drop for SafepointGuard {
    fn drop(&mut self) {
        let counter = self.state.counter(self.kind);
        let active = counter.get();
        debug_assert!(active > 0, "safepoint guard underflow");
        counter.set(active.saturating_sub(1));
    }
}

impl fmt::Display for SafepointGuardKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            Self::LambdaCell => "lambda cell",
            Self::MutPlace => "mutable place",
            Self::Global => "global",
            Self::Collection => "collection loan",
            Self::Collecting => "collection",
            Self::HeapBorrow => "heap borrow",
        };
        f.write_str(name)
    }
}
