use std::{
    cell::{Cell, RefCell},
    ptr::NonNull,
};

use crate::{cycle_collector::CollectStats, managed_rc::RcHeader};

pub(crate) const MIN_THRESHOLD: usize = 256;
pub(crate) const MAX_THRESHOLD: usize = 4096;
const DEFAULT_THRESHOLD: usize = 256;

pub struct SuspectEntry {
    pub ptr: NonNull<RcHeader>,
}

thread_local! {
    pub(crate) static SUSPECT_BUFFER: RefCell<Vec<SuspectEntry>> = const { RefCell::new(vec![]) };
    static AUTO_COLLECT_ENABLED: Cell<bool> = const { Cell::new(true) };
    static COLLECT_THRESHOLD: Cell<usize> = const { Cell::new(DEFAULT_THRESHOLD) };
}

pub fn set_auto_collect(enabled: bool) {
    AUTO_COLLECT_ENABLED.with(|flag| flag.set(enabled));
}

pub fn push_suspect(ptr: NonNull<RcHeader>) {
    SUSPECT_BUFFER.with(|buf| {
        buf.borrow_mut().push(SuspectEntry { ptr });
    });
    maybe_collect();
}

pub fn maybe_collect() {
    let threshold = COLLECT_THRESHOLD.with(Cell::get);
    let should = AUTO_COLLECT_ENABLED.with(Cell::get)
        && SUSPECT_BUFFER.with(|buf| buf.borrow().len() >= threshold);
    if should {
        let stats = crate::cycle_collector::collect_cycles();
        adjust_threshold(&stats);
    }
}

fn adjust_threshold(stats: &CollectStats) {
    if stats.suspects == 0 {
        return;
    }
    COLLECT_THRESHOLD.with(|t| {
        let current = t.get();
        if stats.freed.saturating_mul(2) >= stats.suspects {
            t.set((current * 2 / 3).max(MIN_THRESHOLD));
        } else if stats.freed.saturating_mul(10) < stats.suspects {
            t.set((current * 3 / 2).min(MAX_THRESHOLD));
        }
    });
}

pub fn suspect_count() -> usize {
    SUSPECT_BUFFER.with(|buf| buf.borrow().len())
}

pub fn clear_suspects() {
    SUSPECT_BUFFER.with(|buf| buf.borrow_mut().clear());
}

#[cfg(test)]
pub fn get_collect_threshold() -> usize {
    COLLECT_THRESHOLD.with(Cell::get)
}

pub fn reset_collect_threshold() {
    COLLECT_THRESHOLD.with(|t| t.set(DEFAULT_THRESHOLD));
}

#[cfg(test)]
pub fn set_collect_threshold(val: usize) {
    COLLECT_THRESHOLD.with(|t| t.set(val));
}
