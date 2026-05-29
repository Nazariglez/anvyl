use std::cell::Cell;

macro_rules! reset_metric {
    ($metrics:ident, live) => {};
    ($metrics:ident, live_peak) => {
        $metrics.live_peak.set($metrics.live.get());
    };
    ($metrics:ident, $field:ident) => {
        $metrics.$field.set(0);
    };
}

macro_rules! metrics {
    ($($field:ident),+ $(,)?) => {
        #[derive(Debug, Default)]
        pub(crate) struct MetricsCells {
            $(pub $field: Cell<usize>,)+
        }

        impl MetricsCells {
            pub(crate) fn snapshot(&self) -> HeapStats {
                HeapStats {
                    $($field: self.$field.get(),)+
                }
            }

            pub(crate) fn reset(&self) {
                $(reset_metric!(self, $field);)+
            }
        }

        #[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
        pub struct HeapStats {
            $(pub $field: usize,)+
        }
    };
}

metrics!(
    allocs,
    clones,
    drops,
    collected,
    live,
    live_peak,
    suspects,
    suspects_processed,
    suspects_peak,
    zeroes_peak,
    cycle_batches,
    cycle_roots,
    stale_roots,
    candidate_nodes,
    edge_visits,
    internal_edges,
    blackened,
    max_candidate,
    max_batch_cost,
    budget_overshoots,
    budget_debt_peak,
);
