use std::ops::Range;

use crate::{RuntimeError, RuntimeResult};

pub fn checked_index(index: i64, len: usize) -> usize {
    assert!(index >= 0, "negative index");
    let index = usize::try_from(index).expect("non-negative index must fit usize");
    assert!(index < len, "index out of bounds");
    index
}

pub fn checked_index_result(index: i64, len: usize, kind: &str) -> Result<usize, RuntimeError> {
    let valid = index >= 0 && usize::try_from(index).is_ok_and(|index| index < len);
    if valid {
        Ok(index as usize)
    } else {
        Err(RuntimeError::new(format!(
            "{kind} index {index} out of bounds for len {len}"
        )))
    }
}

pub fn checked_range(start: i64, end: i64, inclusive: bool, len: usize) -> Range<usize> {
    assert!(start >= 0 && end >= 0, "negative range bound");
    assert!(start <= end, "invalid range");
    let end = if inclusive {
        end.checked_add(1).expect("range end overflow")
    } else {
        end
    };
    let start = usize::try_from(start).expect("non-negative range start must fit usize");
    let end = usize::try_from(end).expect("non-negative range end must fit usize");
    assert!(end <= len, "range out of bounds");
    start..end
}

pub fn checked_for_step(step: i64) -> RuntimeResult<i64> {
    if step > 0 {
        Ok(step)
    } else {
        Err(RuntimeError::new("for-loop step must be positive"))
    }
}

pub struct AnvRangeIter {
    next: Option<i64>,
    last: i64,
    step: i64,
    ordinal: i64,
}

impl AnvRangeIter {
    pub fn new(start: i64, end: i64, inclusive: bool, reversed: bool, step: i64) -> Self {
        debug_assert!(step > 0);
        let empty = if inclusive { start > end } else { start >= end };
        let last = if inclusive || empty {
            end
        } else {
            end.checked_sub(1)
                .expect("non-empty exclusive range end must have predecessor")
        };
        let (next, last, step) = if reversed {
            (last, start, -step)
        } else {
            (start, last, step)
        };
        Self {
            next: (!empty).then_some(next),
            last,
            step,
            ordinal: 0,
        }
    }

    fn in_bounds(&self, item: i64) -> bool {
        if self.step > 0 {
            item <= self.last
        } else {
            item >= self.last
        }
    }
}

impl Iterator for AnvRangeIter {
    type Item = (i64, i64);

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.next?;
        if !self.in_bounds(item) {
            self.next = None;
            return None;
        }

        let ordinal = self.ordinal;
        self.next = item.checked_add(self.step);
        match self.ordinal.checked_add(1) {
            Some(next) => self.ordinal = next,
            None => self.next = None,
        }
        Some((ordinal, item))
    }
}

#[cfg(test)]
mod tests {
    use super::AnvRangeIter;

    fn items(iter: AnvRangeIter) -> Vec<(i64, i64)> {
        iter.collect()
    }

    #[test]
    fn range_iter_handles_overflow_edges() {
        assert_eq!(
            items(AnvRangeIter::new(i64::MAX - 1, i64::MAX, true, false, 1)),
            vec![(0, i64::MAX - 1), (1, i64::MAX)]
        );
        assert_eq!(
            items(AnvRangeIter::new(i64::MIN, i64::MIN, true, true, 1)),
            vec![(0, i64::MIN)]
        );
    }

    #[test]
    fn range_iter_handles_reverse_empty_and_large_step() {
        assert_eq!(items(AnvRangeIter::new(3, 3, false, true, 1)), vec![]);
        assert_eq!(items(AnvRangeIter::new(4, 3, false, true, 1)), vec![]);
        assert_eq!(
            items(AnvRangeIter::new(0, 5, false, true, 20)),
            vec![(0, 4)]
        );
    }
}
