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

pub fn checked_range(
    start: i64,
    end: i64,
    inclusive: bool,
    len: usize,
) -> RuntimeResult<Range<usize>> {
    if start < 0 || end < 0 {
        return Err(RuntimeError::new("negative range bound"));
    }
    if start > end {
        return Err(RuntimeError::new("invalid range"));
    }
    let end = if inclusive {
        end.checked_add(1)
            .ok_or_else(|| RuntimeError::new("range end overflow"))?
    } else {
        end
    };
    let start = usize::try_from(start).map_err(|_| RuntimeError::new("range out of bounds"))?;
    let end = usize::try_from(end).map_err(|_| RuntimeError::new("range out of bounds"))?;
    if end > len {
        return Err(RuntimeError::new("range out of bounds"));
    }
    Ok(start..end)
}

pub fn checked_for_step(step: i64) -> RuntimeResult<i64> {
    if step > 0 {
        Ok(step)
    } else {
        Err(RuntimeError::new("for-loop step must be positive"))
    }
}

pub struct AnvCollectionIter(AnvRangeIter);

impl AnvCollectionIter {
    pub fn new(len: i64, reversed: bool, step: i64) -> Self {
        debug_assert!(len >= 0);
        Self(AnvRangeIter::new(0, len, false, reversed, step))
    }
}

impl Iterator for AnvCollectionIter {
    type Item = (i64, i64);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
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
    use super::{AnvCollectionIter, AnvRangeIter};

    fn items(iter: impl Iterator<Item = (i64, i64)>) -> Vec<(i64, i64)> {
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

    #[test]
    fn collection_iter_handles_order_step_and_empty() {
        assert_eq!(items(AnvCollectionIter::new(0, false, 1)), vec![]);
        assert_eq!(
            items(AnvCollectionIter::new(5, false, 2)),
            vec![(0, 0), (1, 2), (2, 4)]
        );
        assert_eq!(
            items(AnvCollectionIter::new(5, true, 2)),
            vec![(0, 4), (1, 2), (2, 0)]
        );
        assert_eq!(items(AnvCollectionIter::new(3, true, 10)), vec![(0, 2)]);
        assert_eq!(
            items(AnvCollectionIter::new(3, false, i64::MAX)),
            vec![(0, 0)]
        );
    }
}
