use std::ops::Range;

use anvyx_semantics::FloatToIntError;

use crate::{RuntimeError, RuntimeResult};

pub fn float_to_int(float: f64) -> RuntimeResult<i64> {
    anvyx_semantics::float_to_int(float).map_err(|error| {
        RuntimeError::new(match error {
            FloatToIntError::NonFinite => "cannot convert non-finite float to int",
            FloatToIntError::OutOfRange => "float is out of range for int",
        })
    })
}

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

pub fn checked_iter_step_by(step: i64) -> RuntimeResult<i64> {
    if step > 0 {
        Ok(step)
    } else {
        Err(RuntimeError::new("step_by requires a positive count"))
    }
}

pub fn checked_iter_skip(count: i64) -> RuntimeResult<i64> {
    if count >= 0 {
        Ok(count)
    } else {
        Err(RuntimeError::new("skip requires a non-negative count"))
    }
}

pub fn checked_iter_take(count: i64) -> RuntimeResult<i64> {
    if count >= 0 {
        Ok(count)
    } else {
        Err(RuntimeError::new("take requires a non-negative count"))
    }
}

pub struct AnvOrdinalIter {
    next: i128,
    step: i128,
    len: u128,
    ordinal: u128,
}

impl AnvOrdinalIter {
    #[must_use]
    pub fn collection(len: i64) -> Self {
        debug_assert!(len >= 0);
        Self::new(0, len as u128)
    }

    #[must_use]
    pub fn range(start: i64, end: i64, inclusive: bool) -> Self {
        Self::new(i128::from(start), range_len(start, end, inclusive))
    }

    fn new(next: i128, len: u128) -> Self {
        Self {
            next,
            step: 1,
            len,
            ordinal: 0,
        }
    }

    #[must_use]
    pub fn rev(mut self) -> Self {
        if self.len > 1 {
            self.advance(self.len - 1);
            self.step = -self.step;
        }
        self
    }

    #[must_use]
    pub fn skip(mut self, count: i64) -> Self {
        let count = iter_count(count).min(self.len);
        self.advance(count);
        self.len -= count;
        self
    }

    #[must_use]
    pub fn take(mut self, count: i64) -> Self {
        self.len = self.len.min(iter_count(count));
        self
    }

    #[must_use]
    pub fn step_by(mut self, step: i64) -> Self {
        let step = iter_count(step);
        debug_assert!(step > 0);
        let len = self.len.div_ceil(step);
        if len > 1 {
            let step = i128::try_from(step).expect("iterator step must fit i128");
            self.step = self
                .step
                .checked_mul(step)
                .expect("iterator stride must stay within source bounds");
        }
        self.len = len;
        self
    }

    fn advance(&mut self, count: u128) {
        if count == 0 || self.len == 0 {
            return;
        }
        let count = i128::try_from(count).expect("iterator offset must fit i128");
        let offset = self
            .step
            .checked_mul(count)
            .expect("iterator offset must stay within source bounds");
        self.next = self
            .next
            .checked_add(offset)
            .expect("iterator item must stay within source bounds");
    }
}

impl Iterator for AnvOrdinalIter {
    type Item = (i64, i64);

    fn next(&mut self) -> Option<Self::Item> {
        if self.len == 0 {
            return None;
        }
        let ordinal = self.ordinal;
        let item = self.next;
        self.len -= 1;
        self.ordinal = self.ordinal.checked_add(1)?;
        if self.len > 0 {
            self.next = self.next.checked_add(self.step)?;
        }
        let ordinal = i64::try_from(ordinal).ok()?;
        let item = i64::try_from(item).ok()?;
        Some((ordinal, item))
    }
}

fn iter_count(count: i64) -> u128 {
    debug_assert!(count >= 0);
    count as u128
}

fn range_len(start: i64, end: i64, inclusive: bool) -> u128 {
    if start > end || (!inclusive && start == end) {
        return 0;
    }
    let span = i128::from(end) - i128::from(start);
    let len = if inclusive { span + 1 } else { span };
    u128::try_from(len).expect("valid range length must fit u128")
}
