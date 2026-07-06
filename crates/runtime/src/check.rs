use std::ops::Range;

use crate::RuntimeError;

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
