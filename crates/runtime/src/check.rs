use std::ops::Range;

pub fn checked_index(index: i64, len: usize) -> usize {
    assert!(index >= 0, "negative index");
    let index = usize::try_from(index).expect("non-negative index must fit usize");
    assert!(index < len, "index out of bounds");
    index
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

#[cfg(test)]
mod tests {
    use super::{checked_index, checked_range};

    #[test]
    fn checked_index_accepts_valid_index() {
        assert_eq!(checked_index(2, 3), 2);
    }

    #[test]
    #[should_panic(expected = "negative index")]
    fn checked_index_rejects_negative_index() {
        checked_index(-1, 1);
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn checked_index_rejects_out_of_bounds_index() {
        checked_index(1, 1);
    }

    #[test]
    fn checked_range_accepts_valid_ranges() {
        assert_eq!(checked_range(0, 2, false, 3), 0..2);
        assert_eq!(checked_range(0, 2, true, 3), 0..3);
    }

    #[test]
    #[should_panic(expected = "negative range bound")]
    fn checked_range_rejects_negative_bounds() {
        checked_range(0, -1, true, 1);
    }

    #[test]
    #[should_panic(expected = "invalid range")]
    fn checked_range_rejects_reversed_bounds() {
        checked_range(2, 1, false, 3);
    }

    #[test]
    #[should_panic(expected = "range end overflow")]
    fn checked_range_rejects_inclusive_overflow() {
        checked_range(0, i64::MAX, true, 1);
    }

    #[test]
    #[should_panic(expected = "range out of bounds")]
    fn checked_range_rejects_out_of_bounds_end() {
        checked_range(0, 2, true, 2);
    }
}
