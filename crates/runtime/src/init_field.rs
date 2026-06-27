#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AnvInitField<T> {
    Omitted,
    Provided(T),
}
