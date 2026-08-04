use std::{fmt::Display, hash::Hash};

use internment::Intern;

#[derive(Debug, Clone)]
pub struct NumericLiteral<T> {
    spelling: Intern<String>,
    value: T,
}

impl<T> NumericLiteral<T> {
    pub(crate) fn new(spelling: impl Into<String>, value: T) -> Self {
        Self {
            spelling: Intern::new(spelling.into()),
            value,
        }
    }

    pub fn spelling(&self) -> &str {
        self.spelling.as_str()
    }

    pub fn value(&self) -> &T {
        &self.value
    }

    pub(crate) fn into_value(self) -> T {
        self.value
    }

    pub(crate) fn map<U>(self, f: impl FnOnce(T) -> U) -> NumericLiteral<U> {
        NumericLiteral {
            spelling: self.spelling,
            value: f(self.value),
        }
    }
}

impl<T: Display> NumericLiteral<T> {
    fn canonical(value: T) -> Self {
        Self::new(value.to_string(), value)
    }
}

impl<T: PartialEq> PartialEq for NumericLiteral<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: Eq> Eq for NumericLiteral<T> {}

impl<T: Hash> Hash for NumericLiteral<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl From<i64> for NumericLiteral<i64> {
    fn from(value: i64) -> Self {
        Self::canonical(value)
    }
}

impl From<usize> for NumericLiteral<usize> {
    fn from(value: usize) -> Self {
        Self::canonical(value)
    }
}

impl From<f64> for NumericLiteral<f64> {
    fn from(value: f64) -> Self {
        let mut spelling = value.to_string();
        if value.is_finite()
            && !spelling.contains('.')
            && !spelling.contains('e')
            && !spelling.contains('E')
        {
            spelling.push_str(".0");
        }
        Self::new(spelling, value)
    }
}

impl<T> Display for NumericLiteral<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.spelling())
    }
}
