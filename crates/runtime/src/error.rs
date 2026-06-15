use std::fmt;

use anvyx_heap::AccessError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeError {
    message: String,
}

impl RuntimeError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for RuntimeError {}

pub fn heap_access_error(error: AccessError) -> RuntimeError {
    let message = match error {
        AccessError::WrongHeap => "heap object belongs to a different heap",
        AccessError::DeadHandle => "heap object handle is no longer live",
    };
    RuntimeError::new(message)
}
