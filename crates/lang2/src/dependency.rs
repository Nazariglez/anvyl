#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DependencyAliasError {
    Invalid,
}

pub fn validate_dependency_alias(alias: &str) -> Result<(), DependencyAliasError> {
    if is_normalized_alias(alias) && anvyx_frontend::lexer::is_source_ident(alias) {
        return Ok(());
    }
    Err(DependencyAliasError::Invalid)
}

fn is_normalized_alias(alias: &str) -> bool {
    let mut bytes = alias.bytes();
    let Some(first) = bytes.next() else {
        return false;
    };
    first.is_ascii_lowercase()
        && bytes.all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'_')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dependency_alias_accepts_normalized_identifiers() {
        for alias in ["math", "my_engine", "math2", "std"] {
            assert_eq!(validate_dependency_alias(alias), Ok(()));
        }
    }

    #[test]
    fn dependency_alias_rejects_invalid_or_unnormalized_names() {
        for alias in ["", "Math", "_math", "bad-name", "fn"] {
            assert_eq!(
                validate_dependency_alias(alias),
                Err(DependencyAliasError::Invalid)
            );
        }
    }
}
