mod ast;
mod error;
mod lexer;
mod parser;
mod span;
mod typecheck;

// TODO: only ranges for now but eventually we will have more like
// options or stdlib, we need to work on imports and exports first
const CORE_PRELUDE: &str = include_str!("../core/ranges.anv");

fn parse_source(
    source: &str,
    file_path: &str,
) -> Result<(ast::Program, Vec<lexer::SpannedToken>), String> {
    let tokens = match lexer::tokenize(source) {
        Ok(tokens) => tokens,
        Err(errors) => {
            error::report_lexer_errors(source, file_path, errors);
            return Err("Failed to tokenize program".to_string());
        }
    };

    let ast = match parser::parse_ast(&tokens) {
        Ok(ast) => ast,
        Err(errors) => {
            error::report_parse_errors(source, file_path, &tokens, errors);
            return Err("Failed to parse program".to_string());
        }
    };

    Ok((ast, tokens))
}

pub fn generate_ast(program: &str, file_path: &str) -> Result<ast::Program, String> {
    let (prelude_ast, _) = parse_source(CORE_PRELUDE, "<prelude>")
        .map_err(|_| "Failed to parse prelude (internal error)".to_string())?;

    let (user_ast, user_tokens) = parse_source(program, file_path)?;

    let mut combined_stmts = prelude_ast.stmts;
    combined_stmts.extend(user_ast.stmts);
    let combined = ast::Program {
        stmts: combined_stmts,
    };

    if let Err(errors) = typecheck::check_program(&combined) {
        error::report_typecheck_errors(program, file_path, &user_tokens, errors);
        return Err("Failed to typecheck program".to_string());
    }

    Ok(combined)
}

pub fn run_program(program: &str, file_path: &str) -> Result<String, String> {
    let _ast = generate_ast(program, file_path)?;
    Ok("output".to_string())
}
