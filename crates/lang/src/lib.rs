mod ast;
mod error;
mod lexer;
mod parser;
mod span;

pub fn generate_ast(program: &str) -> Result<ast::Program, String> {
    let tokens = match lexer::tokenize(program) {
        Ok(tokens) => tokens,
        Err(errors) => {
            error::report_lexer_errors(program, errors);
            return Err("Failed to tokenize program".to_string());
        }
    };

    let ast = match parser::parse_ast(&tokens) {
        Ok(ast) => ast,
        Err(errors) => {
            error::report_parse_errors(program, &tokens, errors);
            return Err("Failed to parse program".to_string());
        }
    };

    // let ast = match typecheck::check_program(&ast) {
    //     Ok(_) => Ok(()),
    //     Err(errors) => {
    //         println!("Typecheck errors: {:?}", errors);
    //         // error::report_typecheck_errors(program, errors);
    //         Err("Failed to typecheck program".to_string())
    //     }
    // };
    Ok(ast)
}

pub fn run_program(program: &str) -> Result<String, String> {
    let _ast = generate_ast(program)?;
    Ok("output".to_string())
}
