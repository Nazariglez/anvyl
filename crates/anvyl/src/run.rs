use anvyl_lang::run_program;
use std::fs;
use std::path::Path;

pub fn cmd(file: &Path) -> Result<(), String> {
    let program = fs::read_to_string(file).map_err(|e| format!("Failed to read file: {}", e))?;
    let file_path = file.to_string_lossy().to_string();
    let ast =
        run_program(&program, &file_path).map_err(|e| format!("Failed to run program: {}", e))?;
    println!("AST: {:?}", ast);
    Ok(())
}
