// Lexer -> Parser -> Resolver -> Typechecker -> AIR lowering

pub struct Source {
    pub code: String,
    pub path: String,
}

pub trait SourceLoader {
    fn load(&mut self, module_path: &[String]) -> Result<Option<Source>, String>;
}

pub struct ProgramInput<'a> {
    pub main: Source,
    pub prelude: String,
    pub core_modules: Vec<Source>,
    pub source_loader: &'a mut dyn SourceLoader,
}
