use crate::error::ErrorReported;

#[macro_use]
mod ast;
#[macro_use]
mod codegen;
pub mod error;
mod parser;
mod ty_check;
mod utils;

pub fn compile(file_path: &str) -> Result<String, ErrorReported> {
    let source = parser::SourceCode::from_file(file_path)?;
    let mut compil_unit = parser::parse(&source)?;
    ty_check::ty_check(&mut compil_unit)?;
    Ok(codegen::gen(&compil_unit, false))
}
