use crate::codegen as cu_codegen;
use crate::ast as desc;
use crate::c_ast as c;
use crate::cpp_ast as cpp;
use crate::c_ast::cpp_to_c_mapper::CppToCMap;

pub fn gen_cl (compil_unit: &desc::CompilUnit, idx_checks: bool) {
    let mut cu_program = cu_codegen::gen_items(compil_unit, idx_checks);
    let mut copy_visitor = CopyVisitor{};
    let mut cl_program = copy_visitor.gen_cl_program(cu_program);

    println!("test");
}


struct CopyVisitor {
}

impl CopyVisitor {
    pub fn gen_cl_program(&mut self, mut cu_program: Vec<cpp::Item>) -> Vec<c::Item> {
        cu_program.iter_mut().filter_map(|f| self.map_item( f)).collect()
    }
}

impl CppToCMap for CopyVisitor {

}