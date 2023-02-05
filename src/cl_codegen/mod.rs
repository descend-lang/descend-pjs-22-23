mod printer;

use crate::codegen as cu_codegen;
use crate::ast as desc;
use crate::c_ast as c;
use crate::cpp_ast as cpp;
use crate::c_ast::cpp_to_c_mapper::CppToCMap;
use crate::cl_codegen::printer::print;

pub fn gen_cl (compil_unit: &desc::CompilUnit, idx_checks: bool) -> String {
    let mut cu_program = cu_codegen::gen_items(compil_unit, idx_checks);
    let mut copy_visitor = CopyVisitor{};
    let mut cl_program = copy_visitor.gen_cl_program(cu_program);

    let include = c::Item::Include ( "descend.hpp".to_string() );
    let mut cl_cpu_program:Vec<c::Item> = vec![];
    let mut cl_gpu_program:Vec<c::Item> = vec![];

    cl_program.into_iter().for_each(|item| {
        match item {
            c::Item::Include { .. }  => { },
            c::Item::FunDef { is_dev_fun, .. } => {
                if is_dev_fun {
                    cl_gpu_program.push(item);
                } else {
                    cl_cpu_program.push(item);
                }
            }
        }
    });

    printer::print(&include, &*cl_cpu_program, &*cl_gpu_program)
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