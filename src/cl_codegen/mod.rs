use crate::{ast, cpp_ast as cl};
use crate::ast as desc;
use crate::ast::{utils};

mod printer;

// Precondition. all function definitions are successfully typechecked and
// therefore every subexpression stores a type
pub fn gen(compil_unit: &desc::CompilUnit, idx_checks: bool) -> String {
    let fun_defs_to_be_generated = compil_unit
        .fun_defs
        .iter()
        .filter(|f| {
            f.param_decls.iter().all(|p| {
                !ast::utils::is_shape_ty(p.ty.as_ref().unwrap())
                    && !matches!(
                        &p.ty.as_ref().unwrap().ty,
                        desc::TyKind::Data(desc::DataTy {
                            dty: desc::DataTyKind::ThreadHierchy(_),
                            ..
                        })
                    )
            })
        })
        .cloned()
        .collect::<Vec<desc::FunDef>>();



    let cl_program = std::iter::once(cl::Item::Include("descend.cuh".to_string()))
        .chain(
            fun_defs_to_be_generated
                .iter()
                .map(|fun_def| gen_fun_def(fun_def, &compil_unit.fun_defs, idx_checks)),
        )
        .collect::<cl::Program>();
    printer::print(&cl_program)
}

fn gen_fun_def(gl_fun: &desc::FunDef, comp_unit: &[desc::FunDef], idx_checks: bool) -> cl::Item {


}