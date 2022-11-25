use std::borrow::Borrow;
use std::collections::HashMap;
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

    let mut cl_program_map:HashMap<String, cl::Item> = HashMap::new();
    cl_program_map.insert("Header".to_string(), cl::Item::Include{name: "Header".to_string(), content: "descend.cuh".to_string()});

    fun_defs_to_be_generated
        .iter()
        .for_each(|descend_fun_dev| gen_and_add_fun_def(descend_fun_dev, &compil_unit.fun_defs, idx_checks, &mut cl_program_map));

    //TODO: Iterate over function definitions with template Parameters and duplicate them for each entry in Parameter Values

    let cl_program:Vec<cl::Item>;
    cl_program = cl_program_map.into_iter().map(|entry| entry.1).collect();

    printer::print(&cl_program)
}

fn gen_and_add_fun_def(gl_fun: &desc::FunDef, comp_unit: &[desc::FunDef], idx_checks: bool, item_map: &mut HashMap<String, cl::Item>) {
    let desc::FunDef {
        name,
        generic_params: ty_idents,
        param_decls: params,
        ret_dty: ret_ty,
        exec,
        body_expr,
        ..
    } = gl_fun;

    //TODO: Template Functions Definitionen
    //TODO: main Kernel Function kann auch getemplated sein. Wie rufen wir diese dann aus Host auf?

    let item = cl::Item::FunDef {
        name: name.clone(),
        //templ_params: gen_templ_params(ty_idents),
        templ_params: vec![],
        templ_values: vec![],
        params: vec![],
        ret_ty: cl::Ty::Scalar(cl::ScalarTy::F32),
        body: cl::Stmt::Skip,
        is_gpu_function: false,
    };

    match item { cl::Item::FunDef {ref name, ..}|cl::Item::Include {ref name, ..} => item_map.insert(name.to_string(), item.clone())};
}
