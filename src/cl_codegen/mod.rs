use std::borrow::Borrow;
use std::collections::HashMap;
use crate::{ast, cpp_ast as cl};
use crate::ast as desc;
use crate::ast::{utils};
use crate::cpp_ast::Item;
use crate::cu_codegen as cu;
use crate::cu_codegen::{CodegenCtx, gen_ty};
use crate::cpp_codegen as cpp;

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

    // We remove the item, so ownership is transferred from the map to this function (since this function is the owner for the case where no Item exists for the name)
    // We transfer ownership back to the map when we insert the item at the end of this function
    let item = match item_map.remove(name) {
        // Case where we haven't iterated over a function call yet
        None => cl::Item::FunDef {
            name: name.clone(),
            templ_params: cpp::gen_templ_params(ty_idents),
            params: cu::gen_param_decls(params),
            ret_ty: cu::gen_ty(&desc::TyKind::Data(ret_ty.clone()), desc::Mutability::Mut),
            body: cu::gen_stmt(
                body_expr,
                !matches!(
                ret_ty,
                desc::DataTy {
                    dty: desc::DataTyKind::Scalar(desc::ScalarTy::Unit),
                    ..
                }
            ),
                &mut cu::CodegenCtx::new(),
                comp_unit,
                false,
                idx_checks
            ),
            is_gpu_function: cu::is_dev_fun(*exec),
            //Template Values are openCL only (cuda can handle Template Params itself)
            templ_values: vec!()
        },
        // Case where some other function Definition already called this (templated) function in it's body
        Some(existing_item) => existing_item
    };

    // Put our function Definition into the map and transfer ownership to the map
    match item { cl::Item::FunDef {ref name, ..}|cl::Item::Include {ref name, ..} => item_map.insert(name.to_string(), item)};
}
