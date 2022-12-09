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
    let include_header:Item = cl::Item::Include{name: "Header".to_string(), content: "descend.hpp".to_string()};
    // cl_program_map.insert("Header".to_string(), cl::Item::Include{name: "Header".to_string(), content: "descend.hpp".to_string()});

    fun_defs_to_be_generated
        .iter()
        .for_each(|descend_fun_dev| gen_and_add_fun_def(descend_fun_dev, &compil_unit.fun_defs, idx_checks, &mut cl_program_map));

    //TODO: Iterate over function definitions with template Parameters and duplicate them for each entry in Parameter Values

    let mut cl_cpu_program:Vec<cl::Item> = vec![];
    let mut cl_gpu_program:Vec<cl::Item> = vec![];

    cl_program_map.into_iter().map(|entry| entry.1).for_each(|item| {
        match item {
            Item::Include { .. }  => cl_cpu_program.push(item),
            Item::FunDef { is_gpu_function, .. } => {
                if is_gpu_function {
                   cl_gpu_program.push(item);
                } else {
                    cl_cpu_program.push(item);
                }
            }
        }
    });

    printer::print(&include_header, &cl_cpu_program, &cl_gpu_program)
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
        None => cl::Item::new_fun_def(name.clone()),
        // Case where some other function Definition already called this (templated) function in it's body
        Some(existing_item) => existing_item
    };

    // Put our function Definition into the map and transfer ownership to the map
    match item { cl::Item::FunDef {ref name, ..}|cl::Item::Include {ref name, ..} => item_map.insert(name.to_string(), item)};
}
