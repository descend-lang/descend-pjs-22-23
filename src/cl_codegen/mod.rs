use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::Debug;
use crate::{ast, cpp_ast as cl};
use crate::ast as desc;
use crate::ast::{utils};
use crate::cpp_ast::Item;
use crate::cu_codegen as cu;
use crate::cpp_codegen as cpp;
use crate::cpp_codegen::codegenCtx::CodegenCtx;

mod printer;

// Precondition. all function definitions are successfully typechecked and
// therefore every subexpression stores a type
pub fn gen(compil_unit: &desc::CompilUnit, idx_checks: bool) -> String {
    //TODO: Generate Function Declarations
    let initial_fns_to_generate = collect_initial_fns_to_generate(compil_unit);

    let mut codegen_ctx = CodegenCtx::new();
    let mut initial_fns = Vec::with_capacity(initial_fns_to_generate.len() * 2);
    for fun_def in &initial_fns_to_generate {
        codegen_ctx.push_scope();
        initial_fns.push(gen_fun_def(fun_def, &compil_unit.fun_defs, idx_checks, &mut codegen_ctx));
        codegen_ctx.drop_scope();
        //TODO: Why is this here
        //debug_assert_eq!(codegen_ctx.shape_ctx.map.len(), 0);
    }

    let cl_fun_defs = codegen_ctx
        .inst_fn_ctx
        .into_values()
        .chain(
            initial_fns
                .into_iter()
        )
        .collect::<Vec<_>>();

    let include = Item::Include { name: "include".to_string(), content: "descend.hpp".to_string() };
    let cl_program = std::iter::empty()
        .chain(cl_fun_defs)
        .collect::<Vec<_>>();
        
    //TODO: Iterate over function definitions with template Parameters and duplicate them for each entry in Parameter Values
    //TODO: Parameter von GPU Funktionen müssen in beiden Programmen vorkommen
    let mut cl_cpu_program:Vec<cl::Item> = vec![];
    let mut cl_gpu_program:Vec<cl::Item> = vec![];

    cl_program.into_iter().for_each(|item| {
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

    printer::print(&include, &cl_cpu_program, &cl_gpu_program)
}

fn collect_initial_fns_to_generate(comp_unit: &desc::CompilUnit) -> Vec<desc::FunDef> {
    comp_unit
        .fun_defs
        .iter()
        // Filter out the only functions that make sense to be generated without an explicit call.
        //
        // As soon as exec resources are generic, we need to know the specific values
        // that are used in a function call, so that we can inline them (therefore these functions
        // are not inlcuded).
        .filter(|f|
            f.param_decls
                .iter()
                .all(|p| !ast::utils::is_shape_ty(p.ty.as_ref().unwrap())
                    && !matches!(
                        &p.ty.as_ref().unwrap().ty,
                        desc::TyKind::Data(desc::DataTy {
                            dty: desc::DataTyKind::ThreadHierchy(_),
                            ..
                        }))
                    && f.generic_params.is_empty())
        )
        .cloned()
        .collect::<Vec<desc::FunDef>>()
}

// TODO: Push generated template functions to codegenCtx
fn gen_fun_def(gl_fun: &desc::FunDef, comp_unit: &[desc::FunDef], idx_checks: bool, codegen_ctx: &mut CodegenCtx) -> cl::Item{
    let desc::FunDef {
        name,
        generic_params: ty_idents,
        param_decls: params,
        ret_dty: ret_ty,
        exec,
        body_expr,
        ..
    } = gl_fun;

    cl::Item::FunDef {
        name: name.clone(),
        templ_params: cpp::gen_templ_params(ty_idents),
        params: cpp::gen_param_decls(params),
        ret_ty: cpp::gen_ty(&desc::TyKind::Data(ret_ty.clone()), desc::Mutability::Mut),
        body: cu::gen_stmt(
            body_expr,
            !matches!(
                ret_ty,
                desc::DataTy {
                    dty: desc::DataTyKind::Scalar(desc::ScalarTy::Unit),
                    ..
                }
            ),
            &mut CodegenCtx::new(),
            comp_unit,
            false,
            idx_checks,
        ),
        is_gpu_function: is_dev_fun(*exec),
        //Template Values are openCL only (cuda can handle Template Params itself)
        templ_values: vec!()
    }
}

//TODO: implement
fn gen_stmt(
    expr: &desc::Expr,
    return_value: bool,
    codegen_ctx: &mut CodegenCtx,
    comp_unit: &[desc::FunDef],
    dev_fun: bool,
    idx_checks: bool,
) -> cl::Stmt{
    cl::Stmt::Skip
}

fn is_dev_fun(exec: desc::Exec) -> bool {
    match exec {
        desc::Exec::GpuGrid
        | desc::Exec::GpuBlock
        | desc::Exec::GpuWarp
        | desc::Exec::GpuThread => true,
        desc::Exec::CpuThread | desc::Exec::View => false,
    }
}