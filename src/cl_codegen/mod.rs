use std::borrow::Borrow;
use std::collections::HashMap;

use crate::{codegen as cu_codegen, map_list};
use crate::ast as desc;
use crate::c_ast as c;
use crate::c_ast::cpp_to_c_mapper::{CppToCMap, walk_expr};
use crate::cpp_ast as cpp;
use crate::cpp_ast::{Expr, Item, TemplateArg};

mod printer;
mod monomorphize_visitor;

pub fn gen_cl (compil_unit: &desc::CompilUnit, idx_checks: bool) -> String {
    let mut cu_program = cu_codegen::gen_items(compil_unit, idx_checks);

    let mut copy_visitor = CopyVisitor{ cu_program: &cu_program, c_program: vec![]};

    let mut c_program: Vec<cpp::Item> = cu_program.iter().filter_map(|f| copy_visitor.map_item(f)).collect();
    // Copy Visitor creates monomorphized functions in his own vector
    c_program.append(&mut copy_visitor.c_program);


    let include = cpp::Item::Include ( "descend.hpp".to_string() );
    let mut cl_cpu_program:Vec<cpp::Item> = vec![];
    let mut cl_gpu_program:Vec<cpp::Item> = vec![];

    c_program.into_iter().for_each(|item| {
        match item {
            cpp::Item::Include { .. }  => { },
            cpp::Item::FunDef { is_dev_fun, .. } => {
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

struct CopyVisitor<'a> {
    cu_program: &'a Vec<cpp::Item>,
    c_program: Vec<cpp::Item>
}

impl<'a> CopyVisitor<'a> {
    fn monomorphize(&mut self, fun: &cpp::Expr, template_args: Vec<TemplateArg>, args: Vec<cpp::Expr>) -> cpp::Expr {
        if let cpp::Expr::Ident(fun_identifier) = fun {
            let cpp_fun_def = self.cu_program.iter().find(|f| match f {
                Item::Include(_) => {false}
                Item::FunDef { name, .. } => {name == fun_identifier}
            });

            if let Some(template_fun) = cpp_fun_def {
                let mangled_function_name = monomorphize_visitor::mangle_function_name(fun_identifier.clone(), &template_args);
                let mut monomorphizer = monomorphize_visitor::MonomorphizeVisitor {
                    template_args,
                    template_fun,
                    values_for_names: HashMap::new(),
                    c_program: &mut self.c_program
                };

                if let Some(monomorphized_item) = monomorphizer.map_item(template_fun) {
                    self.c_program.push(monomorphized_item);
                    cpp::Expr::FunCall { fun: Box::new(cpp::Expr::Ident(mangled_function_name)), template_args: vec![], args: map_list!(self, map_expr, args) }
                } else {
                    panic!("This cannot happen");
                }
            } else {
                // This is the case where a Function from the header is called with template Params.
                // Since this cannot happen in der Kernel, we do not monomorphize here
                // Todo: Handle the case of unfound Standart Functions
                let new_fun = cpp::Expr::Ident(fun_identifier.clone());
                let new_args = vec![];
                cpp::Expr::FunCall {
                    fun: Box::new(new_fun),
                    template_args: template_args,
                    args: new_args }
                //panic!("Could not find Function Definition for Call {}", fun_identifier)
            }
        }
        else {
            panic!("This should not happen");
        }
    }
}

impl<'a> CppToCMap for CopyVisitor<'a> {
    fn map_expr(&mut self, expr: &cpp::Expr) -> cpp::Expr {
        match expr {
            cpp::Expr::FunCall { fun, template_args, args } => {
                if !template_args.is_empty() {
                    self.monomorphize(fun, template_args.clone(), args.clone())
                }
                // Cuda Syncthreads are barriers in OpenCL
                else if let Expr::Ident(ident) = fun.as_ref() {
                    if ident == "__syncthreads" {
                        cpp::Expr::FunCall {
                            fun: Box::new(cpp::Expr::Ident("barrier".to_string())),
                            template_args: vec![],
                            args: vec![cpp::Expr::Ident("CLK_LOCAL_MEM_FENCE".to_string())]
                        }
                    } else {
                        walk_expr(self, expr)
                    }
                }
                else {
                    walk_expr(self, expr)
                }
            }
            _ => walk_expr(self, expr)
        }
    }
}