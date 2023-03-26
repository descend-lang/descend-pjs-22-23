use std::collections::HashMap;

use crate::{codegen as cu_codegen, map_list};
use crate::ast as desc;
use crate::cl_codegen::cuda_to_cl_mapper::{cu_to_cl_map, walk_expr};
use crate::cpp_ast as cpp;
use crate::cpp_ast::{Expr, Item, TemplateArg};

mod printer;
mod monomorphize_visitor;
pub(crate) mod cuda_to_cl_mapper;

static mut KERNEL_COUNTER: i32 = 0;

pub fn gen_cl(compil_unit: &desc::CompilUnit, idx_checks: bool) -> String {
    let mut cu_program = cu_codegen::gen_items(compil_unit, idx_checks);

    let mut copy_visitor = CopyVisitor { cu_program: &cu_program, c_program: vec![] };

    let mut c_program: Vec<cpp::Item> = cu_program.iter().filter_map(|f| copy_visitor.map_item(f)).collect();
    // Copy Visitor creates monomorphized functions in his own vector
    c_program.append(&mut copy_visitor.c_program);


    let include = cpp::Item::Include("descend.hpp".to_string());
    let mut cl_cpu_program: Vec<cpp::Item> = vec![];
    let mut cl_gpu_program: Vec<cpp::Item> = vec![];

    c_program.into_iter().for_each(|item| {
        match item {
            cpp::Item::Include { .. } => {}
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
    c_program: Vec<cpp::Item>,
}

impl<'a> CopyVisitor<'a> {
    fn monomorphize(&mut self, fun: &cpp::Expr, template_args: Vec<TemplateArg>, args: Vec<cpp::Expr>) -> cpp::Expr {
        if let cpp::Expr::Ident(fun_identifier) = fun {
            let cpp_fun_def = self.cu_program.iter().find(|f| match f {
                Item::Include(_) => { false }
                Item::FunDef { name, .. } => { name == fun_identifier }
            });

            if let Some(template_fun) = cpp_fun_def {
                let mangled_function_name = monomorphize_visitor::mangle_function_name(fun_identifier.clone(), &template_args);
                let mut monomorphizer = monomorphize_visitor::monomorphize_visitor {
                    template_args,
                    template_fun,
                    values_for_names: HashMap::new(),
                    c_program: &mut self.c_program,
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
                let new_fun = cpp::Expr::Ident(fun_identifier.clone());
                cpp::Expr::FunCall {
                    fun: Box::new(new_fun),
                    template_args,
                    args,
                }
            }
        } else {
            panic!("This should not happen");
        }
    }
}

impl<'a> cu_to_cl_map for CopyVisitor<'a> {
    fn map_expr(&mut self, expr: &cpp::Expr) -> cpp::Expr {
        match expr {
            cpp::Expr::FunCall { fun, template_args, args } => {
                match fun.as_ref() {
                    Expr::Ident(ident) => {
                        if ident.contains("exec") {
                            let lambda_item = get_lambda_from_exec(args, &vec![]);

                            let lambda_item = self.map_item(&lambda_item).unwrap();

                            let kernel_name = if let Item::FunDef { name, .. } = &lambda_item {
                                name.clone()
                            } else {
                                panic!("Mapped Include from Exec Lambda");
                            };
                            self.c_program.push(lambda_item);

                            map_exec(&fun, &template_args, &args, kernel_name)
                        } else if !template_args.is_empty() {
                            self.monomorphize(fun, template_args.clone(), args.clone())
                        } else {
                            walk_expr(self, expr)
                        }
                    }
                    _ => {
                        if !template_args.is_empty() {
                            self.monomorphize(fun, template_args.clone(), args.clone())
                        } else {
                            walk_expr(self, expr)
                        }
                    }
                }
            }
            _ => walk_expr(self, expr)
        }
    }
}

fn map_exec(fun_ident: &cpp::Expr, template_args: &Vec<TemplateArg>, args: &Vec<cpp::Expr>, kernel_name: String) -> cpp::Expr {
    let kernel_args = &mut args.clone().into_iter().skip(2).collect();
    let kernel_name_expr = cpp::Expr::Lit(cpp::Lit::String(kernel_name));
    let kernel_raw_string_expr = cpp::Expr::Ident("kernel".to_string());

    let mut args_new = vec![args[0].clone(), kernel_name_expr, kernel_raw_string_expr];
    args_new.append(kernel_args);

    cpp::Expr::FunCall {
        fun: Box::new(fun_ident.clone()),
        template_args: template_args.clone(),
        args: args_new,
    }
}

fn get_lambda_from_exec(args: &Vec<cpp::Expr>, template_params: &Vec<cpp::TemplParam>) -> cpp::Item {
    if let Some(item) = args.iter().filter_map(|f| {
        match f {
            cpp::Expr::Lambda { captures, params, body, ret_ty, is_dev_fun } => {
                let mut kernel_name = "please change me".to_string();
                // Cannot be called in multithreaded context
                unsafe {
                    kernel_name = format!("__kernel_{KERNEL_COUNTER}_");
                    KERNEL_COUNTER += 1;
                }

                let fun_def = Item::FunDef {
                    name: kernel_name,
                    templ_params: template_params.to_vec(),
                    params: params.to_vec(),
                    ret_ty: ret_ty.clone(),
                    body: *body.clone(),
                    is_dev_fun: *is_dev_fun,
                };
                Some(fun_def)
            }
            _ => { None }
        }
    }).last() {
        item
    } else {
        panic!("No Lambda found in Exec");
    }
}
