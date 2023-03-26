use std::collections::HashMap;
use crate::cl_codegen::cuda_to_cl_mapper::{cu_to_cl_map, walk_expr, walk_nat, walk_ty};
use crate::{cpp_ast as cpp, map_list};
use crate::ast::{Ident, Nat};
use crate::cl_codegen::{get_lambda_from_exec, KERNEL_COUNTER, map_exec};
use crate::cpp_ast::{Item, Lit, TemplateArg, TemplParam, Ty};

pub struct monomorphize_visitor<'b> {
    pub(crate) template_args: Vec<cpp::TemplateArg>,
    pub(crate) template_fun: &'b cpp::Item,
    pub(crate) values_for_names: HashMap<String, cpp::TemplateArg>,
    // This is a borrowed instance from the CopyVisitor.
    // Both structs share this, so when we push in this struct, we also push in the CopyVisitor
    //Todo: Refac so we don't need this Hack
    pub(crate) c_program: &'b mut Vec<cpp::Item>,
}

impl<'b> monomorphize_visitor<'b> {
    fn find_names_for_template_args(&mut self) {
        if let cpp::Item::FunDef { templ_params, .. } = self.template_fun {
            for i in 0..templ_params.len() {
                if let Some(param) = templ_params.get(i) {
                    let param_name = match param {
                        cpp::TemplParam::Value { param_name, .. } => { param_name.clone() }
                        cpp::TemplParam::TyName { name } => { name.clone() }
                    };
                    if let Some(template_args) = self.template_args.get(i) {
                        self.values_for_names.insert(param_name, template_args.clone());
                    }
                }
            }
        } else {
            panic!("No Monomorph for include!");
        }
    }
}

impl<'b> cu_to_cl_map for monomorphize_visitor<'b> {
    fn map_item(&mut self, item: &cpp::Item) -> Option<cpp::Item> {
        self.find_names_for_template_args();
        match item {
            cpp::Item::FunDef {
                name,
                params,
                templ_params,
                ret_ty,
                body,
                is_dev_fun
            } => {
                if !templ_params.is_empty() {
                    Some(cpp::Item::FunDef {
                        name: mangle_function_name(name.clone(), &self.template_args),
                        templ_params: vec![],
                        params: map_list!(self, map_param_decl, params),
                        ret_ty: self.map_ty(ret_ty),
                        body: self.map_stmt(body),
                        is_dev_fun: is_dev_fun.clone(),
                    })
                } else {
                    panic!("I think you're using this the wrong way");
                }
            }
            _ => {
                panic!("I think you're using this the wrong way");
            }
        }
    }

    fn map_nat(&mut self, nat: &Nat) -> Nat {
        match nat {
            Nat::Ident(ident) => {
                if let Some(template_param) = self.values_for_names.get(&*ident.name) {
                    match template_param {
                        cpp::TemplateArg::Expr(ex) => {
                            if let cpp::Expr::Nat(template_nat) = ex {
                                template_nat.clone()
                            } else {
                                panic!("Trying to map non Nat Template Param to Nat");
                            }
                        }
                        cpp::TemplateArg::Ty(_) => { panic!("Template Param missmatch trying to assing type to nat!") }
                    }
                } else {
                    walk_nat(self, nat)
                }
            }
            Nat::BinOp(op, lhs, rhs) => {
                Nat::BinOp(
                    op.clone(),
                    Box::new(self.map_nat(lhs)),
                    Box::new(self.map_nat(rhs)))
            }
            _ => walk_nat(self, nat)
        }
    }

    fn map_expr(&mut self, expr: &cpp::Expr) -> cpp::Expr {
        match expr {
            cpp::Expr::FunCall { fun, template_args, args } => {
                if let cpp::Expr::Ident(ident) = fun.as_ref() {
                    if ident.contains("exec") {
                        let exec_new = if let Item::FunDef { templ_params, .. } = self.template_fun {
                            let lambda_item = get_lambda_from_exec(args, templ_params);
                            let exec_new = if let Some(monomorphized_lambda) = self.map_item(&lambda_item) {
                                let kernel_name = if let Item::FunDef { name, .. } = &monomorphized_lambda {
                                    name.clone()
                                } else {
                                    panic!("Mapped Include from Exec Lambda");
                                };
                                self.c_program.push(monomorphized_lambda);

                                // The new Exec call should be walked by the monomorphize visitor, so it too is monomorphized
                                walk_expr(self, &map_exec(&fun, &template_args, &args, kernel_name))
                            } else {
                                panic!("Monomorphozation returned nonen")
                            };
                            exec_new
                        } else {
                            panic!("Template Fun is Include");
                        };
                        exec_new
                    } else {
                        walk_expr(self, expr)
                    }
                } else {
                    walk_expr(self, expr)
                }
            }
            _ => walk_expr(self, expr)
        }
    }

    fn map_ty(&mut self, ty: &Ty) -> Ty {
        match ty {
            Ty::Ident(ident) => {
                if let Some(template_arg) = self.values_for_names.get(ident) {
                    match template_arg {
                        TemplateArg::Expr(_) => { panic!("Trying to Monomorphize Expression to Template Type") }
                        TemplateArg::Ty(ty) => {
                            ty.clone()
                        }
                    }
                } else {
                    walk_ty(self, ty)
                }
            }
            _ => walk_ty(self, ty)
        }
    }
}

pub fn mangle_function_name(function_name: String, template_args: &Vec<cpp::TemplateArg>) -> String {
    let mut mangled_name = function_name.clone();
    template_args.iter().map(|f| {
        match f {
            cpp::TemplateArg::Expr(expr) => {
                if let cpp::Expr::Nat(nat) = expr {
                    match nat {
                        Nat::Lit(size) => { size.to_string() }
                        _ => { panic!("Trying to Mangle Nat which is not Lit") }
                    }
                } else {
                    panic!("Trying to mangle Exprssion which is not a nat")
                }
            }
            cpp::TemplateArg::Ty(ty) => {
                match ty {
                    cpp::Ty::Scalar(scalar_ty) => { scalar_ty_to_name(scalar_ty) }
                    cpp::Ty::Atomic(scalar_ty) => { scalar_ty_to_name(scalar_ty) }
                    _ => { panic!("Unmapped Type!") }
                }
            }
        }
    }).for_each(|str| mangled_name.push_str(&*str));

    mangled_name
}

fn scalar_ty_to_name(ty: &cpp::ScalarTy) -> String {
    match ty {
        cpp::ScalarTy::Void => { "void" }
        cpp::ScalarTy::I32 => { "i32" }
        cpp::ScalarTy::U32 => { "u32" }
        cpp::ScalarTy::F32 => { "f32" }
        cpp::ScalarTy::F64 => { "f64" }
        cpp::ScalarTy::Bool => { "bool" }
        cpp::ScalarTy::Gpu => { "gpu" }
        cpp::ScalarTy::SizeT => { "size-t" }
        cpp::ScalarTy::Memory => { "memory" }
        _ => { panic!("Unmapped Type!") }
    }.to_string()
}