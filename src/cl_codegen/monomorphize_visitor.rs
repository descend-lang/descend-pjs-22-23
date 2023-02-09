use std::collections::HashMap;
use crate::c_ast::cpp_to_c_mapper::{CppToCMap, walk_expr, walk_nat};
use crate::{cpp_ast as cpp, map_list};
use crate::c_ast as c;
use crate::ast::{Ident, Nat};
use crate::cl_codegen::CopyVisitor;
use crate::cpp_ast::{Expr, Item};

pub struct MonomorphizeVisitor<'b> {
    pub(crate) template_args: Vec<cpp::TemplateArg>,
    pub(crate) template_fun: &'b cpp::Item,
    pub(crate) values_for_names: HashMap<String, cpp::TemplateArg>,
    // This is a borrowed instance from the CopyVisitor.
    // Both structs share this, so when we push in this struct, we also push in the CopyVisitor
    //Todo: Refac so we don't need this Hack
    pub(crate) c_program: &'b mut Vec<cpp::Item>,
}

impl<'b> MonomorphizeVisitor<'b> {
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

impl<'b> CppToCMap for MonomorphizeVisitor<'b> {
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
                } else if ident.name.contains("threadIdx") {
                    let dimension_index = get_dimension_index_from_x_y_z(&ident.name);

                    Nat::App(Ident {
                        name: "get_local_id".to_string(),
                        span: None,
                        is_implicit: false,
                    }, vec![Nat::Lit(dimension_index)])
                } else if ident.name.contains("blockIdx") {
                    let dimension_index = get_dimension_index_from_x_y_z(&ident.name);

                    Nat::App(Ident {
                        name: "get_group_id".to_string(),
                        span: None,
                        is_implicit: false,
                    }, vec![Nat::Lit(dimension_index)])
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

    fn map_expr(&mut self, expr: &Expr) -> cpp::Expr {
        match expr {
            cpp::Expr::Lambda {
                captures,
                params,
                body,
                ret_ty,
                is_dev_fun
            } => {
                if let Item::FunDef { templ_params, .. } = self.template_fun.clone() {
                    if let Some(kernel_item) = self.map_item(&cpp::Item::FunDef {
                        name: "__kernel".to_string(),
                        templ_params: templ_params.to_vec(),
                        params: params.clone(),
                        ret_ty: ret_ty.clone(),
                        body: *body.clone(),
                        is_dev_fun: is_dev_fun.clone(),
                    }) {
                        let kernel_name: String;
                        if let Item::FunDef { name, .. } = &kernel_item {
                            kernel_name = name.clone();
                        } else {
                            panic!("Generated Include from Lambda (this won't happen)");
                        }
                        self.c_program.push(kernel_item);
                        //TODO Currently no String params supported... Add them
                        cpp::Expr::Ident(kernel_name)
                    } else {
                        panic!("Could not Generate Lambda as Kernel maybe forgot to use monomorphize visitor?");
                    }
                } else {
                    panic!("Template Function of Monomorphize Visitor is include! Maybe take a break?")
                }
            }
            cpp::Expr::FunCall { fun, template_args, args } => {
                if let Expr::Ident(ident) = fun.as_ref() {
                    if ident == "__syncthreads" {
                        cpp::Expr::FunCall {
                            fun: Box::new(cpp::Expr::Ident("barrier".to_string())),
                            template_args: vec![],
                            args: vec![cpp::Expr::Ident("CLK_LOCAL_MEM_FENCE".to_string())],
                        }
                    } else if ident == "descend::gpu_device" {

                        let mut new_args = args.clone();
                        new_args.push(Expr::Ident("kernel".to_string()));

                        cpp::Expr::FunCall {
                            fun: fun.clone(),
                            template_args: vec![],
                            args: new_args
                        }
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

fn get_dimension_index_from_x_y_z(identifier: &String) -> usize {
    let dimension: Vec<char> = identifier.chars().rev().take(1).collect();

    let dimension_index = match dimension.get(0) {
        None => { panic!("Cannot get Dimension from ThreadIdx") }
        Some(dimension_axis) => {
            if 'x' == *dimension_axis { 0 } else if 'y' == *dimension_axis { 1 } else if 'z' == *dimension_axis { 2 } else { panic!("Unknown Dimension Axis {}", dimension_axis) }
        }
    };
    dimension_index
}