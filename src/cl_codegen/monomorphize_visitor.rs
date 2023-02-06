use std::collections::HashMap;
use crate::c_ast::cpp_to_c_mapper::{CppToCMap, walk_expr, walk_nat};
use crate::{cpp_ast as cpp, map_list};
use crate::c_ast as c;
use crate::ast::Nat;
use crate::cl_codegen::CopyVisitor;
use crate::cpp_ast::Expr;

pub struct MonomorphizeVisitor<'b> {
    pub(crate) template_args: Vec<cpp::TemplateArg>,
    pub(crate) template_fun: &'b cpp::Item,
    pub(crate) values_for_names: HashMap<String, cpp::TemplateArg>,
    pub(crate) c_program: &'b mut Vec<c::Item>
}

impl<'b> MonomorphizeVisitor<'b> {
    fn find_names_for_template_args(&mut self) {
        if let cpp::Item::FunDef { templ_params, .. } = self.template_fun {
            for i in 0 .. templ_params.len() {
                if let Some(param) = templ_params.get(i) {
                    let param_name = match param {
                        cpp::TemplParam::Value { param_name, .. } => {param_name.clone()}
                        cpp::TemplParam::TyName { name } => {name.clone()}
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
    fn map_item(&mut self, item: &cpp::Item) -> Option<c::Item> {
        self.find_names_for_template_args();
        match item {
            cpp::Item::FunDef {
                name,
                params,
                templ_params,
                ret_ty,
                body,
                is_dev_fun } => {
                if !templ_params.is_empty() {
                    Some(c::Item::FunDef {
                        name: mangle_function_name(name.clone(), &self.template_args),
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
                            }
                            else {
                                panic!("Trying to map non Nat Template Param to Nat");
                            }
                        }
                        cpp::TemplateArg::Ty(_) => {panic!("Template Param missmatch trying to assing type to nat!")}
                    }
                } else {
                    walk_nat(self,nat)
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

    fn map_expr(&mut self, expr: &Expr) -> c::Expr {
        match expr {
            cpp::Expr::Lambda { captures,
                params,
                body,
                ret_ty,
                is_dev_fun } => {

                let mut copy_visitor = CopyVisitor{ cu_program: &mut vec![], c_program: vec![] };
                if let Some(kernel_item) = copy_visitor.map_item(&cpp::Item::FunDef {
                    name: "kernel".to_string(),
                    templ_params: vec![],
                    params: params.clone(),
                    ret_ty: ret_ty.clone(),
                    body: *body.clone(),
                    is_dev_fun: is_dev_fun.clone(),
                }) {
                    self.c_program.push(kernel_item);
                }


                // Lambdas should only appear for exec. Exec is called with Raw-String Param in OpenCL
                c::Expr::Ident("kernel".to_string())
            },
            _ => walk_expr(self, expr)
        }
    }

}

pub fn mangle_function_name(function_name: String, template_args: &Vec<cpp::TemplateArg> ) -> String {
    let mut mangled_name = function_name.clone();
    template_args.iter().map(|f| {
        match f {
            cpp::TemplateArg::Expr(expr) => {
                if let cpp::Expr::Nat(nat) = expr {
                    match nat {
                        Nat::Lit(size) => {size.to_string()},
                        _ => { panic!("Trying to Mangle Nat which is not Lit") }
                    }
                } else {
                    panic!("Trying to mangle Exprssion which is not a nat")
                }
            }
            cpp::TemplateArg::Ty(ty) => {
                match ty {
                    cpp::Ty::Scalar(scalar_ty) => {scalar_ty_to_name(scalar_ty)}
                    cpp::Ty::Atomic(scalar_ty) => {scalar_ty_to_name(scalar_ty)}
                    _ => {panic!("Unmapped Type!")}
                }
            }
        }
    }).for_each(|str| mangled_name.push_str(&*str));

    mangled_name
}

fn scalar_ty_to_name(ty: &cpp::ScalarTy) -> String{
    match ty {
        cpp::ScalarTy::Void => {"void"}
        cpp::ScalarTy::I32 => {"i32"}
        cpp::ScalarTy::U32 => {"u32"}
        cpp::ScalarTy::F32 => {"f32"}
        cpp::ScalarTy::F64 => {"f64"}
        cpp::ScalarTy::Bool => {"bool"}
        cpp::ScalarTy::Gpu => {"gpu"}
        cpp::ScalarTy::SizeT => {"size-t"}
        cpp::ScalarTy::Memory => {"memory"}
        _ => {panic!("Unmapped Type!")}
    }.to_string()
}