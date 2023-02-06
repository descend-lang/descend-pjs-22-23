mod printer;

use std::collections::HashMap;
use crate::codegen as cu_codegen;
use crate::ast as desc;
use crate::ast::{BinOpNat, Nat};
use crate::c_ast as c;
use crate::cpp_ast as cpp;
use crate::c_ast::cpp_to_c_mapper::{CppToCMap, walk_expr, walk_nat, walk_param_decl, walk_ty};
use crate::c_ast::Stmt::Expr;
use crate::cpp_ast::{BinOp, Item, ParamDecl, TemplateArg, TemplParam, Ty};

macro_rules! map_list {
    ($mapper: expr, $method: ident, $list: expr) => {
        $list.to_vec().into_iter().map(|mut f| $mapper.$method(& f)).collect()
    };
}

pub fn gen_cl (compil_unit: &desc::CompilUnit, idx_checks: bool) -> String {
    let mut cu_program = cu_codegen::gen_items(compil_unit, idx_checks);

    let mut copy_visitor = CopyVisitor{ cu_program: &cu_program, c_program: vec![]};

    let mut c_program: Vec<c::Item> = cu_program.iter().filter_map(|f| copy_visitor.map_item(f)).collect();
    // Copy Visitor creates monomorphized functions in his own vector
    c_program.append(&mut copy_visitor.c_program);


    let include = c::Item::Include ( "descend.hpp".to_string() );
    let mut cl_cpu_program:Vec<c::Item> = vec![];
    let mut cl_gpu_program:Vec<c::Item> = vec![];

    c_program.into_iter().for_each(|item| {
        match item {
            c::Item::Include { .. }  => { },
            c::Item::FunDef { is_dev_fun, .. } => {
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
    c_program: Vec<c::Item>
}

impl<'a> CopyVisitor<'a> {
    fn monomorphize(&mut self, fun: &cpp::Expr, template_args: Vec<TemplateArg>, args: Vec<cpp::Expr>) -> c::Expr {
        if let cpp::Expr::Ident(fun_identifier) = fun {
            let cpp_fun_def = self.cu_program.iter().find(|f| match f {
                Item::Include(_) => {false}
                Item::FunDef { name, .. } => {name == fun_identifier}
            });

            if let Some(template_fun) = cpp_fun_def {
                let mut monomorphizer = MonomorphizeVisitor {
                    template_args,
                    template_fun,
                    values_for_names: HashMap::new(),
                };

                if let Some(monomorphized_item) = monomorphizer.map_item(template_fun) {
                    self.c_program.push(monomorphized_item);
                    c::Expr::FunCall { fun: Box::new(c::Expr::Ident("MonomorphizedName".to_string())), args: vec![] }
                } else {
                    panic!("This cannot happen");
                }
            } else {
                // Todo: Handle the case of unfound Standart Functions
                let new_fun = c::Expr::Ident(fun_identifier.clone());
                let new_args = vec![];
                c::Expr::FunCall { fun: Box::new(new_fun), args: new_args }
                //panic!("Could not find Function Definition for Call {}", fun_identifier)
            }
        }
        else {
            panic!("This should not happen");
        }
    }
}

impl<'a> CppToCMap for CopyVisitor<'a> {
    fn map_expr(&mut self, expr: &cpp::Expr) -> c::Expr {
        match expr {
            cpp::Expr::FunCall { fun, template_args, args } => {
                if !template_args.is_empty() {
                    self.monomorphize(fun, template_args.clone(), args.clone())
                } else {
                    walk_expr(self, expr)
                }
            },
            _ => walk_expr(self, expr)
        }
    }
}

struct MonomorphizeVisitor<'b> {
    template_args: Vec<TemplateArg>,
    template_fun: &'b Item,
    values_for_names: HashMap<String, TemplateArg>
}

impl<'b> MonomorphizeVisitor<'b> {
    fn find_names_for_template_args(&mut self) {
        if let Item::FunDef { templ_params, .. } = self.template_fun {
            for i in 0 .. templ_params.len() {
                if let Some(param) = templ_params.get(i) {
                    let param_name = match param {
                        TemplParam::Value { param_name, ty } => {param_name.clone()}
                        TemplParam::TyName { name } => {name.clone()}
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
    fn map_item(&mut self, item: &Item) -> Option<c::Item> {
        self.find_names_for_template_args();
        match item {
            cpp::Item::FunDef {
                name,
                params,
                templ_params,
                ret_ty,
                body,
                is_dev_fun } => {
                // Templated Functions are mapped only when their apply is mapped. Therefor we filter them here
                if (!templ_params.is_empty()) {
                    Some(c::Item::FunDef {
                        name: "Todo:MangleName!".to_string(),
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
                        TemplateArg::Expr(ex) => {
                            if let cpp::Expr::Nat(template_nat) = ex {
                                template_nat.clone()
                            }
                            else {
                                panic!("Trying to map non Nat Template Param to Nat");
                            }
                        }
                        TemplateArg::Ty(_) => {panic!("Template Param missmatch trying to assing type to nat!")}
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

}