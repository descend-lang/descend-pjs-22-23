use crate::cpp_ast as cpp;
use crate::cpp_ast::{TemplateArg, TemplParam};


// Taken from the Rust compiler
macro_rules! walk_list {
    ($mapper: expr, $method: ident, $list: expr) => {
        for elem in $list {
            $mapper.$method(elem)
        }
    };
}

#[macro_export] macro_rules! map_list {
    ($mapper: expr, $method: ident, $list: expr) => {
        $list.to_vec().into_iter().map(|mut f| $mapper.$method(& f)).collect()
    };
}

macro_rules! map_opt {
    ($mapper: expr, $method: ident, $optional: expr) => {
        if let Some(opt_val) = $optional {
            Some($mapper.$method(opt_val))
        } else {
            None
        }
    }
}

pub trait CuToClMap: Sized {
    fn map_item(&mut self, item: &cpp::Item) -> Option<cpp::Item> {
        walk_item(self, item)
    }
    fn map_param_decl(&mut self, param_decl: &cpp::ParamDecl) -> cpp::ParamDecl {
        walk_param_decl(self, param_decl)
    }
    fn map_ty(&mut self, ty: &cpp::Ty) -> cpp::Ty {
        walk_ty(self, ty)
    }
    fn map_stmt(&mut self, stmt: &cpp::Stmt) -> cpp::Stmt {
        walk_stmt(self, stmt)
    }
    fn map_scalar_ty(&mut self, scalar_ty: &cpp::ScalarTy) -> cpp::ScalarTy {
        walk_scalar_ty(self, scalar_ty)
    }
    fn map_nat(&mut self, nat: &crate::ast::Nat) -> crate::ast::Nat { walk_nat(self, nat) }
    fn map_buffer_kind(&mut self, buffer_kind: &cpp::BufferKind) -> cpp::BufferKind {
        walk_buffer_kind(self, buffer_kind)
    }
    fn map_gpu_addr_space(&mut self, addr_space: &cpp::GpuAddrSpace) -> cpp::GpuAddrSpace {
        walk_gpu_addr_space(self, addr_space)
    }
    fn map_expr(&mut self, expr: &cpp::Expr) -> cpp::Expr {
        walk_expr(self, expr)
    }
    fn map_ident(&mut self, ident: &crate::ast::Ident) -> crate::ast::Ident {
        walk_ident(self, ident)
    }
    fn map_bin_op_nat(&mut self, bin_op_nat: &crate::ast::BinOpNat) -> crate::ast::BinOpNat {
        walk_bin_op_nat(self, bin_op_nat)
    }
    fn map_lit(&mut self, lit: &cpp::Lit) -> cpp::Lit {
        walk_lit(self, lit)
    }
    fn map_un_op(&mut self, un_op: &cpp::UnOp) -> cpp::UnOp {
        walk_un_op(self, un_op)
    }
    fn map_bin_op(&mut self, bin_op: &cpp::BinOp) -> cpp::BinOp {
        walk_bin_op(self, bin_op)
    }
    fn map_template_arg(&mut self, template_arg: &cpp::TemplateArg) -> cpp::TemplateArg {
        walk_template_arg(self, template_arg)
    }
    fn map_templ_param(&mut self, templ_param: &cpp::TemplParam) -> cpp::TemplParam {
        walk_templ_param(self, templ_param)
    }
}

pub fn walk_item<V: CuToClMap>(mapper: &mut V, item: &cpp::Item) -> Option<cpp::Item> {
    match item {
        cpp::Item::Include(name) => { Some(cpp::Item::Include(name.clone())) }
        cpp::Item::FunDef { name, params, templ_params, ret_ty, body, is_dev_fun } => {
            // Templated Functions are mapped only when their apply is mapped. Therefor we filter them here
            if (templ_params.is_empty()) {
                Some(cpp::Item::FunDef {
                    name: name.clone(),
                    templ_params: map_list!(mapper, map_templ_param, templ_params),
                    params: map_list!(mapper, map_param_decl, params),
                    ret_ty: mapper.map_ty(ret_ty),
                    body: mapper.map_stmt(body),
                    is_dev_fun: is_dev_fun.clone(),
                })
            } else {
                None
            }
        }
    }
}

pub fn walk_param_decl<V: CuToClMap>(mapper: &mut V, param_decl: &cpp::ParamDecl) -> cpp::ParamDecl {
    cpp::ParamDecl { name: param_decl.name.clone(), ty: mapper.map_ty(&param_decl.ty) }
}

pub fn walk_ty<V: CuToClMap>(mapper: &mut V, ty: &cpp::Ty) -> cpp::Ty {
    match ty {
        cpp::Ty::Scalar(scalar_ty) => {
            cpp::Ty::Scalar(mapper.map_scalar_ty(scalar_ty))
        }
        cpp::Ty::Atomic(scalar_ty) => { cpp::Ty::Atomic(mapper.map_scalar_ty(scalar_ty)) }
        cpp::Ty::Tuple(ty_vec) => {
            cpp::Ty::Tuple(map_list!(mapper, map_ty, ty_vec))
        }
        cpp::Ty::Array(ty, nat) => {
            cpp::Ty::Array(Box::new(mapper.map_ty(ty)), mapper.map_nat(nat))
        }
        cpp::Ty::CArray(ty, nat) => {
            cpp::Ty::CArray(Box::new(mapper.map_ty(ty)), mapper.map_nat(nat))
        }
        cpp::Ty::Buffer(ty, buffer_kind) => {
            cpp::Ty::Buffer(Box::new(mapper.map_ty(ty)), mapper.map_buffer_kind(buffer_kind))
        }
        cpp::Ty::Ptr(ty, addr_space_opt) => {
            let gpu_addr_space_c_opt = if let Some(addr_space) = addr_space_opt {
                Some(mapper.map_gpu_addr_space(addr_space))
            } else {
                None
            };
            cpp::Ty::Ptr(Box::new(mapper.map_ty(ty)), gpu_addr_space_c_opt)
        }
        cpp::Ty::PtrConst(ty, addr_space_opt) => {
            let gpu_addr_space_c_opt = if let Some(addr_space) = addr_space_opt {
                Some(mapper.map_gpu_addr_space(addr_space))
            } else {
                None
            };
            cpp::Ty::PtrConst(Box::new(mapper.map_ty(ty)), gpu_addr_space_c_opt)
        }
        cpp::Ty::Const(ty) => { cpp::Ty::Const(Box::new(mapper.map_ty(ty))) }
        cpp::Ty::Ident(name) => { panic!("Should not encounter TemplateParameter Identifiers in non Templated Functions") }
    }
}

pub fn walk_stmt<V: CuToClMap>(mapper: &mut V, stmt: &cpp::Stmt) -> cpp::Stmt {
    match stmt {
        cpp::Stmt::Skip => { cpp::Stmt::Skip }
        cpp::Stmt::VarDecl { name, ty, addr_space, expr, } => {
            cpp::Stmt::VarDecl {
                name: name.clone(),
                ty: mapper.map_ty(ty),
                addr_space: map_opt!(mapper, map_gpu_addr_space, addr_space),
                expr: map_opt!(mapper, map_expr, expr),
            }
        }
        cpp::Stmt::Block(stmt) => {
            cpp::Stmt::Block(Box::new(mapper.map_stmt(stmt)))
        }
        cpp::Stmt::Seq(stmt_list) => {
            cpp::Stmt::Seq(map_list!(mapper, map_stmt, stmt_list))
        }
        cpp::Stmt::Expr(expr) => {
            cpp::Stmt::Expr(mapper.map_expr(expr))
        }
        cpp::Stmt::If { cond, body } => {
            cpp::Stmt::If { cond: mapper.map_expr(cond), body: Box::new(mapper.map_stmt(body)) }
        }
        cpp::Stmt::IfElse { cond, true_body, false_body } => {
            cpp::Stmt::IfElse {
                cond: mapper.map_expr(cond),
                true_body: Box::new(mapper.map_stmt(true_body)),
                false_body: Box::new(mapper.map_stmt(false_body)),
            }
        }
        cpp::Stmt::While { cond, stmt } => {
            cpp::Stmt::While { cond: mapper.map_expr(cond), stmt: Box::new(mapper.map_stmt(stmt)) }
        }
        cpp::Stmt::ForLoop { init, cond, iter, stmt } => {
            cpp::Stmt::ForLoop {
                init: Box::new(mapper.map_stmt(init)),
                cond: mapper.map_expr(cond),
                iter: mapper.map_expr(iter),
                stmt: Box::new(mapper.map_stmt(stmt)),
            }
        }
        cpp::Stmt::Return(expr) => {
            cpp::Stmt::Return(map_opt!(mapper, map_expr, expr))
        }
        cpp::Stmt::Label(name) => { cpp::Stmt::Label(name.clone()) }
    }
}

pub fn walk_scalar_ty<V: CuToClMap>(mapper: &mut V, scalar_ty: &cpp::ScalarTy) -> cpp::ScalarTy {
    match scalar_ty {
        cpp::ScalarTy::Auto => { panic!("Cannot Map Auto Types") }
        cpp::ScalarTy::Void => { cpp::ScalarTy::Void }
        cpp::ScalarTy::I32 => { cpp::ScalarTy::I32 }
        cpp::ScalarTy::U32 => { cpp::ScalarTy::U32 }
        cpp::ScalarTy::F32 => { cpp::ScalarTy::F32 }
        cpp::ScalarTy::F64 => { cpp::ScalarTy::F64 }
        cpp::ScalarTy::Bool => { cpp::ScalarTy::Bool }
        cpp::ScalarTy::SizeT => { cpp::ScalarTy::SizeT }
        cpp::ScalarTy::Memory => { cpp::ScalarTy::Memory }
        cpp::ScalarTy::Gpu => { cpp::ScalarTy::Gpu }
    }
}

pub fn walk_nat<V: CuToClMap>(mapper: &mut V, nat: &crate::ast::Nat) -> crate::ast::Nat {
    nat.clone()
}

pub fn walk_buffer_kind<V: CuToClMap>(mapper: &mut V, buffer_kind: &cpp::BufferKind) -> cpp::BufferKind {
    match buffer_kind {
        cpp::BufferKind::CpuMem => { cpp::BufferKind::CpuMem }
        cpp::BufferKind::GpuGlobal => { cpp::BufferKind::GpuGlobal }
        cpp::BufferKind::Ident(ident) => { cpp::BufferKind::Ident(ident.clone()) }
    }
}

pub fn walk_gpu_addr_space<V: CuToClMap>(mapper: &mut V, gpu_addr_space: &cpp::GpuAddrSpace) -> cpp::GpuAddrSpace {
    match gpu_addr_space {
        cpp::GpuAddrSpace::Global => { cpp::GpuAddrSpace::Global }
        //GpuAddrSpace::Local => {}
        cpp::GpuAddrSpace::Constant => { cpp::GpuAddrSpace::Constant }
        cpp::GpuAddrSpace::Shared => { cpp::GpuAddrSpace::Shared }
    }
}

pub fn walk_expr<V: CuToClMap>(mapper: &mut V, expr: &cpp::Expr) -> cpp::Expr {
    match expr {
        cpp::Expr::Empty => { cpp::Expr::Empty }
        cpp::Expr::Ident(ident) => { cpp::Expr::Ident(ident.clone()) }
        cpp::Expr::Lit(lit) => {
            cpp::Expr::Lit(mapper.map_lit(lit))
        }
        cpp::Expr::Assign { lhs, rhs } => {
            cpp::Expr::Assign {
                lhs: Box::new(mapper.map_expr(lhs)),
                rhs: Box::new(mapper.map_expr(rhs)),
            }
        }
        cpp::Expr::FunCall { fun, args, template_args } => {
            match fun.as_ref() {
                cpp::Expr::Ident(ident) => {
                    if ident == "__syncthreads" {
                        cpp::Expr::FunCall {
                            fun: Box::new(cpp::Expr::Ident("barrier".to_string())),
                            template_args: vec![],
                            args: vec![cpp::Expr::Ident("CLK_LOCAL_MEM_FENCE".to_string())],
                        }
                    } else {
                        cpp::Expr::FunCall {
                            fun: Box::new(mapper.map_expr(fun)),
                            template_args: map_list!(mapper, map_template_arg, template_args),
                            args: map_list!(mapper, map_expr, args),
                        }
                    }
                }
                _ => {
                    cpp::Expr::FunCall {
                        fun: Box::new(mapper.map_expr(fun)),
                        template_args: map_list!(mapper, map_template_arg, template_args),
                        args: map_list!(mapper, map_expr, args),
                    }
                }
            }
        }
        cpp::Expr::UnOp { op, arg } => {
            cpp::Expr::UnOp {
                op: mapper.map_un_op(op),
                arg: Box::new(mapper.map_expr(expr)),
            }
        }
        cpp::Expr::BinOp { op, lhs, rhs } => {
            cpp::Expr::BinOp {
                op: mapper.map_bin_op(op),
                lhs: Box::new(mapper.map_expr(lhs)),
                rhs: Box::new(mapper.map_expr(rhs)),
            }
        }
        cpp::Expr::ArraySubscript { array, index } => {
            cpp::Expr::ArraySubscript {
                array: Box::new(mapper.map_expr(array)),
                index: mapper.map_nat(index),
            }
        }
        cpp::Expr::Proj { tuple, n } => {
            cpp::Expr::Proj {
                tuple: Box::new(mapper.map_expr(tuple)),
                n: n.clone(),
            }
        }
        cpp::Expr::InitializerList { elems } => {
            cpp::Expr::InitializerList { elems: map_list!(mapper, map_expr, elems) }
        }
        cpp::Expr::Ref(expr) => {
            cpp::Expr::Ref(Box::new(mapper.map_expr(expr)))
        }
        cpp::Expr::Deref(expr) => {
            cpp::Expr::Deref(Box::new(mapper.map_expr(expr)))
        }
        cpp::Expr::Tuple(expr_list) => {
            cpp::Expr::Tuple(map_list!(mapper, map_expr, expr_list))
        }
        cpp::Expr::Nat(nat) => {
            cpp::Expr::Nat(mapper.map_nat(nat))
        }
        cpp::Expr::Lambda { captures, params, body, ret_ty, is_dev_fun } => {
            panic!("Lambdas are not supported in C!");
        }
    }
}

pub fn walk_ident<V: CuToClMap>(mapper: &mut V, ident: &crate::ast::Ident) -> crate::ast::Ident {
    ident.clone()
}

pub fn walk_bin_op_nat<V: CuToClMap>(mapper: &mut V, bin_op_nat: &crate::ast::BinOpNat) -> crate::ast::BinOpNat {
    bin_op_nat.clone()
}

pub fn walk_lit<V: CuToClMap>(mapper: &mut V, lit: &cpp::Lit) -> cpp::Lit {
    match lit {
        cpp::Lit::Bool(b) => { cpp::Lit::Bool(b.clone()) }
        cpp::Lit::I32(i) => { cpp::Lit::I32(i.clone()) }
        cpp::Lit::U32(u) => { cpp::Lit::U32(u.clone()) }
        cpp::Lit::F32(f) => { cpp::Lit::F32(f.clone()) }
        cpp::Lit::F64(f) => { cpp::Lit::F64(f.clone()) }
    }
}

pub fn walk_un_op<V: CuToClMap>(mapper: &mut V, un_op: &cpp::UnOp) -> cpp::UnOp {
    match un_op {
        cpp::UnOp::Not => { cpp::UnOp::Not }
        cpp::UnOp::Neg => { cpp::UnOp::Neg }
    }
}

pub fn walk_bin_op<V: CuToClMap>(mapper: &mut V, bin_op: &cpp::BinOp) -> cpp::BinOp {
    match bin_op {
        cpp::BinOp::Add => { cpp::BinOp::Add }
        cpp::BinOp::Sub => { cpp::BinOp::Sub }
        cpp::BinOp::Mul => { cpp::BinOp::Mul }
        cpp::BinOp::Div => { cpp::BinOp::Div }
        cpp::BinOp::Mod => { cpp::BinOp::Mod }
        cpp::BinOp::And => { cpp::BinOp::And }
        cpp::BinOp::Or => { cpp::BinOp::Or }
        cpp::BinOp::Eq => { cpp::BinOp::Eq }
        cpp::BinOp::Lt => { cpp::BinOp::Lt }
        cpp::BinOp::Le => { cpp::BinOp::Le }
        cpp::BinOp::Gt => { cpp::BinOp::Gt }
        cpp::BinOp::Ge => { cpp::BinOp::Ge }
        cpp::BinOp::Neq => { cpp::BinOp::Neq }
    }
}

pub fn walk_template_arg<V: CuToClMap>(mapper: &mut V, template_arg: &cpp::TemplateArg) -> cpp::TemplateArg {
    match template_arg {
        cpp::TemplateArg::Expr(expr) => {
            cpp::TemplateArg::Expr(mapper.map_expr(expr))
        }
        TemplateArg::Ty(ty) => {
            cpp::TemplateArg::Ty(mapper.map_ty(ty))
        }
    }
}

pub fn walk_templ_param<V: CuToClMap>(mapper: &mut V, templ_param: &cpp::TemplParam) -> cpp::TemplParam {
    match templ_param {
        TemplParam::Value { param_name, ty } => {
            TemplParam::Value {
                param_name: param_name.clone(),
                ty: mapper.map_ty(ty),
            }
        }
        TemplParam::TyName { name } => {
            TemplParam::TyName { name: name.clone() }
        }
    }
}