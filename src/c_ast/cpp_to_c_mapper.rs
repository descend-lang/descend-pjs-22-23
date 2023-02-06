use crate::c_ast as c;
use crate::cpp_ast as cpp;


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

pub trait CppToCMap: Sized {
    fn map_item(&mut self, item: & cpp::Item) -> Option<c::Item> {
        walk_item(self, item)
    }
    fn map_param_decl(&mut self, param_decl: & cpp::ParamDecl) -> c::ParamDecl {
        walk_param_decl(self, param_decl)
    }
    fn map_ty(&mut self, ty: & cpp::Ty) -> c::Ty {
        walk_ty(self, ty)
    }
    fn map_stmt(&mut self, stmt: & cpp::Stmt) -> c::Stmt {
        walk_stmt(self, stmt)
    }
    fn map_scalar_ty(&mut self, scalar_ty: & cpp::ScalarTy) -> c::ScalarTy {
        walk_scalar_ty(self, scalar_ty)
    }
    fn map_nat(&mut self, nat: & crate::ast::Nat) -> crate::ast::Nat { walk_nat(self, nat) }
    fn map_buffer_kind(&mut self, buffer_kind: & cpp::BufferKind) -> c::BufferKind {
        walk_buffer_kind(self, buffer_kind)
    }
    fn map_gpu_addr_space(&mut self, addr_space: & cpp::GpuAddrSpace) -> c::GpuAddrSpace {
        walk_gpu_addr_space(self, addr_space)
    }
    fn map_expr(&mut self, expr: & cpp::Expr) -> c::Expr {
        walk_expr(self, expr)
    }
    fn map_ident(&mut self, ident: & crate::ast::Ident) -> crate::ast::Ident {
        walk_ident(self, ident)
    }
    fn map_bin_op_nat(&mut self, bin_op_nat: & crate::ast::BinOpNat) -> crate::ast::BinOpNat {
        walk_bin_op_nat(self, bin_op_nat)
    }
    fn map_lit(&mut self, lit: & cpp::Lit) -> c::Lit {
        walk_lit(self, lit)
    }
    fn map_un_op(&mut self, un_op: & cpp::UnOp) -> c::UnOp {
        walk_un_op(self, un_op)
    }
    fn map_bin_op(&mut self, bin_op: & cpp::BinOp) -> c::BinOp {
        walk_bin_op(self, bin_op)
    }
}

pub fn walk_item<V: CppToCMap>(mapper: &mut V, item: & cpp::Item) -> Option<c::Item> {
    match item {
        cpp::Item::Include(name) => { Some(c::Item::Include(name.clone())) }
        cpp::Item::FunDef { name, params, templ_params, ret_ty, body, is_dev_fun } => {
            // Templated Functions are mapped only when their apply is mapped. Therefor we filter them here
            if (templ_params.is_empty()) {
                Some(c::Item::FunDef {
                    name: name.clone(),
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

pub fn walk_param_decl<V: CppToCMap>(mapper: &mut V, param_decl: & cpp::ParamDecl) -> c::ParamDecl {
    c::ParamDecl { name: param_decl.name.clone(), ty: mapper.map_ty(& param_decl.ty) }
}

pub fn walk_ty<V: CppToCMap>(mapper: &mut V, ty: & cpp::Ty) -> c::Ty {
    match ty {
        cpp::Ty::Scalar(scalar_ty) => {
            c::Ty::Scalar(mapper.map_scalar_ty(scalar_ty))
        }
        cpp::Ty::Atomic(scalar_ty) => { c::Ty::Atomic(mapper.map_scalar_ty(scalar_ty)) }
        cpp::Ty::Tuple(ty_vec) => {
            c::Ty::Tuple(map_list!(mapper, map_ty, ty_vec))
        }
        cpp::Ty::Array(ty, nat) => {
            c::Ty::Array(Box::new(mapper.map_ty(ty)), mapper.map_nat(nat))
        }
        cpp::Ty::CArray(ty, nat) => {
            c::Ty::CArray(Box::new(mapper.map_ty(ty)), mapper.map_nat(nat))
        }
        cpp::Ty::Buffer(ty, buffer_kind) => {
            c::Ty::Buffer(Box::new(mapper.map_ty(ty)), mapper.map_buffer_kind(buffer_kind))
        }
        cpp::Ty::Ptr(ty, addr_space_opt) => {
            let gpu_addr_space_c_opt = if let Some(addr_space) = addr_space_opt {
                Some(mapper.map_gpu_addr_space(addr_space))
            } else {
                None
            };
            c::Ty::Ptr(Box::new(mapper.map_ty(ty)), gpu_addr_space_c_opt)
        }
        cpp::Ty::PtrConst(ty, addr_space_opt) => {
            let gpu_addr_space_c_opt = if let Some(addr_space) = addr_space_opt {
                Some(mapper.map_gpu_addr_space(addr_space))
            } else {
                None
            };
            c::Ty::PtrConst(Box::new(mapper.map_ty(ty)), gpu_addr_space_c_opt)
        }
        cpp::Ty::Const(ty) => { c::Ty::Const(Box::new(mapper.map_ty(ty))) }
        cpp::Ty::Ident(name) => { panic!("Should not encounter TemplateParameter Identifiers in non Templated Functions") }
    }
}

pub fn walk_stmt<V: CppToCMap>(mapper: &mut V, stmt: & cpp::Stmt) -> c::Stmt {
    match stmt {
        cpp::Stmt::Skip => { c::Stmt::Skip }
        cpp::Stmt::VarDecl { name, ty, addr_space, expr, } => {
            c::Stmt::VarDecl {
                name: name.clone(),
                ty: mapper.map_ty(ty),
                addr_space: map_opt!(mapper, map_gpu_addr_space, addr_space),
                expr: map_opt!(mapper, map_expr, expr),
            }
        }
        cpp::Stmt::Block(stmt) => {
            c::Stmt::Block(Box::new(mapper.map_stmt(stmt)))
        }
        cpp::Stmt::Seq(stmt_list) => {
            c::Stmt::Seq(map_list!(mapper, map_stmt, stmt_list))
        }
        cpp::Stmt::Expr(expr) => {
            c::Stmt::Expr(mapper.map_expr(expr))
        }
        cpp::Stmt::If { cond, body } => {
            c::Stmt::If { cond: mapper.map_expr(cond), body: Box::new(mapper.map_stmt(body)) }
        }
        cpp::Stmt::IfElse { cond, true_body, false_body } => {
            c::Stmt::IfElse {
                cond: mapper.map_expr(cond),
                true_body: Box::new(mapper.map_stmt(true_body)),
                false_body: Box::new(mapper.map_stmt(false_body)),
            }
        }
        cpp::Stmt::While { cond, stmt } => {
            c::Stmt::While { cond: mapper.map_expr(cond), stmt: Box::new(mapper.map_stmt(stmt)) }
        }
        cpp::Stmt::ForLoop { init, cond, iter, stmt } => {
            c::Stmt::ForLoop {
                init: Box::new(mapper.map_stmt(init)),
                cond: mapper.map_expr(cond),
                iter: mapper.map_expr(iter),
                stmt: Box::new(mapper.map_stmt(stmt)),
            }
        }
        cpp::Stmt::Return(expr) => {
            c::Stmt::Return(map_opt!(mapper, map_expr, expr))
        }
        cpp::Stmt::Label(name) => { c::Stmt::Label(name.clone()) }
    }
}

pub fn walk_scalar_ty<V: CppToCMap>(mapper: &mut V, scalar_ty: & cpp::ScalarTy) -> c::ScalarTy {
    match scalar_ty {
        cpp::ScalarTy::Auto => { panic!("Cannot Map Auto Types") }
        cpp::ScalarTy::Void => { c::ScalarTy::Void }
        cpp::ScalarTy::I32 => { c::ScalarTy::I32 }
        cpp::ScalarTy::U32 => { c::ScalarTy::U32 }
        cpp::ScalarTy::F32 => { c::ScalarTy::F32 }
        cpp::ScalarTy::F64 => { c::ScalarTy::F64 }
        cpp::ScalarTy::Bool => { c::ScalarTy::Bool }
        cpp::ScalarTy::SizeT => { c::ScalarTy::SizeT }
        cpp::ScalarTy::Memory => { c::ScalarTy::Memory }
        cpp::ScalarTy::Gpu => { c::ScalarTy::Gpu }
    }
}

pub fn walk_nat<V: CppToCMap>(mapper: &mut V, nat: & crate::ast::Nat) -> crate::ast::Nat {
    nat.clone()
}

pub fn walk_buffer_kind<V: CppToCMap>(mapper: &mut V, buffer_kind: & cpp::BufferKind) -> c::BufferKind {
    match buffer_kind {
        cpp::BufferKind::CpuMem => { c::BufferKind::CpuMem }
        cpp::BufferKind::GpuGlobal => { c::BufferKind::GpuGlobal }
        cpp::BufferKind::Ident(ident) => { c::BufferKind::Ident(ident.clone()) }
    }
}

pub fn walk_gpu_addr_space<V: CppToCMap>(mapper: &mut V, gpu_addr_space: & cpp::GpuAddrSpace) -> c::GpuAddrSpace {
    match gpu_addr_space {
        cpp::GpuAddrSpace::Global => { c::GpuAddrSpace::Global }
        //GpuAddrSpace::Local => {}
        cpp::GpuAddrSpace::Constant => { c::GpuAddrSpace::Constant }
        cpp::GpuAddrSpace::Shared => { c::GpuAddrSpace::Shared }
    }
}

pub fn walk_expr<V: CppToCMap>(mapper: &mut V, expr: & cpp::Expr) -> c::Expr {
    match expr {
        cpp::Expr::Empty => { c::Expr::Empty }
        cpp::Expr::Ident(ident) => { c::Expr::Ident(ident.clone()) }
        cpp::Expr::Lit(lit) => {
            c::Expr::Lit(mapper.map_lit(lit))
        }
        cpp::Expr::Assign { lhs, rhs } => {
            c::Expr::Assign {
                lhs: Box::new(mapper.map_expr(lhs)),
                rhs: Box::new(mapper.map_expr(rhs)) }
        }
        cpp::Expr::FunCall { fun, args, template_args } => {
            if !template_args.is_empty() {
                println!("Function with template Args {}", template_args.len());
            }
            c::Expr::FunCall {
                fun: Box::new(mapper.map_expr(fun)),
                args: map_list!(mapper, map_expr, args)
            }
        }
        cpp::Expr::UnOp { op, arg } => {
            c::Expr::UnOp {
                op: mapper.map_un_op(op),
                arg: Box::new(mapper.map_expr(expr)) }
        }
        cpp::Expr::BinOp { op, lhs, rhs } => {
            c::Expr::BinOp{
                op: mapper.map_bin_op(op),
                lhs: Box::new(mapper.map_expr(lhs)),
                rhs: Box::new(mapper.map_expr(rhs))
            }
        }
        cpp::Expr::ArraySubscript { array, index } => {
            c::Expr::ArraySubscript {
                array: Box::new(mapper.map_expr(array)),
                index: mapper.map_nat(index) }
        }
        cpp::Expr::Proj { tuple, n } => {
            c::Expr::Proj {
                tuple: Box::new(mapper.map_expr(tuple)), n: n.clone()
            }
        }
        cpp::Expr::InitializerList { elems } => {
            c::Expr::InitializerList { elems: map_list!(mapper, map_expr, elems) }
        }
        cpp::Expr::Ref(expr) => {
            c::Expr::Ref(Box::new(mapper.map_expr(expr)))
        }
        cpp::Expr::Deref(expr) => {
            c::Expr::Deref(Box::new(mapper.map_expr(expr)))
        }
        cpp::Expr::Tuple(expr_list) => {
            c::Expr::Tuple(map_list!(mapper, map_expr, expr_list))
        }
        cpp::Expr::Nat(nat) => {
            c::Expr::Nat(mapper.map_nat(nat))
        }
        cpp::Expr::Lambda { captures, params, body, ret_ty, is_dev_fun } => {
            // Lambdas should only appear for exec. Exec is called with Raw-String Param in OpenCL
            c::Expr::Ident("kernel".to_string())
        }
    }
}

pub fn walk_ident<V: CppToCMap>(mapper: &mut V, ident: & crate::ast::Ident) -> crate::ast::Ident {
    ident.clone()
}

pub fn walk_bin_op_nat<V: CppToCMap>(mapper: &mut V, bin_op_nat: & crate::ast::BinOpNat) -> crate::ast::BinOpNat {
    bin_op_nat.clone()
}

pub fn walk_lit<V: CppToCMap>(mapper: &mut V, lit: & cpp::Lit) -> c::Lit {
    match lit {
        cpp::Lit::Bool(b) => {c::Lit::Bool(b.clone())}
        cpp::Lit::I32(i) => {c::Lit::I32(i.clone())}
        cpp::Lit::U32(u) => {c::Lit::U32(u.clone())}
        cpp::Lit::F32(f) => {c::Lit::F32(f.clone())}
        cpp::Lit::F64(f) => {c::Lit::F64(f.clone())}
    }
}

pub fn walk_un_op<V: CppToCMap>(mapper: &mut V, un_op: & cpp::UnOp) -> c::UnOp {
    match un_op {
        cpp::UnOp::Not => {c::UnOp::Not}
        cpp::UnOp::Neg => {c::UnOp::Neg}
    }
}

pub fn walk_bin_op<V: CppToCMap>(mapper: &mut V, bin_op: & cpp::BinOp) -> c::BinOp{
    match bin_op {
        cpp::BinOp::Add => {c::BinOp::Add}
        cpp::BinOp::Sub => {c::BinOp::Sub}
        cpp::BinOp::Mul => {c::BinOp::Mul}
        cpp::BinOp::Div => {c::BinOp::Div}
        cpp::BinOp::Mod => {c::BinOp::Mod}
        cpp::BinOp::And => {c::BinOp::And}
        cpp::BinOp::Or => {c::BinOp::Or}
        cpp::BinOp::Eq => {c::BinOp::Eq}
        cpp::BinOp::Lt => {c::BinOp::Lt}
        cpp::BinOp::Le => {c::BinOp::Le}
        cpp::BinOp::Gt => {c::BinOp::Gt}
        cpp::BinOp::Ge => {c::BinOp::Ge}
        cpp::BinOp::Neq => {c::BinOp::Neq}
    }
}