use crate::cpp_ast::*;


// Taken from the Rust compiler
macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr) => {
        for elem in $list {
            $visitor.$method(elem)
        }
    };
}

pub trait Visit: Sized {
    fn visit_item(& self, item: & Item) {
        walk_item(self, item);
    }
    fn visit_templ_param(& self, templ_param: & TemplParam) {
        walk_templ_param(self, templ_param);
    }
    fn visit_param_decl(& self, param_decl: & ParamDecl) {
        walk_param_decl(self, param_decl);
    }
    fn visit_ty(& self, ty: & Ty) {
        walk_ty(self, ty);
    }
    fn visit_stmt(& self, stmt: & Stmt) {
        walk_stmt(self, stmt);
    }
    fn visit_scalar_ty(& self, scalar_ty: & ScalarTy) {
        walk_scalar_ty(self, scalar_ty);
    }
    fn visit_nat(& self, nat: & Nat) { walk_nat(self, nat); }
    fn visit_buffer_kind(& self, buffer_kind: & BufferKind) {
        walk_buffer_kind(self, buffer_kind);
    }
    fn visit_gpu_addr_space(& self, addr_space: & GpuAddrSpace) {
        walk_gpu_addr_space(self, addr_space);
    }
    fn visit_expr(& self, expr: & Expr) {
        walk_expr(self, expr);
    }
    fn visit_ident(& self, ident: &crate::ast::Ident) {
        walk_ident(self, ident);
    }
    fn visit_bin_op_nat(& self, bin_op_nat: &crate::ast::BinOpNat) {
        walk_bin_op_nat(self, bin_op_nat);
    }
    fn visit_lit(& self, lit: & Lit) {
        walk_lit(self, lit);
    }
    fn visit_template_arg(& self, template_arg: & TemplateArg) {
        walk_template_arg(self, template_arg);
    }
    fn visit_un_op(& self, un_op: & UnOp) {
        walk_un_op(self, un_op);
    }
    fn visit_bin_op(& self, bin_op: & BinOp) {
        walk_bin_op(self, bin_op);
    }
}

pub fn walk_item<V: Visit>(visitor: & V, item: & Item) {
    match item {
        Item::Include { .. } => { todo!("Brauchen wir das? Enhält nur Strings") }
        Item::FunDef { templ_params, params, ret_ty, body, .. } => {
            walk_list!(visitor, visit_templ_param, templ_params);
            walk_list!(visitor, visit_param_decl, params);
            visitor.visit_ty(ret_ty);
            visitor.visit_stmt(body);
        }
    };
}

pub fn walk_templ_param<V: Visit>(visitor: & V, templ_param: & TemplParam) {
    match templ_param {
        TemplParam::Value { ty, .. } => {
            visitor.visit_ty(ty);
        }
        TemplParam::TyName { .. } => { todo!("Enthält nur Strings") }
    };
}

pub fn walk_param_decl<V: Visit>(visitor: & V, param_decl: & ParamDecl) {
    visitor.visit_ty(& param_decl.ty);
}

pub fn walk_ty<V: Visit>(visitor: & V, ty: & Ty) {
    match ty {
        Ty::Scalar(scalar_ty) => { visitor.visit_scalar_ty(scalar_ty) }
        Ty::Atomic(scalar_ty) => { visitor.visit_scalar_ty(scalar_ty) }
        Ty::Tuple(ty_vec) => { walk_list!(visitor, visit_ty, ty_vec) }
        Ty::Array(ty, nat) => {
            visitor.visit_ty(ty);
            visitor.visit_nat(nat);
        }
        Ty::CArray(ty, nat) => {
            visitor.visit_ty(ty);
            visitor.visit_nat(nat);
        }
        Ty::Buffer(ty, buffer_kind) => {
            visitor.visit_ty(ty);
            visitor.visit_buffer_kind(buffer_kind);
        }
        Ty::Ptr(ty, addr_space_opt) => {
            visitor.visit_ty(ty);
            if let Some(addr_space) = addr_space_opt {
                visitor.visit_gpu_addr_space(addr_space);
            }
        }
        Ty::PtrConst(ty, addr_space_opt) => {
            visitor.visit_ty(ty);
            if let Some(addr_space) = addr_space_opt {
                visitor.visit_gpu_addr_space(addr_space);
            }
        }
        Ty::Const(ty) => {visitor.visit_ty(ty);}
        Ty::Ident(_) => {}
    }
}

pub fn walk_stmt<V: Visit>(visitor: & V, stmt: & Stmt) {
    match stmt {
        Stmt::Skip => {}
        Stmt::VarDecl { ty, addr_space, expr, .. } => {
            visitor.visit_ty(ty);
            if let Some(addr_space_val) = addr_space {
                visitor.visit_gpu_addr_space(addr_space_val);
            }
            if let Some(expr_val) = expr {
                visitor.visit_expr(expr_val);
            }
        }
        Stmt::Block(stmt) => {
            visitor.visit_stmt(stmt);
        }
        Stmt::Seq(stmt_list) => {
            walk_list!(visitor, visit_stmt, stmt_list);
        }
        Stmt::Expr(expr) => {
            visitor.visit_expr(expr);
        }
        Stmt::If { cond, body } => {
            visitor.visit_expr(cond);
            visitor.visit_stmt(body);
        }
        Stmt::IfElse { cond, true_body, false_body } => {
            visitor.visit_expr(cond);
            visitor.visit_stmt(true_body);
            visitor.visit_stmt(false_body);
        }
        Stmt::While { cond, stmt } => {
            visitor.visit_expr(cond);
            visitor.visit_stmt(stmt);
        }
        Stmt::ForLoop { init, cond, iter, stmt } => {
            visitor.visit_stmt(init);
            visitor.visit_expr(cond);
            visitor.visit_expr(iter);
            visitor.visit_stmt(stmt);
        }
        Stmt::Return(expr) => {
            if let Some(expr_val) = expr {
                visitor.visit_expr(expr_val);
            }
        }
        Stmt::Label(_) => {todo!("String only!")}
    }
}

pub fn walk_scalar_ty<V: Visit>(visitor: & V, scalar_ty: & ScalarTy) {
    //Probably not needed (enum has no values)
}

pub fn walk_nat<V: Visit>(visitor: & V, nat: & Nat) {
    match nat {
        Nat::Ident(ident) => {visitor.visit_ident(ident);}
        Nat::Lit(_) => {}
        Nat::BinOp(bin_op_nat, nat_left, nat_right) => {
            visitor.visit_bin_op_nat(bin_op_nat);
            visitor.visit_nat(nat_left);
            visitor.visit_nat(nat_right);
        }
        Nat::App(ident, nat_list) => {
            visitor.visit_ident(ident);
            walk_list!(visitor, visit_nat, nat_list);
        }
    }
}

pub fn walk_buffer_kind<V: Visit>(visitor: & V, buffer_kind: & BufferKind) {
    match buffer_kind {
        BufferKind::CpuMem => {}
        BufferKind::GpuGlobal => {}
        BufferKind::Ident(_) => {}
    }
}

pub fn walk_gpu_addr_space<V: Visit>(visitor: & V, gpu_addr_space: & GpuAddrSpace) {
    match gpu_addr_space {
        GpuAddrSpace::Global => {}
        //GpuAddrSpace::Local => {}
        GpuAddrSpace::Constant => {}
        GpuAddrSpace::Shared => {}
    }
}

pub fn walk_expr<V: Visit>(visitor: & V, expr: & Expr) {
    match expr {
        Expr::Empty => {}
        Expr::Ident(_) => {}
        Expr::Lit(lit) => {
            visitor.visit_lit(lit);
        }
        Expr::Assign { lhs, rhs } => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        Expr::Lambda { params, body, ret_ty, .. } => {
            // Todo: What about Captures? They are defined from descend ast
            walk_list!(visitor, visit_param_decl, params);
            visitor.visit_stmt(body);
            visitor.visit_ty(ret_ty);
        }
        Expr::FunCall { fun, template_args, args } => {
            visitor.visit_expr(fun);
            walk_list!(visitor, visit_template_arg, template_args);
            walk_list!(visitor, visit_expr, args);
        }
        Expr::UnOp { op, arg } => {
            visitor.visit_un_op(op);
            visitor.visit_expr(expr);
        }
        Expr::BinOp { op, lhs, rhs } => {
            visitor.visit_bin_op(op);
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        Expr::ArraySubscript { array, index } => {
            visitor.visit_expr(array);
            visitor.visit_nat(index);
        }
        Expr::Proj { tuple, n } => {
            visitor.visit_expr(tuple);
        }
        Expr::InitializerList { elems } => {
            walk_list!(visitor, visit_expr, elems);
        }
        Expr::Ref(expr) => {
            visitor.visit_expr(expr);
        }
        Expr::Deref(expr) => {
            visitor.visit_expr(expr);
        }
        Expr::Tuple(expr_list) => {
            walk_list!(visitor, visit_expr, expr_list);
        }
        Expr::Nat(nat) => {
            visitor.visit_nat(nat);
        }
    }
}

pub fn walk_ident<V: Visit>(visitor: & V, ident: &crate::ast::Ident) {
    //Todo: Span Nescessary?
}

pub fn walk_bin_op_nat<V:Visit>(visitor: & V, bin_op_nat: &crate::ast::BinOpNat) {

}

pub fn walk_lit<V: Visit>(visitor: & V, lit: & Lit) {

}

pub fn walk_template_arg<V: Visit>(visitor: & V, template_arg: & TemplateArg) {
    match template_arg {
        TemplateArg::Expr(expr) => {
            visitor.visit_expr(expr);
        }
        TemplateArg::Ty(ty) => {
            visitor.visit_ty(ty);
        }
    }
}

pub fn walk_un_op<V: Visit>(visitor: & V, un_op: & UnOp) {
    match un_op {
        UnOp::Not => {}
        UnOp::Neg => {}
    }
}

pub fn walk_bin_op<V: Visit>(visitor: & V, bin_op: & BinOp) {
    match bin_op {
        BinOp::Add => {}
        BinOp::Sub => {}
        BinOp::Mul => {}
        BinOp::Div => {}
        BinOp::Mod => {}
        BinOp::And => {}
        BinOp::Or => {}
        BinOp::Eq => {}
        BinOp::Lt => {}
        BinOp::Le => {}
        BinOp::Gt => {}
        BinOp::Ge => {}
        BinOp::Neq => {}
    }
}