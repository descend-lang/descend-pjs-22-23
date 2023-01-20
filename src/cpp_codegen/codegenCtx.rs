use std::collections::HashMap;
use std::fmt::Debug;
use crate::cpp_ast as ast;
use crate::ast as desc;
use crate::ast::utils;

pub struct CodegenCtx {
    pub parall_ctx: ParallCtx,
    pub shape_ctx: ShapeCtx,
    // FunDefs which are generated from template Function Calls will be pushed here
    pub inst_fn_ctx: HashMap<String, ast::Item>,
}

impl CodegenCtx {
    pub fn new() -> Self {
        CodegenCtx {
            parall_ctx: ParallCtx::new(),
            shape_ctx: ShapeCtx::new(),
            inst_fn_ctx: HashMap::new()
        }
    }

    pub fn push_scope(&mut self) {
        self.parall_ctx.push_scope();
        self.shape_ctx.push_scope();
    }

    pub fn drop_scope(&mut self) {
        self.parall_ctx.drop_scope();
        self.shape_ctx.drop_scope();
    }
}

pub type ParallCtx = ScopeCtx<ParallelityCollec>;
pub type ShapeCtx = ScopeCtx<ShapeExpr>;

#[derive(Default, Clone, Debug)]
pub struct ScopeCtx<T: Debug + Clone> {
    pub map: Vec<HashMap<String, T>>,
}

impl<T: Debug + Clone> ScopeCtx<T> {
    pub(crate) fn new() -> Self {
        ScopeCtx {
            map: vec![HashMap::new()],
        }
    }

    pub(crate) fn insert(&mut self, ident: &str, key: T) {
        match self.map.last_mut() {
            Some(map) => {
                if let Some(old) = map.insert(ident.to_string(), key.clone()) {
                    panic!(
                        "Reassigning variable from `{i} = {old:?}` to `{i} = {new:?}`",
                        i = ident,
                        old = old,
                        new = key
                    )
                }
            }
            None => panic!("No scoped mapping found."),
        }
    }

    pub(crate) fn get(&self, ident: &str) -> &T {
        for scope in self.map.iter().rev() {
            if let Some(key) = scope.get(ident) {
                return key;
            }
        }
        panic!("Identifier `{}` not in context.", ident)
    }

    pub(crate) fn contains_key(&self, ident: &str) -> bool {
        self.map.iter().rev().any(|scope| scope.contains_key(ident))
    }

    pub(crate) fn push_scope(&mut self) {
        self.map.push(HashMap::new())
    }

    pub(crate) fn drop_scope(&mut self) {
        if let None = self.map.pop() {
            panic!("There is no scope to remove.")
        }
    }
}

pub enum CheckedExpr {
    Expr(ast::Expr),
    // TODO Wrap in Box
    ExprIdxCheck(ast::Stmt, ast::Expr),
}

impl CheckedExpr {
    pub fn map<F>(&self, f: F) -> Self
        where
            F: Fn(ast::Expr) -> ast::Expr,
    {
        match self {
            Self::Expr(e) => Self::Expr(f(e.clone())),
            Self::ExprIdxCheck(c, e) => Self::ExprIdxCheck(c.clone(), f(e.clone())),
        }
    }

    pub fn expr(&self) -> &ast::Expr {
        match self {
            Self::Expr(e) => e,
            Self::ExprIdxCheck(c, e) => panic!("expected expr, found idxCheck"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParallelityCollec {
    Ident(desc::Ident),
    Proj {
        parall_expr: Box<ParallelityCollec>,
        i: usize,
    },
    Split {
        pos: desc::Nat,
        coll_size: desc::Nat,
        parall_expr: Box<ParallelityCollec>,
    },
}

impl ParallelityCollec {
    pub fn create_from(expr: &desc::Expr, parall_ctx: &ParallCtx) -> ParallelityCollec {
        match &expr.expr {
            desc::ExprKind::App(f, gen_args, args) => {
                if let desc::ExprKind::PlaceExpr(desc::PlaceExpr {
                                                     pl_expr: desc::PlaceExprKind::Ident(ident),
                                                     ..
                                                 }) = &f.expr
                {
                    if ident.name == crate::ty_check::pre_decl::SPLIT_THREAD_GRP
                        || ident.name == crate::ty_check::pre_decl::SPLIT_BLOCK_GRP
                        || ident.name == crate::ty_check::pre_decl::SPLIT_WARP_GRP
                    {
                        if let (desc::ArgKinded::Nat(k), desc::ArgKinded::Nat(n), Some(p)) =
                            (&gen_args[0], &gen_args[1], args.first())
                        {
                            return ParallelityCollec::Split {
                                pos: k.clone(),
                                coll_size: n.clone(),
                                parall_expr: Box::new(ParallelityCollec::create_from(
                                    p, parall_ctx,
                                )),
                            };
                        }
                        panic!("Cannot create `split` for parallel collection from the provided arguments.");
                    } else if ident.name == crate::ty_check::pre_decl::SPLIT_WARP {
                        if let (desc::ArgKinded::Nat(k), Some(p)) = (&gen_args[0], args.first()) {
                            return ParallelityCollec::Split {
                                pos: k.clone(),
                                coll_size: desc::Nat::Lit(32),
                                parall_expr: Box::new(ParallelityCollec::create_from(
                                    p, parall_ctx,
                                )),
                            };
                        }
                        panic!("Cannot create `split` from the provided arguments.");
                    } else {
                        unimplemented!()
                    }
                } else {
                    panic!(
                        "Non-globally defined functions that can transform parallel collections \
                     do not exist."
                    )
                }
            }
            desc::ExprKind::PlaceExpr(pl_expr) => {
                ParallelityCollec::create_parall_pl_expr(pl_expr, parall_ctx)
            }
            desc::ExprKind::Proj(expr, i) => ParallelityCollec::Proj {
                parall_expr: Box::new(ParallelityCollec::create_from(expr, parall_ctx)),
                i: *i,
            },
            _ => panic!(
                "Expected a function application, identifer or projection, but found {:?}",
                expr.expr
            ),
        }
    }

    pub fn create_parall_pl_expr(
        parall_expr: &desc::PlaceExpr,
        parall_ctx: &ParallCtx,
    ) -> ParallelityCollec {
        match parall_expr {
            desc::PlaceExpr {
                pl_expr: desc::PlaceExprKind::Ident(ident),
                ..
            } => parall_ctx.get(&ident.name).clone(),
            desc::PlaceExpr {
                pl_expr: desc::PlaceExprKind::Proj(pp, i),
                ..
            } => ParallelityCollec::Proj {
                parall_expr: Box::new(ParallelityCollec::create_parall_pl_expr(pp, parall_ctx)),
                i: *i,
            },
            desc::PlaceExpr {
                pl_expr: desc::PlaceExprKind::Deref(_),
                ..
            } => panic!(
                "It is not possible to take references of Grids or Blocks.\
                This should never happen."
            ),
        }
    }
}

pub(crate) fn is_parall_collec_ty(ty: &desc::Ty) -> bool {
    matches!(
        ty.ty,
        desc::TyKind::Data(desc::DataTy {
            dty: desc::DataTyKind::ThreadHierchy(_),
            ..
        })
    )
}

#[derive(Debug, Clone)]
pub enum ShapeExpr {
    ToView {
        ref_expr: Box<desc::Expr>,
    },
    Tuple {
        shapes: Vec<ViewOrExpr>,
    },
    Idx {
        idx: desc::Nat,
        shape: Box<ShapeExpr>,
    },
    Proj {
        shape: Box<ShapeExpr>,
        i: usize,
    },
    SplitAt {
        pos: desc::Nat,
        shape: Box<ShapeExpr>,
    },
    Group {
        size: desc::Nat,
        shape: Box<ShapeExpr>,
    },
    Join {
        group_size: desc::Nat,
        shape: Box<ShapeExpr>,
    },
    Transpose {
        shape: Box<ShapeExpr>,
    },
}

impl ShapeExpr {
    // Precondition: Expression is a fully typed function application and has type ArrayView.
    pub fn create_from(expr: &desc::Expr, shape_ctx: &ShapeCtx) -> ShapeExpr {
        match &expr.expr {
            // TODO this is assuming that f is an identifier
            desc::ExprKind::App(f, gen_args, args) => {
                if let desc::ExprKind::PlaceExpr(desc::PlaceExpr {
                                                     pl_expr: desc::PlaceExprKind::Ident(ident),
                                                     ..
                                                 }) = &f.expr
                {
                    if ident.name == crate::ty_check::pre_decl::TO_VIEW
                        || ident.name == crate::ty_check::pre_decl::TO_VIEW_MUT
                    {
                        ShapeExpr::create_to_shape_shape(args)
                    } else if ident.name == crate::ty_check::pre_decl::GROUP
                        || ident.name == crate::ty_check::pre_decl::GROUP_MUT
                    {
                        ShapeExpr::create_group_shape(gen_args, args, shape_ctx)
                    } else if ident.name == crate::ty_check::pre_decl::JOIN
                        || ident.name == crate::ty_check::pre_decl::JOIN_MUT
                    {
                        ShapeExpr::create_join_shape(gen_args, args, shape_ctx)
                    } else if ident.name == crate::ty_check::pre_decl::TRANSPOSE
                        || ident.name == crate::ty_check::pre_decl::TRANSPOSE_MUT
                    {
                        ShapeExpr::create_transpose_shape(args, shape_ctx)
                    } else {
                        unimplemented!()
                    }
                } else {
                    panic!("Non-globally defined shape functions do not exist.")
                }
            }
            desc::ExprKind::Split(_, _, _, s, shape) => {
                if let desc::PlaceExpr {
                    pl_expr: desc::PlaceExprKind::Deref(shape),
                    ..
                } = shape.as_ref()
                {
                    ShapeExpr::create_split_at_shape(s, shape.as_ref(), shape_ctx)
                } else {
                    panic!(
                        "An error pointing out that only a value must be split by reborrowing \
                        should have been thrown before."
                    )
                }
            }
            desc::ExprKind::PlaceExpr(pl_expr) => {
                ShapeExpr::create_pl_expr_shape(pl_expr, shape_ctx)
            }
            desc::ExprKind::Proj(expr, i) => ShapeExpr::Proj {
                shape: Box::new(ShapeExpr::create_from(expr, shape_ctx)),
                i: *i,
            },
            desc::ExprKind::Tuple(elems) => ShapeExpr::create_tuple_shape(elems, shape_ctx),
            desc::ExprKind::Ref(
                _,
                _,
                desc::PlaceExpr {
                    pl_expr: desc::PlaceExprKind::Deref(ple),
                    ..
                },
            ) => ShapeExpr::create_pl_expr_shape(ple, shape_ctx),
            _ => {
                panic!(
                    "Expected a function application, identifer or projection, but found {:?}",
                    expr.expr
                )
            }
        }
    }

    pub fn create_to_shape_shape(args: &[desc::Expr]) -> ShapeExpr {
        match args.first() {
            Some(e) =>
            // e cannot contain shapes, so the shape_ctx can be empty
                {
                    ShapeExpr::ToView {
                        ref_expr: Box::new(e.clone()),
                    }
                }
            _ => panic!("Place expression argument for to shape does not exist."),
        }
    }

    pub fn create_pl_expr_shape(shape: &desc::PlaceExpr, shape_ctx: &ShapeCtx) -> ShapeExpr {
        match shape {
            desc::PlaceExpr {
                pl_expr: desc::PlaceExprKind::Ident(ident),
                ..
            } => shape_ctx.get(&ident.name).clone(),
            desc::PlaceExpr {
                pl_expr: desc::PlaceExprKind::Proj(vv, i),
                ..
            } => ShapeExpr::Proj {
                shape: Box::new(ShapeExpr::create_pl_expr_shape(vv, shape_ctx)),
                i: *i,
            },
            desc::PlaceExpr {
                pl_expr: desc::PlaceExprKind::Deref(_),
                ..
            } => {
                panic!("It is not possible to take references of shapes. This should never happen.")
            }
        }
    }

    pub fn create_tuple_shape(elems: &[desc::Expr], shape_ctx: &ShapeCtx) -> ShapeExpr {
        ShapeExpr::Tuple {
            shapes: elems
                .iter()
                .map(|e| {
                    if utils::is_shape_ty(e.ty.as_ref().unwrap()) {
                        ViewOrExpr::V(ShapeExpr::create_from(e, shape_ctx))
                    } else {
                        ViewOrExpr::E(e.clone())
                    }
                })
                .collect(),
        }
    }

    pub fn create_split_at_shape(
        s: &desc::Nat,
        shape: &desc::PlaceExpr,
        shape_ctx: &ShapeCtx,
    ) -> ShapeExpr {
        ShapeExpr::SplitAt {
            pos: s.clone(),
            shape: Box::new(ShapeExpr::create_from(
                &desc::Expr::new(desc::ExprKind::PlaceExpr(shape.clone())),
                shape_ctx,
            )),
        }
    }

    pub fn create_group_shape(
        gen_args: &[desc::ArgKinded],
        args: &[desc::Expr],
        shape_ctx: &ShapeCtx,
    ) -> ShapeExpr {
        if let (desc::ArgKinded::Nat(s), Some(v)) = (&gen_args[0], args.first()) {
            return ShapeExpr::Group {
                size: s.clone(),
                shape: Box::new(ShapeExpr::create_from(v, shape_ctx)),
            };
        }
        panic!("Cannot create `group` from the provided arguments.");
    }

    pub fn create_join_shape(
        gen_args: &[desc::ArgKinded],
        args: &[desc::Expr],
        shape_ctx: &ShapeCtx,
    ) -> ShapeExpr {
        if let (desc::ArgKinded::Nat(n), Some(v)) = (&gen_args[3], args.first()) {
            return ShapeExpr::Join {
                group_size: n.clone(),
                shape: Box::new(ShapeExpr::create_from(v, shape_ctx)),
            };
        }
        panic!("Cannot create `to_view` from the provided arguments.");
    }

    pub fn create_transpose_shape(args: &[desc::Expr], shape_ctx: &ShapeCtx) -> ShapeExpr {
        if let Some(v) = args.first() {
            return ShapeExpr::Transpose {
                shape: Box::new(ShapeExpr::create_from(v, shape_ctx)),
            };
        }
        panic!("Cannot create `to_shape` from the provided arguments.");
    }

    pub fn collect_and_rename_input_exprs(&mut self) -> Vec<(String, desc::Expr)> {
        fn collect_and_rename_input_exprs_rec(
            v: &mut ShapeExpr,
            count: &mut u32,
            mut vec: Vec<(String, desc::Expr)>,
        ) -> Vec<(String, desc::Expr)> {
            match v {
                ShapeExpr::ToView { ref_expr } => {
                    let new_name = format!("p{}", *count);
                    vec.push((new_name.clone(), ref_expr.as_ref().clone()));
                    ref_expr.expr = desc::ExprKind::PlaceExpr(desc::PlaceExpr::new(
                        desc::PlaceExprKind::Ident(desc::Ident::new(&new_name)),
                    ));
                    *count += 1;
                    vec
                }
                ShapeExpr::SplitAt { shape, .. } => {
                    collect_and_rename_input_exprs_rec(shape, count, vec)
                }
                ShapeExpr::Group { shape, .. } => {
                    collect_and_rename_input_exprs_rec(shape, count, vec)
                }
                ShapeExpr::Join { shape, .. } => {
                    collect_and_rename_input_exprs_rec(shape, count, vec)
                }
                ShapeExpr::Transpose { shape, .. } => {
                    collect_and_rename_input_exprs_rec(shape, count, vec)
                }
                ShapeExpr::Tuple { shapes: elems } => {
                    let mut renamed = vec;
                    for e in elems {
                        match e {
                            ViewOrExpr::V(v) => {
                                renamed = collect_and_rename_input_exprs_rec(v, count, renamed);
                            }
                            ViewOrExpr::E(expr) => {
                                let new_name = format!("p{}", *count);
                                renamed.push((new_name.clone(), expr.clone()));
                                expr.expr = desc::ExprKind::PlaceExpr(desc::PlaceExpr::new(
                                    desc::PlaceExprKind::Ident(desc::Ident::new(&new_name)),
                                ));
                                *count += 1;
                            }
                        }
                    }
                    renamed
                }
                ShapeExpr::Idx { shape, .. } => {
                    collect_and_rename_input_exprs_rec(shape, count, vec)
                }
                ShapeExpr::Proj { shape, .. } => {
                    collect_and_rename_input_exprs_rec(shape, count, vec)
                }
            }
        }
        let vec = vec![];
        let mut count = 0;
        collect_and_rename_input_exprs_rec(self, &mut count, vec)
    }
}

#[derive(Debug, Clone)]
pub enum ViewOrExpr {
    V(ShapeExpr),
    E(desc::Expr),
}
impl ViewOrExpr {
    pub fn expr(&self) -> desc::Expr {
        if let ViewOrExpr::E(expr) = self {
            expr.clone()
        } else {
            panic!("Not an expression.")
        }
    }
}