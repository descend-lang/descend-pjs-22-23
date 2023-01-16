use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::Debug;
use crate::{ast, cpp_ast as cl};
use crate::ast as desc;
use crate::ast::{utils};
use crate::cpp_ast::Item;
use crate::cu_codegen as cu;
use crate::cpp_codegen as cpp;

mod printer;

// Precondition. all function definitions are successfully typechecked and
// therefore every subexpression stores a type
pub fn gen(compil_unit: &desc::CompilUnit, idx_checks: bool) -> String {
    //TODO: Generate Function Declarations
    let initial_fns_to_generate = collect_initial_fns_to_generate(compil_unit);

    let mut codegen_ctx = CodegenCtx::new();
    let mut initial_fns = Vec::with_capacity(initial_fns_to_generate.len() * 2);
    for fun_def in &initial_fns_to_generate {
        codegen_ctx.push_scope();
        initial_fns.push(gen_fun_def(fun_def, &compil_unit.fun_defs, idx_checks, &mut codegen_ctx));
        codegen_ctx.drop_scope();
        debug_assert_eq!(codegen_ctx.shape_ctx.map.len(), 0);
    }

    let cl_fun_defs = codegen_ctx
        .inst_fn_ctx
        .into_values()
        .chain(
            initial_fns
                .into_iter()
        )
        .collect::<Vec<_>>();

    let include = Item::Include { name: "include".to_string(), content: "descend.hpp".to_string() };
    let cl_program = std::iter::empty()
        .chain(cl_fun_defs)
        .collect::<Vec<_>>();
        
    //TODO: Iterate over function definitions with template Parameters and duplicate them for each entry in Parameter Values
    //TODO: Parameter von GPU Funktionen müssen in beiden Programmen vorkommen
    let mut cl_cpu_program:Vec<cl::Item> = vec![];
    let mut cl_gpu_program:Vec<cl::Item> = vec![];

    cl_program.into_iter().for_each(|item| {
        match item {
            Item::Include { .. }  => cl_cpu_program.push(item),
            Item::FunDef { is_gpu_function, .. } => {
                if is_gpu_function {
                   cl_gpu_program.push(item);
                } else {
                    cl_cpu_program.push(item);
                }
            }
        }
    });

    printer::print(&include, &cl_cpu_program, &cl_gpu_program)
}

fn collect_initial_fns_to_generate(comp_unit: &desc::CompilUnit) -> Vec<desc::FunDef> {
    comp_unit
        .fun_defs
        .iter()
        // Filter out the only functions that make sense to be generated without an explicit call.
        //
        // As soon as exec resources are generic, we need to know the specific values
        // that are used in a function call, so that we can inline them (therefore these functions
        // are not inlcuded).
        .filter(|f|
            f.param_decls
                .iter()
                .all(|p| !ast::utils::is_shape_ty(p.ty.as_ref().unwrap())
                    && !matches!(
                        &p.ty.as_ref().unwrap().ty,
                        desc::TyKind::Data(desc::DataTy {
                            dty: desc::DataTyKind::ThreadHierchy(_),
                            ..
                        }))
                    && f.generic_params.is_empty())
        )
        .cloned()
        .collect::<Vec<desc::FunDef>>()
}

// TODO: Push generated template functions to codegenCtx
fn gen_fun_def(gl_fun: &desc::FunDef, comp_unit: &[desc::FunDef], idx_checks: bool, codegen_ctx: &mut CodegenCtx) -> cl::Item{
    let desc::FunDef {
        name,
        generic_params: ty_idents,
        param_decls: params,
        ret_dty: ret_ty,
        exec,
        body_expr,
        ..
    } = gl_fun;

    cl::Item::FunDef {
        name: name.clone(),
        templ_params: cpp::gen_templ_params(ty_idents),
        params: cpp::gen_param_decls(params),
        ret_ty: cpp::gen_ty(&desc::TyKind::Data(ret_ty.clone()), desc::Mutability::Mut),
        body: gen_stmt(
            body_expr,
            !matches!(
                ret_ty,
                desc::DataTy {
                    dty: desc::DataTyKind::Scalar(desc::ScalarTy::Unit),
                    ..
                }
            ),
            &mut CodegenCtx::new(),
            comp_unit,
            false,
            idx_checks,
        ),
        is_gpu_function: is_dev_fun(*exec),
        //Template Values are openCL only (cuda can handle Template Params itself)
        templ_values: vec!()
    }
}

//TODO: implement
fn gen_stmt(
    expr: &desc::Expr,
    return_value: bool,
    codegen_ctx: &mut CodegenCtx,
    comp_unit: &[desc::FunDef],
    dev_fun: bool,
    idx_checks: bool,
) -> cl::Stmt{
    cl::Stmt::Skip
}

fn is_dev_fun(exec: desc::Exec) -> bool {
    match exec {
        desc::Exec::GpuGrid
        | desc::Exec::GpuBlock
        | desc::Exec::GpuWarp
        | desc::Exec::GpuThread => true,
        desc::Exec::CpuThread | desc::Exec::View => false,
    }
}

struct CodegenCtx {
    parall_ctx: ParallCtx,
    shape_ctx: ShapeCtx,
    // FunDefs which are generated from template Function Calls will be pushed here
    inst_fn_ctx: HashMap<String, Item>,
}

impl CodegenCtx {
    fn new() -> Self {
        CodegenCtx {
            parall_ctx: ParallCtx::new(),
            shape_ctx: ShapeCtx::new(),
            inst_fn_ctx: HashMap::new()
        }
    }

    fn push_scope(&mut self) {
        self.parall_ctx.push_scope();
        self.shape_ctx.push_scope();
    }

    fn drop_scope(&mut self) {
        self.parall_ctx.drop_scope();
        self.shape_ctx.drop_scope();
    }
}

type ParallCtx = ScopeCtx<ParallelityCollec>;
type ShapeCtx = ScopeCtx<ShapeExpr>;

#[derive(Default, Clone, Debug)]
struct ScopeCtx<T: Debug + Clone> {
    map: Vec<HashMap<String, T>>,
}

impl<T: Debug + Clone> ScopeCtx<T> {
    fn new() -> Self {
        ScopeCtx {
            map: vec![HashMap::new()],
        }
    }

    fn insert(&mut self, ident: &str, key: T) {
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

    fn get(&self, ident: &str) -> &T {
        for scope in self.map.iter().rev() {
            if let Some(key) = scope.get(ident) {
                return key;
            }
        }
        panic!("Identifier `{}` not in context.", ident)
    }

    fn contains_key(&self, ident: &str) -> bool {
        self.map.iter().rev().any(|scope| scope.contains_key(ident))
    }

    fn push_scope(&mut self) {
        self.map.push(HashMap::new())
    }

    fn drop_scope(&mut self) {
        if let None = self.map.pop() {
            panic!("There is no scope to remove.")
        }
    }
}

enum CheckedExpr {
    Expr(cl::Expr),
    // TODO Wrap in Box
    ExprIdxCheck(cl::Stmt, cl::Expr),
}

impl CheckedExpr {
    fn map<F>(&self, f: F) -> Self
        where
            F: Fn(cl::Expr) -> cl::Expr,
    {
        match self {
            Self::Expr(e) => Self::Expr(f(e.clone())),
            Self::ExprIdxCheck(c, e) => Self::ExprIdxCheck(c.clone(), f(e.clone())),
        }
    }

    fn expr(&self) -> &cl::Expr {
        match self {
            Self::Expr(e) => e,
            Self::ExprIdxCheck(c, e) => panic!("expected expr, found idxCheck"),
        }
    }
}

#[derive(Debug, Clone)]
enum ParallelityCollec {
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
    fn create_from(expr: &desc::Expr, parall_ctx: &ParallCtx) -> ParallelityCollec {
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

    fn create_parall_pl_expr(
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

fn is_parall_collec_ty(ty: &desc::Ty) -> bool {
    matches!(
        ty.ty,
        desc::TyKind::Data(desc::DataTy {
            dty: desc::DataTyKind::ThreadHierchy(_),
            ..
        })
    )
}

#[derive(Debug, Clone)]
enum ShapeExpr {
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
    fn create_from(expr: &desc::Expr, shape_ctx: &ShapeCtx) -> ShapeExpr {
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

    fn create_to_shape_shape(args: &[desc::Expr]) -> ShapeExpr {
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

    fn create_pl_expr_shape(shape: &desc::PlaceExpr, shape_ctx: &ShapeCtx) -> ShapeExpr {
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

    fn create_tuple_shape(elems: &[desc::Expr], shape_ctx: &ShapeCtx) -> ShapeExpr {
        ShapeExpr::Tuple {
            shapes: elems
                .iter()
                .map(|e| {
                    if ast::utils::is_shape_ty(e.ty.as_ref().unwrap()) {
                        ViewOrExpr::V(ShapeExpr::create_from(e, shape_ctx))
                    } else {
                        ViewOrExpr::E(e.clone())
                    }
                })
                .collect(),
        }
    }

    fn create_split_at_shape(
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

    fn create_group_shape(
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

    fn create_join_shape(
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

    fn create_transpose_shape(args: &[desc::Expr], shape_ctx: &ShapeCtx) -> ShapeExpr {
        if let Some(v) = args.first() {
            return ShapeExpr::Transpose {
                shape: Box::new(ShapeExpr::create_from(v, shape_ctx)),
            };
        }
        panic!("Cannot create `to_shape` from the provided arguments.");
    }

    fn collect_and_rename_input_exprs(&mut self) -> Vec<(String, desc::Expr)> {
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
enum ViewOrExpr {
    V(ShapeExpr),
    E(desc::Expr),
}
impl ViewOrExpr {
    fn expr(&self) -> desc::Expr {
        if let ViewOrExpr::E(expr) = self {
            expr.clone()
        } else {
            panic!("Not an expression.")
        }
    }
}