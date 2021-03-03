use crate::ast::{
    internal, BinOpNat, ExecLoc, Ident, IdentKinded, Kind, Memory, Nat, Ownership, Provenance,
    ScalarTy, Ty,
};

pub static GPU: &str = "gpu";
pub static GPU_ALLOC: &str = "gpu_alloc";
pub static COPY_TO_HOST: &str = "copy_to_host";
pub static SPAWN_THREADS: &str = "spawn_threads";

pub static TO_VIEW: &str = "to_view";
pub static TO_VIEW_MUT: &str = "to_view_mut";
pub static GROUP: &str = "group";
pub static JOIN: &str = "join";
pub static TRANSPOSE: &str = "transpose";

pub struct FunDecl {
    pub name: String,
    pub ty: Ty,
}

// TODO add correct predeclared functions with their types
pub(super) fn fun_decls() -> Vec<FunDecl> {
    let decls = [
        // Built-in functions
        (GPU, gpu_ty()),
        (GPU_ALLOC, gpu_alloc_ty()),
        (COPY_TO_HOST, copy_to_host_ty()),
        (SPAWN_THREADS, spawn_threads_ty()),
        // View constructors
        (TO_VIEW, to_view_ty(Ownership::Shrd)),
        (TO_VIEW_MUT, to_view_ty(Ownership::Uniq)),
    ];

    decls
        .iter()
        .map(|(name, ty)| FunDecl {
            name: (*name).to_string(),
            ty: ty.clone(),
        })
        .collect()
}

// gpu:
//   <>(i32) -[cpu.thread]-> Gpu
fn gpu_ty() -> Ty {
    Ty::Fn(
        vec![],
        vec![Ty::Scalar(ScalarTy::I32)],
        Box::new(internal::FrameExpr::Empty),
        ExecLoc::CpuThread,
        Box::new(Ty::Scalar(ScalarTy::Gpu)),
    )
}

// gpu_alloc:
//   <r1: prv, r2: prv, m1: mem, m2: mem, t: ty>(
//      &r1 uniq m1 Gpu, &r2 shrd m2 t
//   ) -[cpu.thread]-> t @ gpu.global
fn gpu_alloc_ty() -> Ty {
    let r1 = Ident::new("r1");
    let r2 = Ident::new("r2");
    let m1 = Ident::new("m1");
    let m2 = Ident::new("m2");
    let t = Ident::new("t");
    let r1_prv = IdentKinded {
        ident: r1.clone(),
        kind: Kind::Provenance,
    };
    let m1_mem = IdentKinded {
        ident: m1.clone(),
        kind: Kind::Memory,
    };
    let r2_prv = IdentKinded {
        ident: r2.clone(),
        kind: Kind::Provenance,
    };
    let m2_mem = IdentKinded {
        ident: m2.clone(),
        kind: Kind::Memory,
    };
    let t_ty = IdentKinded {
        ident: t.clone(),
        kind: Kind::Ty,
    };
    Ty::Fn(
        vec![r1_prv, r2_prv, m1_mem, m2_mem, t_ty],
        vec![
            Ty::Ref(
                Provenance::Ident(r1),
                Ownership::Uniq,
                Memory::Ident(m1),
                Box::new(Ty::Scalar(ScalarTy::Gpu)),
            ),
            Ty::Ref(
                Provenance::Ident(r2),
                Ownership::Shrd,
                Memory::Ident(m2),
                Box::new(Ty::Ident(t.clone())),
            ),
        ],
        Box::new(internal::FrameExpr::Empty),
        ExecLoc::CpuThread,
        Box::new(Ty::At(Box::new(Ty::Ident(t)), Memory::GpuGlobal)),
    )
}

// copy_to_host:
//   <a: prv, b: prv, t: ty>(&a shrd gpu.global ty, &b uniq cpu.heap ty)
//      -[cpu.thread]-> ()
fn copy_to_host_ty() -> Ty {
    let a = Ident::new("a");
    let b = Ident::new("b");
    let t = Ident::new("t");
    let a_prv = IdentKinded {
        ident: a.clone(),
        kind: Kind::Provenance,
    };
    let b_prv = IdentKinded {
        ident: b.clone(),
        kind: Kind::Provenance,
    };
    let t_ty = IdentKinded {
        ident: t.clone(),
        kind: Kind::Ty,
    };
    Ty::Fn(
        vec![a_prv, b_prv, t_ty],
        vec![
            Ty::Ref(
                Provenance::Ident(a),
                Ownership::Shrd,
                Memory::GpuGlobal,
                Box::new(Ty::Ident(t.clone())),
            ),
            Ty::Ref(
                Provenance::Ident(b),
                Ownership::Uniq,
                Memory::CpuHeap,
                Box::new(Ty::Ident(t)),
            ),
        ],
        Box::new(internal::FrameExpr::Empty),
        ExecLoc::CpuThread,
        Box::new(Ty::Scalar(ScalarTy::Unit)),
    )
}

// spawn_threads:
//  <nb: nat, nt: nat, a: prv>(&a shrd gpu) -> GridConfig<nb, nt>
fn spawn_threads_ty() -> Ty {
    let nb = Ident::new("nb");
    let nt = Ident::new("nt");
    let a = Ident::new("a");
    let m = Ident::new("m");
    let nb_nat = IdentKinded {
        ident: nb.clone(),
        kind: Kind::Nat,
    };
    let nt_nat = IdentKinded {
        ident: nt.clone(),
        kind: Kind::Nat,
    };
    let a_prv = IdentKinded {
        ident: a.clone(),
        kind: Kind::Provenance,
    };
    let m_mem = IdentKinded {
        ident: m.clone(),
        kind: Kind::Memory,
    };
    Ty::Fn(
        vec![nb_nat, nt_nat, a_prv, m_mem],
        vec![Ty::Ref(
            Provenance::Ident(a),
            Ownership::Shrd,
            Memory::Ident(m),
            Box::new(Ty::Scalar(ScalarTy::Gpu)),
        )],
        Box::new(internal::FrameExpr::Empty),
        ExecLoc::CpuThread,
        Box::new(Ty::GridConfig(Nat::Ident(nb), Nat::Ident(nt))),
    )
}

// to_view:
//  <r: prv, m: mem, n: nat, t: ty>(&r shrd m [t; n]) -[view]-> [[&r shrd m t; n]]
// to_view_mut:
//  <r: prv, m: mem, n: nat, t: ty>(&r uniq m [t; n]) -[view]-> [[&r uniq m t; n]]
fn to_view_ty(own: Ownership) -> Ty {
    let r = Ident::new("r");
    let m = Ident::new("m");
    let n = Ident::new("n");
    let t = Ident::new("t");
    let ex = Ident::new("ex");
    let r_prv = IdentKinded {
        ident: r.clone(),
        kind: Kind::Provenance,
    };
    let m_mem = IdentKinded {
        ident: m.clone(),
        kind: Kind::Memory,
    };
    let n_nat = IdentKinded {
        ident: n.clone(),
        kind: Kind::Nat,
    };
    let t_ty = IdentKinded {
        ident: t.clone(),
        kind: Kind::Ty,
    };
    Ty::Fn(
        vec![r_prv, m_mem, n_nat, t_ty],
        vec![Ty::Ref(
            Provenance::Ident(r.clone()),
            own,
            Memory::Ident(m.clone()),
            Box::new(Ty::Array(
                Box::new(Ty::Ident(t.clone())),
                Nat::Ident(n.clone()),
            )),
        )],
        Box::new(internal::FrameExpr::Empty),
        ExecLoc::View,
        Box::new(Ty::ArrayView(
            Box::new(Ty::Ref(
                Provenance::Ident(r),
                own,
                Memory::Ident(m),
                Box::new(Ty::Ident(t)),
            )),
            Nat::Ident(n),
        )),
    )
}

// group:
//  <s: nat, n: nat, t: ty>([[t; n]]) -> [[ [[t; size]]; n/size ]]
fn group_ty() -> Ty {
    let s = Ident::new("s");
    let n = Ident::new("n");
    let t = Ident::new("t");
    let s_nat = IdentKinded {
        ident: t.clone(),
        kind: Kind::Nat,
    };
    let n_nat = IdentKinded {
        ident: t.clone(),
        kind: Kind::Nat,
    };
    let t_ty = IdentKinded {
        ident: t.clone(),
        kind: Kind::Ty,
    };
    Ty::Fn(
        vec![s_nat, n_nat, t_ty],
        vec![Ty::ArrayView(
            Box::new(Ty::Ident(t.clone())),
            Nat::Ident(n.clone()),
        )],
        Box::new(internal::FrameExpr::Empty),
        ExecLoc::View,
        Box::new(Ty::ArrayView(
            Box::new(Ty::ArrayView(
                Box::new(Ty::Ident(t.clone())),
                Nat::Ident(s.clone()),
            )),
            Nat::BinOp(
                BinOpNat::Div,
                Box::new(Nat::Ident(n)),
                Box::new(Nat::Ident(s)),
            ),
        )),
    )
}

// join:
//  <m: nat, n: nat, t: ty>([[ [[t; n]]; m]]) -> [[t; m*n]]
fn join_ty() -> Ty {
    let m = Ident::new("m");
    let n = Ident::new("n");
    let t = Ident::new("t");
    let m_nat = IdentKinded {
        ident: t.clone(),
        kind: Kind::Nat,
    };
    let n_nat = IdentKinded {
        ident: t.clone(),
        kind: Kind::Nat,
    };
    let t_ty = IdentKinded {
        ident: t.clone(),
        kind: Kind::Ty,
    };
    Ty::Fn(
        vec![m_nat, n_nat, t_ty],
        vec![Ty::ArrayView(
            Box::new(Ty::ArrayView(
                Box::new(Ty::Ident(t.clone())),
                Nat::Ident(n.clone()),
            )),
            Nat::Ident(m.clone()),
        )],
        Box::new(internal::FrameExpr::Empty),
        ExecLoc::View,
        Box::new(Ty::ArrayView(
            Box::new(Ty::Ident(t)),
            Nat::BinOp(
                BinOpNat::Mul,
                Box::new(Nat::Ident(m)),
                Box::new(Nat::Ident(n)),
            ),
        )),
    )
}

// transpose:
//  <m: nat, n: nat, t: ty>([[ [[t; n]]; m]]) -> [[ [[t; m]]; n]]
fn transpose_ty() -> Ty {
    let m = Ident::new("m");
    let n = Ident::new("n");
    let t = Ident::new("t");
    let m_nat = IdentKinded {
        ident: t.clone(),
        kind: Kind::Nat,
    };
    let n_nat = IdentKinded {
        ident: t.clone(),
        kind: Kind::Nat,
    };
    let t_ty = IdentKinded {
        ident: t.clone(),
        kind: Kind::Ty,
    };
    Ty::Fn(
        vec![m_nat, n_nat, t_ty],
        vec![Ty::ArrayView(
            Box::new(Ty::ArrayView(
                Box::new(Ty::Ident(t.clone())),
                Nat::Ident(n.clone()),
            )),
            Nat::Ident(m.clone()),
        )],
        Box::new(internal::FrameExpr::Empty),
        ExecLoc::View,
        Box::new(Ty::ArrayView(
            Box::new(Ty::ArrayView(Box::new(Ty::Ident(t)), Nat::Ident(m))),
            Nat::Ident(n),
        )),
    )
}
