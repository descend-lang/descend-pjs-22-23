use crate::ast as desc;
use crate::cpp_ast as ast;

pub(crate) fn gen_templ_params(ty_idents: &[desc::IdentKinded]) -> Vec<ast::TemplParam> {
    ty_idents
        .iter()
        .filter_map(|ty_ident| {
            if !matches!(ty_ident.kind, desc::Kind::Provenance) {
                Some(gen_templ_param(ty_ident))
            } else {
                None
            }
        })
        .collect()
}

fn gen_templ_param(ty_ident: &desc::IdentKinded) -> ast::TemplParam {
    let name = ty_ident.ident.name.clone();
    match ty_ident.kind {
        desc::Kind::Nat => ast::TemplParam::Value {
            param_name: name,
            ty: ast::Ty::Scalar(ast::ScalarTy::SizeT),
        },
        desc::Kind::Memory => ast::TemplParam::Value {
            param_name: name,
            ty: ast::Ty::Scalar(ast::ScalarTy::Memory),
        },
        desc::Kind::Ty => ast::TemplParam::TyName { name },
        _ => panic!("Cannot generate template parameter for {:?}", ty_ident.kind),
    }
}

pub fn gen_param_decls(param_decls: &[desc::ParamDecl]) -> Vec<ast::ParamDecl> {
    param_decls.iter().map(gen_param_decl).collect()
}

fn gen_param_decl(param_decl: &desc::ParamDecl) -> ast::ParamDecl {
    let desc::ParamDecl { ident, ty, mutbl } = param_decl;
    ast::ParamDecl {
        name: ident.name.clone(),
        ty: gen_ty(&ty.as_ref().unwrap().ty, *mutbl),
    }
}

// TODO: Make private again if possible
// Param mutbl is not strictly necessary because every const type can just be wrapped
// in ast::Ty::Const. However, the formalism uses this, because it shows the generated code
// as opposed to a Cuda-AST and there, the order of the const is different
// when it comes to pointers (C things).
pub fn gen_ty(ty: &desc::TyKind, mutbl: desc::Mutability) -> ast::Ty {
    use desc::DataTyKind as d;
    use desc::TyKind::*;

    let m = desc::Mutability::Mut;
    let cu_ty = match ty {
        Ident(ident) => ast::Ty::Ident(ident.name.clone()),
        Data(desc::DataTy {
                 dty: d::Atomic(a), ..
             }) => match a {
            desc::ScalarTy::Unit => ast::Ty::Atomic(ast::ScalarTy::Void),
            desc::ScalarTy::I32 => ast::Ty::Atomic(ast::ScalarTy::I32),
            desc::ScalarTy::U32 => ast::Ty::Atomic(ast::ScalarTy::U32),
            desc::ScalarTy::F32 => ast::Ty::Atomic(ast::ScalarTy::F32),
            desc::ScalarTy::F64 => ast::Ty::Atomic(ast::ScalarTy::F64),
            desc::ScalarTy::Bool => ast::Ty::Atomic(ast::ScalarTy::Bool),
            desc::ScalarTy::Gpu => ast::Ty::Atomic(ast::ScalarTy::Gpu),
        },
        Data(desc::DataTy {
                 dty: d::Scalar(s), ..
             }) => match s {
            desc::ScalarTy::Unit => ast::Ty::Scalar(ast::ScalarTy::Void),
            desc::ScalarTy::I32 => ast::Ty::Scalar(ast::ScalarTy::I32),
            desc::ScalarTy::U32 => ast::Ty::Scalar(ast::ScalarTy::U32),
            desc::ScalarTy::F32 => ast::Ty::Scalar(ast::ScalarTy::F32),
            desc::ScalarTy::F64 => ast::Ty::Scalar(ast::ScalarTy::F64),
            desc::ScalarTy::Bool => ast::Ty::Scalar(ast::ScalarTy::Bool),
            desc::ScalarTy::Gpu => ast::Ty::Scalar(ast::ScalarTy::Gpu),
        },
        //TODO: Use Std::Tuple instead of Thrust
        Data(desc::DataTy {
                 dty: d::Tuple(tys), ..
             }) => ast::Ty::Tuple(tys.iter().map(|ty| gen_ty(&Data(ty.clone()), m)).collect()),
        Data(desc::DataTy {
                 dty: d::Array(ty, n),
                 ..
             }) => ast::Ty::Array(Box::new(gen_ty(&Data(ty.as_ref().clone()), m)), n.clone()),
        Data(desc::DataTy {
                 dty: d::At(ty, mem),
                 ..
             }) => {
            //TODO: Wo tritt das hier auf? Müssen wir eventuell sowas Kernel Seitig und Host Seitig erzeuen?
            if let desc::Memory::GpuShared = mem {
                let dty = match ty.as_ref() {
                    desc::DataTy {
                        dty: d::Array(dty, _),
                        ..
                    } => dty.as_ref().clone(),
                    _ => ty.as_ref().clone(),
                };
                ast::Ty::Ptr(
                    Box::new(gen_ty(&desc::TyKind::Data(dty), mutbl)),
                    Some(ast::GpuAddrSpace::Local),
                )
            } else {
                let buff_kind = match mem {
                    desc::Memory::CpuMem => ast::BufferKind::CpuMem,
                    desc::Memory::GpuGlobal => ast::BufferKind::GpuGlobal,
                    desc::Memory::Ident(ident) => ast::BufferKind::Ident(ident.name.clone()),
                    desc::Memory::GpuShared => unimplemented!(),
                    desc::Memory::GpuLocal => {
                        panic!("GpuLocal is not valid for At types. Should never appear here.")
                    }
                };
                ast::Ty::Buffer(Box::new(gen_ty(&Data(ty.as_ref().clone()), m)), buff_kind)
            }
        }
        Data(desc::DataTy {
                 dty: d::Ref(_, own, _, ty),
                 ..
             }) => {
            let tty = Box::new(gen_ty(
                &Data(
                    // TODO: Eigene Funktion
                    match ty.as_ref() {
                        // Pointers to arrays point to the element type.
                        desc::DataTy {
                            dty: d::Array(elem_ty, _),
                            ..
                        } => elem_ty.as_ref().clone(),
                        _ => ty.as_ref().clone(),
                    }),
                m,
            ));
            if matches!(own, desc::Ownership::Uniq) {
                ast::Ty::Ptr(tty, None)
            } else {
                ast::Ty::PtrConst(tty, None)
            }
        }
        Data(desc::DataTy {
                 dty: d::RawPtr(ty), ..
             }) => {
            let tty = Box::new(gen_ty(
                &Data(match ty.as_ref() {
                    desc::DataTy {
                        dty: d::Array(_, _),
                        ..
                    } => panic!("should never happen"),
                    _ => ty.as_ref().clone(),
                }),
                desc::Mutability::Mut,
            ));
            ast::Ty::Ptr(tty, None)
        }
        // TODO is this  correct. I guess we want to generate type identifiers in generic functions.
        Data(desc::DataTy {
                 dty: d::Ident(ident),
                 ..
             }) => ast::Ty::Ident(ident.name.clone()),
        Data(desc::DataTy {
                 dty: d::ArrayShape(_, _),
                 ..
             }) => panic!(
            "Cannot generate array shape types.\
            Anything with this type should have been compiled away."
        ),
        Data(desc::DataTy {
                 dty: d::Dead(_), ..
             }) => {
            panic!("Dead types are only for type checking and cannot be generated.")
        }
        Fn(_, _, _, _) => unimplemented!("needed?"),
        Dead(_) => panic!("Dead types cannot be generated."),
        Data(desc::DataTy {
                 dty: desc::DataTyKind::ThreadHierchy(_),
                 ..
             })
        | Data(desc::DataTy {
                   dty: desc::DataTyKind::SplitThreadHierchy(_, _),
                   ..
               })
        | Data(desc::DataTy {
                   dty: desc::DataTyKind::Range,
                   ..
               }) => panic!("Cannot generate type for ThreadHierchy or Range"),
    };

    if matches!(mutbl, desc::Mutability::Mut) {
        cu_ty
    } else {
        ast::Ty::Const(Box::new(cu_ty))
    }
}