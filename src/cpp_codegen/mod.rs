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