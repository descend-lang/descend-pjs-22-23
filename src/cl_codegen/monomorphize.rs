use std::collections::{HashMap, HashSet, BTreeMap};

// use crate::ast::{*, visit::Visit};
use crate::{
    ast::{
        visit_mut::{walk_expr, VisitMut},
        *,
    }
};

pub fn monomorphize<'a>(compil_unit: &CompilUnit<'a>) -> CompilUnit<'a> {
    let mut visitor = MonomorphiseVisitor{
        existings_functions: compil_unit,
        monomophised_functions: CompilUnit{
            fun_defs: vec![],
            source: compil_unit.source.clone(),
        },
        functions_to_ignore: HashMap::new()
    };
    compil_unit.fun_defs.iter().for_each(|fun_def| {

        if fun_def.generic_params.len() == 0 {
            let mut fun_def = fun_def.clone();
            visitor.visit_fun_def(&mut fun_def);
            
            visitor.monomophised_functions.fun_defs.push(fun_def);
        }

    });

    compil_unit.clone()
}


struct MonomorphiseVisitor<'a, 'b> {
    existings_functions: &'b CompilUnit<'a>,
    monomophised_functions: CompilUnit<'a>,
    functions_to_ignore: HashMap<(String, Vec<ArgKinded>), String>
}

impl<'a, 'b> VisitMut for MonomorphiseVisitor<'a, 'b> {
    fn visit_expr(&mut self, expr: &mut Expr) { 

        if let ExprKind::App(name , generic_args, args) = &mut expr.expr {
            if generic_args.len() > 0 {
                match self.functions_to_ignore.get(&(get_fun_name_from_expr(name).clone(), generic_args.clone())) {
                    Some(new_name) => {
                        **name  = Expr::new(ExprKind::PlaceExpr(PlaceExpr::new(PlaceExprKind::Ident(Ident::new(&new_name)))));
                        generic_args.clear();
                    }
                    None => self.monomorphise_fun(get_fun_name_from_expr(name), &generic_args),
                } 
                
                let x = self.functions_to_ignore.get(&(get_fun_name_from_expr(name).clone(), generic_args.clone()));
                **name  = Expr::new(ExprKind::PlaceExpr(PlaceExpr::new(PlaceExprKind::Ident(Ident::new("test")))));

            }
        }
        walk_expr(self, expr) 
    }
}

impl <'a, 'b> MonomorphiseVisitor<'a, 'b> {
    fn monomorphise_fun(&mut self, fun_name: &String, fun_generic_args: &Vec<ArgKinded>)  {
        let old_fun = self.existings_functions.fun_defs.iter().find(| existing_fun_def | {
            existing_fun_def.name == *fun_name
        }).expect("function to monomorphize not found in context. This should not happen!");
        let new_function = FunDef{
            name: "test".to_string(),
            generic_params: vec![],
            param_decls:  , // TODO new params
            ret_dty: , // TODO new return
            exec: old_fun.exec.clone(),
            prv_rels: old_fun.prv_rels.clone(),
            body_expr: // TODO new body
        };


    }
    
}


struct NameGenerator {
    /// List of all generated names so far
    generated_names: HashSet<String>,
}

impl NameGenerator {
    /// List of all C++-Keywords which should be avoided as names
    const CPP_KEYWORDS: [&'static str; 101] = [
        "alignas",
        "alignof",
        "and",
        "and_eq",
        "asm",
        "atomic_cancel",
        "atomic_commit",
        "atomic_noexcept",
        "auto",
        "bitand",
        "bitor",
        "bool",
        "break",
        "case",
        "catch",
        "char",
        "char8_t",
        "char16_t",
        "char32_t",
        "class",
        "compl",
        "concept",
        "const",
        "consteval",
        "constexpr",
        "constinit",
        "const_cast",
        "continue",
        "co_await",
        "co_return",
        "co_yield",
        "decltype",
        "default",
        "delete",
        "do",
        "double",
        "dynamic_cast",
        "else",
        "enum",
        "explicit",
        "export",
        "extern",
        "false",
        "final",
        "float",
        "for",
        "friend",
        "goto",
        "if",
        "import",
        "inline",
        "int",
        "long",
        "module",
        "mutable",
        "namespace",
        "new",
        "noexcept",
        "not",
        "not_eq",
        "nullptr",
        "operator",
        "or",
        "or_eq",
        "override",
        "private",
        "protected",
        "public",
        "reflexpr",
        "register",
        "reinterpret_cast",
        "requires",
        "return",
        "short",
        "signed",
        "sizeof",
        "static",
        "static_assert",
        "static_cast",
        "struct",
        "switch",
        "synchronized",
        "template",
        "this",
        "thread_local",
        "throw",
        "true",
        "try",
        "typedef",
        "typeid",
        "typename",
        "union",
        "unsigned",
        "using",
        "virtual",
        "void",
        "volatile",
        "wchar_t",
        "while",
        "xor",
        "xor_eq",
    ];

    pub fn new() -> Self {
        NameGenerator {
            generated_names: HashSet::from_iter(
                NameGenerator::CPP_KEYWORDS
                    .iter()
                    .map(|str| String::from(*str)),
            ),
        }
    }

    /// Generate a new uniq function name
    /// * `function_name` - FunctionName of the function for which the name is generated
    /// * `arg_kinded` - vector of kinded arguments for the constraint kinded identifiers
    /// if this is monomorphised function
    pub fn generate_name(
        &mut self,
        function_name: &String,
        arg_kinded: Option<&Vec<ArgKinded>>,
    ) -> String {
        // original name of the function
        let name = &function_name.clone();
        // generate a string from for the ArgKinded if exists
        let arg_kinded = match arg_kinded {
            Some(args) => Some(args.iter().fold(String::new(), |res, arg| {
                format!("{}_{}", res, NameGenerator::arg_kinded_to_string(arg))
            })),
            None => None,
        };
        if let Some(arg_kinded) = arg_kinded {
            self.generate_name_internal(&format!("{}_{}", name, arg_kinded))
        } else {
            self.generate_name_internal(name)
        }
    }

    /// Returns the name if a function with the same name wasnt generated before.
    /// Else a counter is incremented and appended to the name until a name is reached
    /// which is not generated before
    fn generate_name_internal(&mut self, name: &String) -> String {
        let res =
            // Is this name generated before?
            if self.generated_names.get(name).is_none() {
                name.clone()
            }
            // Else append a counter to the function-name
            else {
                let mut counter = 2;
                let mut result = format!("{}_{}", name, counter);
                while self.generated_names.get(&result).is_some() {
                    counter += 1;
                    result = format!("{}_{}", name, counter)
                }
                result
            };
        // Do not generated the same name again
        self.generated_names.insert(res.clone());
        res
    }

    /// Transform a ArgKinded to a string, so it can be used in a function name
    fn arg_kinded_to_string(arg: &ArgKinded) -> String {
        match arg {
            ArgKinded::Ident(_) => panic!("There should be no idents without kinds"),
            ArgKinded::Nat(nat) => format!("{}", nat),
            ArgKinded::Memory(mem) => String::from(match mem {
                Memory::GpuGlobal => "GpuGlobal",
                Memory::CpuMem => "CpuMem",
                Memory::GpuShared => "GpuShared",
                Memory::GpuLocal => "GpuLocal",
                Memory::Ident(_) => todo!("This should not happen"),
            }),
            ArgKinded::Ty(ty) => NameGenerator::ty_to_string(ty),
            ArgKinded::DataTy(dty) => NameGenerator::dty_to_string(dty),
            ArgKinded::Provenance(prov) => format!("{}", prov),
        }
    }

    fn ty_to_string(ty: &Ty) -> String {
        match &ty.ty {
            TyKind::Data(dty) => NameGenerator::dty_to_string(dty),
            TyKind::Fn(_, _, _, _) => unimplemented!("needed?"),
            TyKind::Ident(_) => unimplemented!("needed?"),
            TyKind::Dead(_) => panic!("This should not contain dead types"),
        }
    }

    fn dty_to_string(dty: &DataTy) -> String {
        match &dty.dty {
            DataTyKind::Ident(i) => format!("{}", i.name),
            DataTyKind::Scalar(s) => String::from(match s {
                ScalarTy::Bool => "bool",
                ScalarTy::Unit => "Unit",
                ScalarTy::I32 => "i32",
                ScalarTy::U32 => "u32",
                ScalarTy::F32 => "f32",
                ScalarTy::F64 => "f64",
                ScalarTy::Gpu => "gpu",
            }),
            DataTyKind::Atomic(s) => format!(
                "Atomic_{}_",
                NameGenerator::dty_to_string(&DataTy::new(DataTyKind::Scalar(s.clone())))
            ),
            DataTyKind::Array(dty, nat) => {
                format!("Array_{}_{}_", NameGenerator::dty_to_string(dty), nat)
            }
            DataTyKind::ArrayShape(dty, nat) => {
                format!("ArrayShape_{}_{}_", NameGenerator::dty_to_string(dty), nat)
            }
            DataTyKind::Tuple(dtys) => format!(
                "{}_",
                dtys.iter().fold(String::from("Tuple"), |res, dty| format!(
                    "{}_{}",
                    res,
                    NameGenerator::dty_to_string(dty)
                ))
            ),
            DataTyKind::At(_, _) => todo!(),
            DataTyKind::Ref(_, _, _, dty) => format!("Ref_{}_", NameGenerator::dty_to_string(dty)),
            DataTyKind::ThreadHierchy(_) => todo!(),
            DataTyKind::SplitThreadHierchy(_, _) => todo!(),
            DataTyKind::RawPtr(dty) => format!("RawPtr_{}_", NameGenerator::dty_to_string(dty)),
            DataTyKind::Range => String::from("Range"),
            DataTyKind::Dead(_) => panic!("This should not contain dead types"),
        }
    }
}

fn get_fun_name_from_expr(fun: &Box<Expr>) -> &String {
    match &fun.expr {
        ExprKind::PlaceExpr(place_expr) => {
            if let PlaceExprKind::Ident(ident) = &place_expr.pl_expr {
                &ident.name
            } else {
                panic!("Dont know how to get function name")
            }
        }
        _ => panic!("Dont know how to get function name"),
    }
}
fn get_fun_name_from_expr_mut(fun: &mut Box<Expr>) -> &mut String {
    match &mut fun.expr {
        ExprKind::PlaceExpr(place_expr) => {
            if let PlaceExprKind::Ident(ident) = &mut place_expr.pl_expr {
                &mut ident.name
            } else {
                panic!("Dont know how to get function name")
            }
        }
        _ => panic!("Dont know how to get function name"),
    }
}