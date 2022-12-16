// use crate::cpp_ast::Item;


use crate::cpp_ast::{
    BinOp, BufferKind, Item, Expr, ParamDecl, ScalarTy, Stmt, TemplParam, TemplateArg, Ty, UnOp,
};
use crate::cpp_ast::{GpuAddrSpace, Lit};
use std::env;
use std::ops::Add;

pub(super) fn print(include_header: &Item, cpu_program: &[Item], gpu_program: &[Item]) -> String {
    use std::fmt::Write;

    let mut code = String::new();

    // print include_header first
    let res = writeln!(&mut code, "{}", include_header.print_cl());
    if res.is_err() {
        panic!("{:?}", res);
    }

    // print first part of raw string (for kernel programm)
    let res = writeln!(&mut code, "std::string kernel = R\"(");
    if res.is_err() {
        panic!("{:?}", res);
    }

    // print kernel programm itself
    for i in gpu_program {
        let res = writeln!(&mut code, "{}", i.print_cl());
        if res.is_err() {
            panic!("{:?}", res);
        }
    }

    // print end of raw string for kernel programm
    let res = writeln!(&mut code, ")\";");
    if res.is_err() {
        panic!("{:?}", res);
    }

    for i in cpu_program {
        let res = writeln!(&mut code, "{}", i.print_cl());
        if res.is_err() {
            panic!("{:?}", res);
        }
    }

    clang_format(&code)
}

fn clang_format(code: &str) -> String {
    //If clang-format is not available for user, it's path can be set in this env Variable (e.g. in .cargo/config.toml)
    let clang_format_path = match env::var("CLANG_FORMAT_PATH") {
        Ok(path) => path,
        Err(_) => String::from("clang-format"),
    };

    use std::io::Write;
    use std::process::{Command, Stdio};
    let mut clang_fmt_cmd = Command::new(clang_format_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to execute `clang-format`");
    if let Some(mut stdin) = clang_fmt_cmd.stdin.take() {
        stdin
            .write_all(code.as_bytes())
            .expect("Could not write to standard input stream.")
    }
    let clang_fmt_output = clang_fmt_cmd
        .wait_with_output()
        .expect("failed to execute `clang-format`");
    String::from_utf8(clang_fmt_output.stdout).expect("cannot read clang-format output as String")
}

trait OpenCLPrint {
    fn print_cl(&self) -> String;
}

impl OpenCLPrint for Item {
    fn print_cl(&self) -> String {
        match self {
            Item::Include { content, .. } => format!("#include {content}\n"),
            Item::FunDef {
                name,
                templ_params,
                params,
                ret_ty,
                body,
                is_gpu_function,
                ..
            } => {
                use std::fmt::Write;
                let mut s = String::new();
                if !templ_params.is_empty() {
                    panic!("There are no template parameters in OpenCL");
                }
                if name == "__kernel__" {
                    s.push_str(format!("__kernel void {} (", name).as_str());
                }
                if let Some(p) = fmt_vec_str(params, ",") {
                    s.push_str(p.as_str())
                }
                s.push_str(") {{\n");
                let res = writeln!(&mut s, "{}", body.print_cl());
                if res.is_err() {
                    panic!("{:?}", res);
                }
                s.push_str("}} \n");
                s
            }
        }
    }
}
impl OpenCLPrint for Stmt {
    fn print_cl(&self) -> String {
        use Stmt::*;
        use std::fmt::Write;
        
        let mut s = String::new();
        match self {
            Skip => String::new(),
            VarDecl {
                name,
                ty,
                addr_space,
                expr
            } => {
                if let Some(addrs) = addr_space {
                    let res = write!(&mut s, "{} ", addrs.print_cl());
                }
                s.push_str("{ty} {name}");
                if let Ty::CArray(_, n) = ty {
                    s.push_str(format!("[{n}]").as_str());
                }
                if let Some(expr) = expr {
                    s.push_str(" = {expr}");
                }
                s.push_str(";");
                s
            },
            Block(stmt) => format!("{{\n {stmt} \n}}"),
            Seq(stmt) => {
                let (last, leading) = stmt.split_last().unwrap();
                for stmt in leading {
                    let res = writeln!(s, "{}", stmt.print_cl());
                    if res.is_err() {
                        panic!("{:?}", res);
                    }
                }
                let res = write!(s, "{}", last);
                if res.is_err() {
                    panic!("{:?}", res);
                }
                s
            },
            Expr(expr) => {
                if let crate::cpp_ast::Expr::Empty = expr {
                    String::new()
                } else {
                    format!("{};", expr.print_cl())
                }
            },
            If { cond, body } => {
                s.push_str(format!("if ({})", cond.print_cl()).as_str());
                s.push_str(format!("{}", body.print_cl()).as_str());
                s
            },
            IfElse {
                cond,
                true_body,
                false_body,
            } => {
                s.push_str(format!("if ({})", cond.print_cl()).as_str());
                s.push_str(
                    format!(
                        "{} else {}",
                        true_body.print_cl(),
                        false_body.print_cl()
                    ).as_str()
                );
                s
            },
            While { cond, stmt } => format!("while ({}) {}", cond.print_cl(), stmt.print_cl()),
            ForLoop {
                init,
                cond,
                iter,
                stmt,
            } => {
                format!(
                    "for ({} {}; {}) {}",
                    init.print_cl(),
                    cond.print_cl(),
                    iter.print_cl(),
                    stmt.print_cl()
                )
            },
            Label(l) => format!("{}:", l),
            Return(expr) => {
                s.push_str("return ");
                if let Some(e) = expr {
                    s.push_str(format!(" {e}").as_str());
                }
                s.push_str(";");
                s
            }
        }
        
    }
}


impl OpenCLPrint for Expr {
    fn print_cl(&self) -> String {
        use Expr::*;
        let s = String::new();

        match  self {
            Empty => String::new(),
            Ident(name) => format!("{name}"),
            Lit(l) => format!("{l}"),
            Assign {
                lhs: l_val,
                rhs: r_val
            } => format!("{} = {}", l_val.print_cl(), r_val.print_cl()),
            Lambda {..} => panic!("There are not lamba functions in OpenCL"),
            FunCall {
                fun,
                template_args,
                args,
            } => {
                s.push_str(format!("{}", fun.print_cl()).as_str());
                if !template_args.is_empty() {
                    panic!("There are no template args for functions in OpenCL")
                }
                s.push_str("(");
                if let Some(a) = fmt_vec(args, ", ") {
                    s.push_str(&a);
                }
                s.push_str(")");
                s
            },
            UnOp { op, arg } => format!("{}{}", op.print_cl(), arg.print_cl()),
            BinOp { op, lhs, rhs } 
                => format!(
                    "{} {} {}",
                    lhs.print_cl(),
                    op.print_cl(),
                    rhs.print_cl()
                ),
            ArraySubscript { array, index } => format!("{}[{}]", array.print_cl(), index),
            Proj { tuple, n } => format!( "{}.{}", tuple.print_cl(), n),
            InitializerList { elems } => {
                s.push_str("{{");
                if let Some(e) = fmt_vec(elems, ", ") {
                    s.push_str(e.as_str());
                }
                s.push_str("}}");
                s
            },
            Ref(expr) => format!("(&{})", expr.print_cl()),
            Deref(expr) => format!("(*{})", expr.print_cl()),
            Tuple(elems) => {
                s.push_str("descend::tuple{{");
                if let Some(e) = fmt_vec(elems, ", ") {
                    s.push_str(e.as_str());
                }
                s.push_str("}}");
                s
            },
            Nat(n) => format!("{n}")
        }
    }
}

impl OpenCLPrint for Lit {
    fn print_cl(&self) -> String {
        use Lit::*;

        match self {
            Bool(val) => format!("{}", val),
            I32(val) => format!("{}", val),
            U32(val) => format!("{}", val),
            F32(f) => {
                if &f.ceil() == f {
                    format!("{f}.0f")
                } else {
                    format!("{f}")
                }
            }
            F64(d) => {
                if &d.ceil() == d {
                    format!("{d}.0")
                } else {
                    format!("{d}")
                }
            }
        }
    }
}

impl OpenCLPrint for ParamDecl {
    fn print_cl(&self) -> String {
        format!(
            "{} {}",
            self.ty.print_cl(),
            self.name
        )
    }
}

impl OpenCLPrint for UnOp {
    fn print_cl(&self) -> String {
        match self {
            Self::Not => String::from("!"),
            Self::Neg => String::from("-")
        }
    }
}

impl OpenCLPrint for BinOp {
    fn print_cl(&self) -> String {
        let str = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::And => "&&",
            Self::Or => "||",
            Self::Eq => "==",
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Gt => ">",
            Self::Ge => ">=",
            Self::Neq => "!="
        };
        String::from(str)
    }
}

impl OpenCLPrint for GpuAddrSpace {
    fn print_cl(&self) -> String {
        use GpuAddrSpace::*;
        match self {
            Global => String::new(),
            Local => String::from("__local"),
            Constant => String::from("__constant")
        }
    }
}

impl OpenCLPrint for Ty {
    fn print_cl(&self) -> String {
        use Ty::*;
        match self {
            Ptr(ty, Some(addr_space)) => format!("{}, {} *", addr_space.print_cl(), ty.print_cl()),
            Ptr(ty, None) => format!("{} *", ty.print_cl()),
            PtrConst(ty, Some(addr_space)) => format!("{} const {} *", addr_space.print_cl(), ty.print_cl()),
            PtrConst(ty, None) => format!("const {} *", ty.print_cl()),
            Const(ty) => match ty.as_ref() {
                Ptr(_, _) => format!("{} const", ty.print_cl()),
                PtrConst(_, _) => format!("{} const", ty.print_cl()),
                _ => format!("const {}", ty.print_cl()),
            },
            Array(ty, size) => format!("descend::array<{}, {}>", ty.print_cl(), size),
            CArray(ty, _) => format!("{}", ty.print_cl()),
            Tuple(tys) => {
                let s = String::new();
                s.push_str("descend::tuple<");
                if let Some(t) = fmt_vec(tys, ", ") {
                    s.push_str(t.as_str());
                }
                s.push_str(">");
                s
            },
            Buffer(ty, buff_kind) => match buff_kind {
                BufferKind::CpuMem => format!("HeapBuffer<{}>", ty.print_cl()),
                BufferKind::GpuGlobal => format!("GpuBuffer<{}>", ty.print_cl()),
                BufferKind::Ident(name) => format!("{}", name)
            },
            Scalar(sty) => format!("{}", sty.print_cl()),
            Atomic(at) => format!("descend::Atomic<{}>", at.print_cl()),
            Ident(name) => format!("{}", name),
        }
    }
}

impl OpenCLPrint for ScalarTy {
    fn print_cl(&self) -> String {
        use ScalarTy::*;

        match self {
            Auto => format!("auto"),
            Void => format!("void"),
            I32 => format!("descend::i32"),
            U32 => format!("descend::u32"),
            F32 => format!("descend::f32"),
            F64 => format!("descend::f64"),
            SizeT => format!("std::size_t"),
            Bool => format!("bool"),
            Memory => format!("descend::Memory"),
            Gpu => format!("descend::Gpu"),
        }
    }
}

fn fmt_vec<D: OpenCLPrint>(v: &[D], sep: &str) -> Option<String> {
    if let Some((last, leading)) = v.split_last() {
        let mut s = String::new();
        for p in leading {
            s.push_str(format!("{}{}", p.print_cl(), sep).as_str());
        }
        s.push_str(format!("{}", last.print_cl()).as_str());
        Some(s)
    } else {
        None
    }
}

fn fmt_vec_str<D: OpenCLPrint>(v: &[D], sep: &str) -> Option<String> {
    if let Some((last, leading)) = v.split_last() {
        let mut s = String::new();
        for p in leading {
            s.push_str(format!("{}{}", p.print_cl(), sep).as_str());
            
        }
        s.push_str(format!("{}", last.print_cl()).as_str());
        Some(s)
    } else {
        None
    }
}

#[test]
fn test_print_program() -> std::fmt::Result {
    use Ty::*;

    let include_header = Item::Include{
        name: "header".to_string(),
        content: "descend.hpp".to_string()
    };

    let cpu_program = vec![
        Item::FunDef {
            name: "test_host_fun".to_string(),
            templ_params: vec![],
            templ_values: vec![],
            params: vec![
                ParamDecl {
                    name: "a".to_string(),
                    ty: Const(Box::new(PtrConst(
                        Box::new(Scalar(ScalarTy::I32)),
                        Some(GpuAddrSpace::Local),
                    ))),
                },
                ParamDecl {
                    name: "b".to_string(),
                    ty: Ptr(Box::new(Scalar(ScalarTy::I32)), None),
                },
            ],
            ret_ty: Scalar(ScalarTy::Void),
            body: Stmt::VarDecl {
                name: "a_f".to_string(),
                ty: Ty::Scalar(ScalarTy::Auto),
                addr_space: None,
                expr: Some(Expr::Ident("a".to_string())),
            },
            is_gpu_function: false,
        },
    ];

    let gpu_program = vec![
        Item::FunDef {
            name: "__kernel__".to_string(),
            templ_params: vec![],
            templ_values: vec![],
            params: vec![
                ParamDecl {
                    name: "a".to_string(),
                    ty: Const(Box::new(PtrConst(
                        Box::new(Scalar(ScalarTy::I32)),
                        Some(GpuAddrSpace::Local),
                    ))),
                },
                ParamDecl {
                    name: "b".to_string(),
                    ty: Ptr(Box::new(Scalar(ScalarTy::I32)), None),
                },
            ],
            ret_ty: Scalar(ScalarTy::Void),
            body: Stmt::VarDecl {
                name: "a_f".to_string(),
                ty: Ty::Scalar(ScalarTy::Auto),
                addr_space: None,
                expr: Some(Expr::Ident("a".to_string())),
            },
            is_gpu_function: false,
        },
        Item::FunDef {
            name: "test_gpu_fun".to_string(),
            templ_params: vec![],
            templ_values: vec![],
            params: vec![
                ParamDecl {
                    name: "a".to_string(),
                    ty: Const(Box::new(PtrConst(
                        Box::new(Scalar(ScalarTy::I32)),
                        Some(GpuAddrSpace::Local),
                    ))),
                },
                ParamDecl {
                    name: "b".to_string(),
                    ty: Ptr(Box::new(Scalar(ScalarTy::I32)), None),
                },
            ],
            ret_ty: Scalar(ScalarTy::Void),
            body: Stmt::VarDecl {
                name: "a_f".to_string(),
                ty: Ty::Scalar(ScalarTy::Auto),
                addr_space: None,
                expr: Some(Expr::Ident("a".to_string())),
            },
            is_gpu_function: false,
        },
    ];

    let code = print(&include_header, &cpu_program, &gpu_program);
    print!("{}", code);
    Ok(())
}
