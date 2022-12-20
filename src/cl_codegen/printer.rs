// use crate::cpp_ast::Item;


use crate::cpp_ast::{
    BinOp, BufferKind, Item, Expr, ParamDecl, ScalarTy, Stmt, TemplParam, TemplateArg, Ty, UnOp,
};
use crate::cpp_ast::{GpuAddrSpace, Lit};
use core::panic;
use std::env;
use std::fmt::format;
use std::ops::Add;

pub(super) fn print(include_header: &Item, cpu_program: &[Item], gpu_program: &[Item]) -> String {
    use std::fmt::Write;

    let mut code = String::new();

    // print include_header first
    let res = writeln!(&mut code, "{}", include_header.print_cl(false));
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
        let res = writeln!(&mut code, "{}", i.print_cl(true));
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
        let res = writeln!(&mut code, "{}", i.print_cl(false));
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
    fn print_cl(&self, is_gpu_function: bool) -> String;
}

impl OpenCLPrint for Item {
    fn print_cl(&self, gpu_fun: bool) -> String {
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
                } else {
                    s.push_str(format!("{} {} (",ret_ty.print_cl(is_gpu_function.clone()), name).as_str());
                }
                if let Some(p) = fmt_vec(params, ", ", is_gpu_function.clone()) {
                    s.push_str(p.as_str())
                }
                s.push_str(") {\n");

                s.push_str(format!("{}", body.print_cl(is_gpu_function.clone())).as_str());
                s.push_str("\n}\n");
                s
            }
        }
    }
}
impl OpenCLPrint for Stmt {
    fn print_cl(&self, is_gpu_function: bool) -> String {
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
                    // let res = write!(&mut s, "{} ", addrs.print_cl(is_gpu_function));
                    s.push_str(format!("{} ", addrs.print_cl(is_gpu_function)).as_str())
                }
                s.push_str(format!("{} {}", ty.print_cl(is_gpu_function), name).as_str());
                if let Ty::CArray(_, n) = ty {
                    s.push_str(format!("[{n}]").as_str());
                }
                if let Some(expr) = expr {
                    s.push_str(format!(" = {}", expr).as_str());
                }
                s.push_str(";");
                s
            },
            Block(stmt) => format!("{{\n {} \n}}", stmt.print_cl(is_gpu_function)),
            Seq(stmt) => {
                let (last, leading) = stmt.split_last().unwrap();
                for stmt in leading {
                    s.push_str(format!("{}", stmt.print_cl(is_gpu_function)).as_str());
                }
                s.push_str(format!("{}", last.print_cl(is_gpu_function)).as_str());
                s
            },
            Expr(expr) => {
                if let crate::cpp_ast::Expr::Empty = expr {
                    String::new()
                } else {
                    format!("{};", expr.print_cl(is_gpu_function))
                }
            },
            If { cond, body } => {
                s.push_str(format!("if ({})", cond.print_cl(is_gpu_function)).as_str());
                s.push_str(format!("{}", body.print_cl(is_gpu_function)).as_str());
                s
            },
            IfElse {
                cond,
                true_body,
                false_body,
            } => {
                s.push_str(format!("if ({})", cond.print_cl(is_gpu_function)).as_str());
                s.push_str(
                    format!(
                        "{} else {}",
                        true_body.print_cl(is_gpu_function),
                        false_body.print_cl(is_gpu_function)
                    ).as_str()
                );
                s
            },
            While { cond, stmt } => format!(
                "while ({}) {}",
                cond.print_cl(is_gpu_function),
                stmt.print_cl(is_gpu_function)
            ),
            ForLoop {
                init,
                cond,
                iter,
                stmt,
            } => {
                format!(
                    "for ({} {}; {}) {}",
                    init.print_cl(is_gpu_function),
                    cond.print_cl(is_gpu_function),
                    iter.print_cl(is_gpu_function),
                    stmt.print_cl(is_gpu_function)
                )
            },
            Label(l) => format!("{}:", l),
            Return(expr) => {
                s.push_str("return ");
                if let Some(e) = expr {
                    s.push_str(format!(" {}", e.print_cl(is_gpu_function)).as_str());
                }
                s.push_str(";");
                s
            }
        }
        
    }
}


impl OpenCLPrint for Expr {
    fn print_cl(&self, is_gpu_function: bool) -> String {
        use Expr::*;
        let mut s = String::new();

        match  self {
            Empty => s,
            Ident(name) => format!("{name}"),
            Lit(l) => format!("{l}"),
            Assign {
                lhs: l_val,
                rhs: r_val
            } => format!("{} = {}", l_val.print_cl(is_gpu_function), r_val.print_cl(is_gpu_function)),
            Lambda {..} => panic!("There are not lamba functions in OpenCL"),
            FunCall {
                fun,
                template_args,
                args,
            } => {
                s.push_str(format!("{}", fun.print_cl(is_gpu_function)).as_str());
                if !template_args.is_empty() {
                    panic!("There are no template args for functions in OpenCL")
                }
                s.push_str("(");
                if let Some(a) = fmt_vec(args, ", ", is_gpu_function) {
                    s.push_str(&a);
                }
                s.push_str(")");
                s
            },
            UnOp { op, arg } => format!("{}{}", op.print_cl(is_gpu_function), arg.print_cl(is_gpu_function)),
            BinOp { op, lhs, rhs } 
                => format!(
                    "{} {} {}",
                    lhs.print_cl(is_gpu_function),
                    op.print_cl(is_gpu_function),
                    rhs.print_cl(is_gpu_function)
                ),
            ArraySubscript { array, index } => format!("{}[{}]", array.print_cl(is_gpu_function), index),
            Proj { tuple, n } => format!( "{}.{}", tuple.print_cl(is_gpu_function), n),
            InitializerList { elems } => {
                s.push_str("{{");
                if let Some(e) = fmt_vec(elems, ", ", is_gpu_function) {
                    s.push_str(e.as_str());
                }
                s.push_str("}}");
                s
            },
            Ref(expr) => format!("(&{})", expr.print_cl(is_gpu_function)),
            Deref(expr) => format!("(*{})", expr.print_cl(is_gpu_function)),
            Tuple(elems) => {
                s.push_str("descend::tuple{{");
                if let Some(e) = fmt_vec(elems, ", ", is_gpu_function) {
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
    fn print_cl(&self, is_gpu_function: bool) -> String {
        use Lit::*;

        match self {
            Bool(val) => {
                let v = if *val {"1"} else {"0"};
                format!("{v}")
            }// format!("{}", val), // TODO? print 0 or 1 on GPU depending on value?
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
    fn print_cl(&self, is_gpu_function: bool) -> String {
        format!(
            "{} {}",
            self.ty.print_cl(is_gpu_function),
            self.name
        )
    }
}

impl OpenCLPrint for UnOp {
    fn print_cl(&self, is_gpu_function: bool) -> String {
        match self {
            Self::Not => String::from("!"),
            Self::Neg => String::from("-")
        }
    }
}

impl OpenCLPrint for BinOp {
    fn print_cl(&self, is_gpu_function: bool) -> String {
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
    fn print_cl(&self, is_gpu_function: bool) -> String {
        use GpuAddrSpace::*;
        match self {
            Global => String::new(),
            Local => String::from("__local"),
            Constant => String::from("__constant")
        }
    }
}

impl OpenCLPrint for Ty {
    fn print_cl(&self, is_gpu_function: bool) -> String {
        use Ty::*;
        match self {
            Ptr(ty, Some(addr_space)) => format!(
                "{}, {} *",
                addr_space.print_cl(is_gpu_function),
                ty.print_cl(is_gpu_function)
            ),
            Ptr(ty, None) => format!("{} *", ty.print_cl(is_gpu_function)),
            PtrConst(ty, Some(addr_space)) => format!("{} const {} *", addr_space.print_cl(is_gpu_function), ty.print_cl(is_gpu_function)),
            PtrConst(ty, None) => format!("const {} *", ty.print_cl(is_gpu_function)),
            Const(ty) => match ty.as_ref() {
                Ptr(_, _) => format!("{} const", ty.print_cl(is_gpu_function)),
                PtrConst(_, _) => format!("{} const", ty.print_cl(is_gpu_function)),
                _ => format!("const {}", ty.print_cl(is_gpu_function)),
            },
            Array(ty, size) => format!("descend::array<{}, {}>", ty.print_cl(is_gpu_function), size),
            CArray(ty, _) => format!("{}", ty.print_cl(is_gpu_function)),
            Tuple(tys) => {
                let mut s = String::new();
                s.push_str("descend::tuple<");
                if let Some(t) = fmt_vec(tys, ", ", is_gpu_function) {
                    s.push_str(t.as_str());
                }
                s.push_str(">");
                s
            },
            Buffer(ty, buff_kind) => match buff_kind {
                BufferKind::CpuMem => format!("HeapBuffer<{}>", ty.print_cl(is_gpu_function)),
                BufferKind::GpuGlobal => format!("GpuBuffer<{}>", ty.print_cl(is_gpu_function)),
                BufferKind::Ident(name) => format!("{}", name)
            },
            Scalar(sty) => format!("{}", sty.print_cl(is_gpu_function)),
            Atomic(at) => format!("descend::Atomic<{}>", at.print_cl(is_gpu_function)),
            Ident(name) => format!("{}", name),
        }
    }
}

impl OpenCLPrint for ScalarTy {
    fn print_cl(&self, is_gpu_function: bool) -> String {
        use ScalarTy::*;

        match self {
            Auto => format!("auto"),
            Void => format!("void"),
            I32 => {
                if is_gpu_function {
                    format!("int")
                } else {
                    format!("descend::i32")
                }
            }, // format!("descend::i32"),
            U32 => {
                if is_gpu_function {
                    format!("unsigned int")
                } else {
                    format!("descend::u32")
                }
            } // format!("descend::u32"),
            F32 => {
                if is_gpu_function {
                    format!("float")
                } else {
                    format!("descend::f32")
                }
            }, // format!("descend::f32"),
            F64 => {
                if is_gpu_function {
                    format!("double")
                } else {
                    format!("descend::f64")
                }
            } // format!("descend::f64"),
            SizeT => format!("std::size_t"),
            Bool => {
                if is_gpu_function {
                    format!("char")
                } else {
                    format!("descend::bool")
                }
            }, //  format!("bool"),
            Memory => format!("descend::Memory"),
            Gpu => format!("descend::Gpu"),
        }
    }
}

fn fmt_vec<D: OpenCLPrint>(v: &[D], sep: &str, gpu_fun: bool) -> Option<String> {
    if let Some((last, leading)) = v.split_last() {
        let mut s = String::new();
        for p in leading {
            s.push_str(format!("{}{}", p.print_cl(gpu_fun), sep).as_str());
        }
        s.push_str(format!("{}", last.print_cl(gpu_fun)).as_str());
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
                        Some(GpuAddrSpace::Global),
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
                ty: Ty::Scalar(ScalarTy::I32),
                addr_space: None,
                expr: Some(Expr::Ident("a".to_string())),
            },
            is_gpu_function: true,
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
                        None
                    ))),
                },
                ParamDecl {
                    name: "b".to_string(),
                    ty: Ptr(Box::new(Scalar(ScalarTy::I32)), None),
                },
            ],
            ret_ty: Scalar(ScalarTy::I32),
            body: Stmt::VarDecl {
                name: "a_f".to_string(),
                ty: Ty::Scalar(ScalarTy::I32),
                addr_space: None,
                expr: Some(Expr::Ident("a".to_string())),
            },
            is_gpu_function: true,
        },
    ];

    let code = print(&include_header, &cpu_program, &gpu_program);
    print!("{}", code);
    Ok(())
}
