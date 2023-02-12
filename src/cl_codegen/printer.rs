use crate::cpp_ast::{
    BinOp, BufferKind, Expr, Item, ParamDecl, ScalarTy, Stmt, TemplateArg, Ty, UnOp,
};
use crate::cpp_ast::{GpuAddrSpace, Lit};
use core::panic;
use std::env;

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

    let mut kernel_program = "".to_string();

    // print kernel programm itself
    for i in gpu_program {
        let res = writeln!(&mut kernel_program, "{}", i.print_cl(true));
        if res.is_err() {
            panic!("{:?}", res);
        }
    }

    kernel_program = clang_format(&kernel_program);

    let res = writeln!(&mut code, "{kernel_program}");
    if res.is_err() {
        panic!("{:?}", res);
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
    fn print_cl(&self, is_dev_fun: bool) -> String;
}

impl OpenCLPrint for Item {
    fn print_cl(&self, gpu_fun: bool) -> String {
        match self {
            Item::Include(content) => format!("#include \"{content}\"\n"),
            Item::FunDef {
                name,
                templ_params,
                params,
                ret_ty,
                body,
                is_dev_fun,
                ..
            } => {
                use std::fmt::Write;
                let mut s = String::new();
                if !templ_params.is_empty() {
                    panic!("There are no template parameters in OpenCL");
                }
                if name.contains("__kernel") {
                    let res = write!(&mut s, "__kernel void {name} (");
                    if res.is_err() {
                        panic!("{:?}", res);
                    }
                } else {
                    write!(&mut s, "{} {name} (", ret_ty.print_cl(*is_dev_fun)).unwrap();
                }
                if let Some(p) = fmt_vec(params, ", ", *is_dev_fun, name.contains("__kernel")) {
                    write!(&mut s, "{p}").unwrap();
                }

                writeln!(&mut s, ") {{").unwrap();
                writeln!(&mut s, "{}", body.print_cl(*is_dev_fun)).unwrap();
                writeln!(&mut s, "}}").unwrap();
                s
            }
        }
    }
}
impl OpenCLPrint for Stmt {
    fn print_cl(&self, is_dev_fun: bool) -> String {
        use std::fmt::Write;
        use Stmt::*;

        let mut s = String::new();
        match self {
            Skip => String::new(),
            VarDecl {
                name,
                ty,
                addr_space,
                expr,
            } => {
                if let Some(addrs) = addr_space {
                    write!(&mut s, "{} ", addrs.print_cl(is_dev_fun)).unwrap();
                }
                s.push_str(format!("{} {}", ty.print_cl(is_dev_fun), name).as_str());
                if let Ty::CArray(_, n) = ty {
                    write!(&mut s, "[{n}]").unwrap();
                }
                if let Some(expr) = expr {
                    write!(&mut s, " = {expr}").unwrap();
                }
                write!(&mut s, ";").unwrap();
                s
            }
            Block(stmt) => format!("{{\n {} \n}}", stmt.print_cl(is_dev_fun)),
            Seq(stmt) => {
                let (last, leading) = stmt.split_last().unwrap();
                for stmt in leading {
                    write!(&mut s, "{}", stmt.print_cl(is_dev_fun)).unwrap();
                }
                write!(&mut s, "{}", last.print_cl(is_dev_fun)).unwrap();
                s
            }
            Expr(expr) => {
                if let crate::cpp_ast::Expr::Empty = expr {
                    String::new()
                } else {
                    format!("{};", expr.print_cl(is_dev_fun))
                }
            }
            If { cond, body } => {
                writeln!(&mut s, "if ({})", cond.print_cl(is_dev_fun)).unwrap();
                write!(&mut s, "{}", body.print_cl(is_dev_fun)).unwrap();
                s
            }
            IfElse {
                cond,
                true_body,
                false_body,
            } => {
                // s.push_str(format!("if ({})", cond.print_cl(is_dev_fun)).as_str());
                writeln!(&mut s, "if ({})", cond.print_cl(is_dev_fun)).unwrap();
                write!(
                    &mut s,
                    "{} else {}",
                    true_body.print_cl(is_dev_fun),
                    false_body.print_cl(is_dev_fun)
                )
                .unwrap();
                s
            }
            While { cond, stmt } => format!(
                "while ({}) {}",
                cond.print_cl(is_dev_fun),
                stmt.print_cl(is_dev_fun)
            ),
            ForLoop {
                init,
                cond,
                iter,
                stmt,
            } => {
                format!(
                    "for ({} {}; {}) {}",
                    init.print_cl(is_dev_fun),
                    cond.print_cl(is_dev_fun),
                    iter.print_cl(is_dev_fun),
                    stmt.print_cl(is_dev_fun)
                )
            }
            Label(l) => format!("{l}:"),
            Return(expr) => {
                write!(&mut s, "return").unwrap();
                if let Some(e) = expr {
                    write!(&mut s, " {}", e.print_cl(is_dev_fun)).unwrap();
                }
                writeln!(&mut s, ";").unwrap();
                s
            }
        }
    }
}

impl OpenCLPrint for TemplateArg {
    fn print_cl(&self, is_dev_fun: bool) -> String {
        match self {
            TemplateArg::Expr(expr) => {
                format!("{expr}")
            }
            TemplateArg::Ty(ty) => {
                format!("{ty}")
            }
        }
    }
}

impl OpenCLPrint for Expr {
    fn print_cl(&self, is_dev_fun: bool) -> String {
        use std::fmt::Write;
        use Expr::*;
        let mut s = String::new();

        match self {
            Empty => s,
            Ident(name) => name.to_string(),
            Lit(l) => l.to_string(),
            Assign {
                lhs: l_val,
                rhs: r_val,
            } => format!(
                "{} = {}",
                l_val.print_cl(is_dev_fun),
                r_val.print_cl(is_dev_fun)
            ),
            Lambda { .. } => panic!("There are not lamba functions in OpenCL"),
            FunCall {
                fun,
                template_args,
                args,
            } => {
                write!(&mut s, "{}", fun.print_cl(is_dev_fun).as_str()).unwrap();
                if !template_args.is_empty() {
                    write!(&mut s, "<").unwrap();
                    if let Some(a) = fmt_vec(template_args, ", ", is_dev_fun, false) {
                        write!(&mut s, "{a}").unwrap();
                    }
                    write!(&mut s, ">").unwrap();
                }
                write!(&mut s, "(").unwrap();
                if let Some(a) = fmt_vec(args, ", ", is_dev_fun, false) {
                    write!(&mut s, "{a}").unwrap();
                }
                write!(&mut s, ")").unwrap();
                s
            }
            UnOp { op, arg } => format!("{}{}", op.print_cl(is_dev_fun), arg.print_cl(is_dev_fun)),
            BinOp { op, lhs, rhs } => format!(
                "{} {} {}",
                lhs.print_cl(is_dev_fun),
                op.print_cl(is_dev_fun),
                rhs.print_cl(is_dev_fun)
            ),
            ArraySubscript { array, index } => format!("{}[{}]", array.print_cl(is_dev_fun), index),
            Proj { tuple, n } => format!("{}.{}", tuple.print_cl(is_dev_fun), n),
            InitializerList { elems } => {
                write!(&mut s, "{{").unwrap();
                if let Some(e) = fmt_vec(elems, ", ", is_dev_fun, false) {
                    write!(&mut s, "{e}").unwrap();
                }
                write!(&mut s, "}}").unwrap();
                s
            }
            Ref(expr) => format!("(&{})", expr.print_cl(is_dev_fun)),
            Deref(expr) => format!("(*{})", expr.print_cl(is_dev_fun)),
            Tuple(elems) => {
                // TODO! only on host code
                write!(&mut s, "descend::tuple{{").unwrap();
                if let Some(e) = fmt_vec(elems, ", ", is_dev_fun, false) {
                    write!(&mut s, "{e}").unwrap();
                }
                write!(&mut s, "}}").unwrap();
                s
            }
            Nat(n) => format!("{n}"),
        }
    }
}

impl OpenCLPrint for Lit {
    fn print_cl(&self, is_dev_fun: bool) -> String {
        use Lit::*;

        match self {
            Bool(val) => {
                if is_dev_fun {
                    let v = if *val { "1" } else { "0" };
                    v.to_string()
                } else {
                    format!("{val}")
                }
            } // TODO? print 0 or 1 on GPU depending on value?
            I32(val) => val.to_string(),
            U32(val) => val.to_string(),
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
            String(string) => {
                format!("\"{string}\"")
            }
        }
    }
}

impl OpenCLPrint for ParamDecl {
    fn print_cl(&self, is_dev_fun: bool) -> String {
        format!("{} {}", self.ty.print_cl(is_dev_fun), self.name)
    }
}

impl OpenCLPrint for UnOp {
    fn print_cl(&self, is_dev_fun: bool) -> String {
        match self {
            Self::Not => String::from("!"),
            Self::Neg => String::from("-"),
        }
    }
}

impl OpenCLPrint for BinOp {
    fn print_cl(&self, is_dev_fun: bool) -> String {
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
            Self::Neq => "!=",
        };
        String::from(str)
    }
}

impl OpenCLPrint for GpuAddrSpace {
    fn print_cl(&self, is_dev_fun: bool) -> String {
        match self {
            GpuAddrSpace::Global => String::new(),
            GpuAddrSpace::Shared => String::from("__local"),
            GpuAddrSpace::Constant => String::from("__constant"),
        }
    }
}

impl OpenCLPrint for Ty {
    fn print_cl(&self, is_dev_fun: bool) -> String {
        use std::fmt::Write;
        use Ty::*;
        match self {
            Ptr(ty, Some(addr_space)) => format!(
                "{}, {} *",
                addr_space.print_cl(is_dev_fun),
                ty.print_cl(is_dev_fun)
            ),
            Ptr(ty, None) => format!("{} *", ty.print_cl(is_dev_fun)),
            PtrConst(ty, Some(addr_space)) => format!(
                "{} const {} *",
                addr_space.print_cl(is_dev_fun),
                ty.print_cl(is_dev_fun)
            ),
            PtrConst(ty, None) => format!("const {} *", ty.print_cl(is_dev_fun)),
            Const(ty) => match ty.as_ref() {
                Ptr(_, _) => format!("{} const", ty.print_cl(is_dev_fun)),
                PtrConst(_, _) => format!("{} const", ty.print_cl(is_dev_fun)),
                _ => format!("const {}", ty.print_cl(is_dev_fun)),
            },
            Array(ty, size) => format!("descend::array<{}, {}>", ty.print_cl(is_dev_fun), size),
            CArray(ty, _) => ty.print_cl(is_dev_fun),
            Tuple(tys) => {
                let mut s = String::new();
                write!(&mut s, "descend::tuple<").unwrap();
                if let Some(t) = fmt_vec(tys, ", ", is_dev_fun, false) {
                    write!(&mut s, "{t}").unwrap();
                }
                write!(&mut s, ">").unwrap();
                s
            }
            Buffer(ty, buff_kind) => match buff_kind {
                BufferKind::CpuMem => format!("descend::HeapBuffer<{}>", ty.print_cl(is_dev_fun)),
                BufferKind::GpuGlobal => format!("descend::GpuBuffer<{}>", ty.print_cl(is_dev_fun)),
                BufferKind::Ident(name) => name.to_string(),
            },
            Scalar(sty) => sty.print_cl(is_dev_fun),
            Atomic(at) => format!("descend::Atomic<{}>", at.print_cl(is_dev_fun)),
            Ident(name) => name.to_string(),
        }
    }
}

impl OpenCLPrint for ScalarTy {
    fn print_cl(&self, is_dev_fun: bool) -> String {
        use ScalarTy::*;

        match self {
            Auto => "auto",
            Void => "void",
            I32 => {
                if is_dev_fun {
                    "int"
                } else {
                    "descend::i32"
                }
            } // format!("descend::i32"),
            U32 => {
                if is_dev_fun {
                    "unsigned int"
                } else {
                    "descend::u32"
                }
            } // format!("descend::u32"),
            F32 => {
                if is_dev_fun {
                    "float"
                } else {
                    "descend::f32"
                }
            } // format!("descend::f32"),
            F64 => {
                if is_dev_fun {
                    "double"
                } else {
                    "descend::f64"
                }
            } // format!("descend::f64"),
            SizeT => "size_t",
            Bool => {
                if is_dev_fun {
                    "char"
                } else {
                    "descend::bool"
                }
            } //  format!("bool"),
            Memory => "descend::Memory",
            Gpu => "descend::Gpu",
        }
        .to_string()
    }
}

fn fmt_vec<D: OpenCLPrint>(
    v: &[D],
    sep: &str,
    gpu_fun: bool,
    kernel_header: bool,
) -> Option<String> {
    use std::fmt::Write;
    if let Some((last, leading)) = v.split_last() {
        let mut s = String::new();
        let prefix = {
            if kernel_header {
                "__global "
            } else {
                ""
            }
        };
        for p in leading {
            write!(&mut s, "{prefix}{}{}", p.print_cl(gpu_fun), sep).unwrap();
        }
        write!(&mut s, "{prefix}{}", last.print_cl(gpu_fun)).unwrap();
        Some(s)
    } else {
        None
    }
}

#[test]
fn test_print_program() -> std::fmt::Result {
    use Ty::*;

    let include_header = Item::Include("descend.hpp".to_string());

    let cpu_program = vec![Item::FunDef {
        name: "test_host_fun".to_string(),
        templ_params: vec![],
        params: vec![
            ParamDecl {
                name: "a".to_string(),
                ty: Const(Box::new(PtrConst(
                    Box::new(Scalar(ScalarTy::I32)),
                    Some(GpuAddrSpace::Shared),
                ))),
            },
            ParamDecl {
                name: "b".to_string(),
                ty: Ptr(Box::new(Scalar(ScalarTy::Bool)), None),
            },
        ],
        ret_ty: Scalar(ScalarTy::Void),
        body: Stmt::VarDecl {
            name: "a_f".to_string(),
            ty: Ty::Scalar(ScalarTy::Auto),
            addr_space: None,
            expr: Some(Expr::Ident("a".to_string())),
        },
        is_dev_fun: false,
    }];

    let gpu_program = vec![
        Item::FunDef {
            name: "__kernel__".to_string(),
            templ_params: vec![],
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
            is_dev_fun: true,
        },
        Item::FunDef {
            name: "test_gpu_fun".to_string(),
            templ_params: vec![],
            params: vec![
                ParamDecl {
                    name: "a".to_string(),
                    ty: Const(Box::new(PtrConst(Box::new(Scalar(ScalarTy::I32)), None))),
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
            is_dev_fun: true,
        },
    ];

    let code = print(&include_header, &cpu_program, &gpu_program);
    print!("{code}");
    Ok(())
}

