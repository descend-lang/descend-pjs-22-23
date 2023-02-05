use crate::c_ast::*;
use crate::cpp_ast::{TemplateArg, TemplParam};

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Include(content) => write!(f, "#include \"{}\"", content),
            Item::FunDef {
                name,
                params,
                ret_ty,
                body,
                is_dev_fun,
            } => {
                writeln!(
                    f,
                    "{}auto {}(",
                    if *is_dev_fun { "__device__ " } else { "" },
                    name
                )?;
                fmt_vec(f, params, ",\n")?;
                writeln!(f, "\n) -> {} {{", ret_ty)?;

                write!(f, "{}", body)?;
                writeln!(f, "\n}}")
            }
        }
    }
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Stmt::*;
        match self {
            Skip => Ok(()),
            VarDecl {
                name,
                ty,
                addr_space,
                expr,
            } => {
                if let Some(addrs) = addr_space {
                    write!(f, "{} ", addrs)?;
                }
                write!(f, "{} {}", ty, name)?;
                if let Ty::CArray(_, n) = ty {
                    write!(f, "[{}]", n)?;
                }
                if let Some(expr) = expr {
                    write!(f, " = {}", expr)?;
                }
                write!(f, ";")
            }
            Block(stmt) => {
                writeln!(f, "{{")?;
                writeln!(f, "{}", stmt)?;
                write!(f, "}}")
            }
            Seq(stmt) => {
                let (last, leading) = stmt.split_last().unwrap();
                for stmt in leading {
                    writeln!(f, "{}", stmt)?;
                }
                write!(f, "{}", last)
            }
            Expr(expr) => {
                if let crate::c_ast::Expr::Empty = expr {
                    Ok(())
                } else {
                    write!(f, "{};", expr)
                }
            }
            If { cond, body } => {
                writeln!(f, "if ({})", cond)?;
                write!(f, "{}", body)
            }
            IfElse {
                cond,
                true_body,
                false_body,
            } => {
                write!(f, "if ({}) ", cond)?;
                write!(f, "{} else {}", true_body, false_body)
            }
            While { cond, stmt } => {
                writeln!(f, "while ({})", cond)?;
                write!(f, "{}", stmt)
            }
            ForLoop {
                init,
                cond,
                iter,
                stmt,
            } => write!(f, "for ({} {}; {}) {}", init, cond, iter, stmt),
            Label(l) => write!(f, "{}:", l),
            Return(expr) => {
                write!(f, "return")?;
                if let Some(e) = expr {
                    write!(f, " {}", e)?;
                }
                write!(f, ";")
            }
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Expr::*;
        match self {
            Empty => Ok(()),
            Ident(name) => write!(f, "{}", name),
            Lit(l) => write!(f, "{}", l),
            Assign {
                lhs: l_val,
                rhs: r_val,
            } => write!(f, "{} = {}", l_val, r_val),
            FunCall {
                fun,
                args,
            } => {
                write!(f, "{}", fun)?;
                write!(f, "(")?;
                fmt_vec(f, args, ", ")?;
                write!(f, ")")
            }
            UnOp { op, arg } => write!(f, "{}{}", op, arg),
            BinOp { op, lhs, rhs } => write!(f, "{} {} {}", lhs, op, rhs),
            ArraySubscript { array, index } => write!(f, "{}[{}]", array, index),
            Proj { tuple, n } => write!(f, "{}.{}", tuple, n),
            InitializerList { elems } => {
                write!(f, "{{")?;
                fmt_vec(f, elems, ", ")?;
                write!(f, "}}")
            }
            Ref(expr) => write!(f, "(&{})", expr),
            Deref(expr) => write!(f, "(*{})", expr),
            Tuple(elems) => {
                write!(f, "descend::tuple{{")?;
                fmt_vec(f, elems, ", ")?;
                write!(f, "}}")
            }
            Nat(n) => write!(f, "{}", n),
        }
    }
}

impl std::fmt::Display for Lit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Bool(b) => write!(f, "{}", b),
            Lit::I32(i) => write!(f, "{}", i),
            Lit::U32(u) => write!(f, "{}", u),
            Lit::F32(fl) => {
                // This is supposed to be a strict comparison. It is equal if fl is an integer.
                if &fl.ceil() == fl {
                    write!(f, "{}.0f", fl)
                } else {
                    write!(f, "{}", fl)
                }
            }
            Lit::F64(d) => {
                if &d.ceil() == d {
                    write!(f, "{}.0", d)
                } else {
                    write!(f, "{}", d)
                }
            }
        }
    }
}

impl std::fmt::Display for ParamDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.ty, self.name)
    }
}


impl std::fmt::Display for UnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Not => write!(f, "!"),
            Self::Neg => write!(f, "-"),
        }
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
        write!(f, "{}", str)
    }
}

impl std::fmt::Display for GpuAddrSpace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GpuAddrSpace::Global => write!(f, ""),
            GpuAddrSpace::Shared => write!(f, "__shared__"),
            GpuAddrSpace::Constant => write!(f, "__constant__"),
        }
    }
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Ty::*;
        match self {
            // TODO print __restrict__
            Ptr(ty, Some(addr_space)) => write!(f, "{} {} *", addr_space, ty),
            Ptr(ty, None) => write!(f, "{} *", ty),
            PtrConst(ty, Some(addr_space)) => write!(f, "{} const {} *", addr_space, ty),
            PtrConst(ty, None) => write!(f, "const {} *", ty),
            Const(ty) => match ty.as_ref() {
                Ptr(_, _) => write!(f, "{} const", ty),
                PtrConst(_, _) => write!(f, "{} const", ty),
                _ => write!(f, "const {}", ty),
            },
            Array(ty, size) => write!(f, "descend::array<{}, {}>", ty, size),
            // Does not print the entire type because that would be impossible since C arrays
            //  are declared by appending the size to a variable.
            CArray(ty, _) => write!(f, "{}", ty),
            Tuple(tys) => {
                write!(f, "descend::tuple<")?;
                fmt_vec(f, tys, ", ")?;
                write!(f, ">")
            }
            Buffer(ty, buff_kind) => match buff_kind {
                BufferKind::CpuMem => write!(f, "HeapBuffer<{}>", ty),
                BufferKind::GpuGlobal => write!(f, "GpuBuffer<{}>", ty),
                BufferKind::Ident(name) => write!(f, "{}", name),
            },
            Scalar(sty) => write!(f, "{}", sty),
            Atomic(at) => write!(f, "descend::Atomic<{}>", at),
        }
    }
}

impl std::fmt::Display for ScalarTy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use ScalarTy::*;
        match self {
            Auto => write!(f, "auto"),
            Void => write!(f, "void"),
            I32 => write!(f, "descend::i32"),
            U32 => write!(f, "descend::u32"),
            F32 => write!(f, "descend::f32"),
            F64 => write!(f, "descend::f64"),
            SizeT => write!(f, "std::size_t"),
            Bool => write!(f, "bool"),
            Memory => write!(f, "descend::Memory"),
            Gpu => write!(f, "descend::Gpu"),
        }
    }
}

fn fmt_vec<D: std::fmt::Display>(f: &mut Formatter<'_>, v: &[D], sep: &str) -> std::fmt::Result {
    if let Some((last, leading)) = v.split_last() {
        for p in leading {
            write!(f, "{}{}", p, sep)?;
        }
        write!(f, "{}", last)
    } else {
        Ok(())
    }
}