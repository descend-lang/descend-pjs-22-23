mod visit_mut;
mod visit;
pub(crate) mod cpp_to_c_mapper;

use crate::ast::Nat;

// TODO big difference in sizes beteween variants
pub enum Item {
    Include(String),
    FunDef {
        name: String,
        params: Vec<ParamDecl>,
        ret_ty: Ty,
        body: Stmt,
        is_dev_fun: bool,
    },
}

#[derive(Clone, Debug)]
pub struct ParamDecl {
    pub name: String,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Skip,
    VarDecl {
        name: String,
        ty: Ty,
        addr_space: Option<GpuAddrSpace>,
        expr: Option<Expr>,
    },
    Block(Box<Stmt>),
    Seq(Vec<Stmt>),
    Expr(Expr),
    If {
        cond: Expr,
        body: Box<Stmt>,
    },
    IfElse {
        cond: Expr,
        true_body: Box<Stmt>,
        false_body: Box<Stmt>,
    },
    While {
        cond: Expr,
        stmt: Box<Stmt>,
    },
    ForLoop {
        init: Box<Stmt>,
        cond: Expr,
        iter: Expr,
        stmt: Box<Stmt>,
    },
    Return(Option<Expr>),
    Label(String),
}

#[derive(Clone, Debug)]
pub enum Expr {
    // TODO Is there a better way to represent Unit values in C++?
    Empty,
    Ident(String),
    Lit(Lit),
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    FunCall {
        fun: Box<Expr>,
        args: Vec<Expr>,
    },
    UnOp {
        op: UnOp,
        arg: Box<Expr>,
    },
    BinOp {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    ArraySubscript {
        array: Box<Expr>,
        index: Nat,
    },
    Proj {
        tuple: Box<Expr>,
        n: usize,
    },
    InitializerList {
        elems: Vec<Expr>,
    },
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    Tuple(Vec<Expr>),
    // The current plan for Nats is to simply print them with C syntax.
    // Instead generate a C/Cuda expression?
    Nat(Nat),
}

#[derive(Clone, Debug)]
pub enum Lit {
    Bool(bool),
    I32(i32),
    U32(u32),
    F32(f32),
    F64(f64),
}

#[derive(Clone, Debug)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Clone, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
    Neq,
}

#[derive(Clone, Debug)]
pub enum Exec {
    Host,
    Device,
}

#[derive(Clone, Debug)]
pub enum GpuAddrSpace {
    Global,
    Shared,
    Constant,
}

#[derive(Clone, Debug)]
pub enum Ty {
    Scalar(ScalarTy),
    Atomic(ScalarTy),
    Tuple(Vec<Ty>),
    Array(Box<Ty>, Nat),
    CArray(Box<Ty>, Nat),
    Buffer(Box<Ty>, BufferKind),
    // for now assume every pointer to be __restrict__ qualified
    // http://www.open-std.org/JTC1/SC22/WG14/www/docs/n1256.pdf#page=122&zoom=auto,-205,535
    Ptr(Box<Ty>, Option<GpuAddrSpace>),
    // The pointer itself is mutable, but the underlying data is not.
    PtrConst(Box<Ty>, Option<GpuAddrSpace>),
    // TODO In C++ const is a type qualifier (as opposed to qualifying an identifier).
    //  However the way we generate code let's us treat const as an identifier qualifier (we would
    //  not return a const value from a function for example, but e.g., a non-const const pointer).
    //  Should the AST be changed to reflect this?
    // const in a parameter declaration changes the parameter type in a definition but not
    // "necessarily" the function signature ... https://abseil.io/tips/109
    // Top-level const
    Const(Box<Ty>),
}

// TODO this is not really a Cuda type and should maybe be represented by a generic type construct
#[derive(Clone, Debug)]
pub enum BufferKind {
    CpuMem,
    GpuGlobal,
    Ident(String),
}

#[derive(Clone, Debug)]
pub enum ScalarTy {
    Void,
    I32,
    U32,
    F32,
    F64,
    Bool,
    SizeT,
    Memory,
    Gpu,
}
