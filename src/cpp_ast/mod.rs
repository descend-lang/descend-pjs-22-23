use crate::ast::Nat;
use std::collections::HashMap;

pub(super) type Program = Vec<Item>;

// TODO big difference in sizes beteween variants
#[derive(Clone, Debug)]
pub(super) enum Item {
    Include{
        name: String,
        content: String,
    },
    FunDef {
        name: String,
        //TODO: This needs to be handeled differently for Kernel and C++ Functions
        templ_params: Vec<TemplParam>,
        //TODO: Remove
        templ_values: Vec<Vec<Expr>>,
        params: Vec<ParamDecl>,
        ret_ty: Ty,
        body: Stmt,
        is_gpu_function: bool,
    },
}

impl Item {
    pub fn new_fun_def(name: String) -> Item {
        Item::FunDef {
            name,
            templ_params: vec![],
            templ_values: vec![],
            params: vec![],
            // Empty return type is unit ()
            ret_ty: Ty::Tuple(vec![]),
            body: Stmt::Skip,
            is_gpu_function: false
        }
    }
}

//TODO: Make crate private again if possible
#[derive(Clone, Debug)]
pub struct ParamDecl {
    pub(super) name: String,
    pub(super) ty: Ty,
}

#[derive(Clone, Debug)]
pub(super) enum Stmt {
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
pub(super) enum Expr {
    // TODO Is there a better way to represent Unit values in C++?
    Empty,
    Ident(String),
    Lit(Lit),
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Lambda {
        captures: Vec<crate::ast::Ident>,
        params: Vec<ParamDecl>,
        body: Box<Stmt>,
        ret_ty: Ty,
        is_dev_fun: bool,
    },
    // Removed Lambda since Kernel is passed as String to exec
    FunCall {
        fun: Box<Expr>,
        template_args: Vec<TemplateArg>,
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
    // TODO: use thrust or std::tuple in c++
    Tuple(Vec<Expr>),
    // The current plan for Nats is to simply print them with C syntax.
    // Instead generate a C/Cuda expression?
    Nat(Nat),
}

#[derive(Clone, Debug)]
pub(super) enum Lit {
    Bool(bool),
    I32(i32),
    U32(u32),
    F32(f32),
    F64(f64),
}

#[derive(Clone, Debug)]
pub(super) enum UnOp {
    Not,
    Neg,
}

#[derive(Clone, Debug)]
pub(super) enum BinOp {
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
pub(super) enum TemplParam {
    Value { param_name: String, ty: Ty },
    TyName { name: String },
}

#[derive(Clone, Debug)]
pub(super) enum TemplateArg {
    Expr(Expr),
    Ty(Ty),
}

//TODO: Make crate private again if possible
#[derive(Clone, Debug)]
pub enum GpuAddrSpace {
    Global,
    Local,
    Constant,
}

//TODO: Make crate private again if possible
#[derive(Clone, Debug)]
pub enum Ty {
    Scalar(ScalarTy),
    // TODO: Refactor to cu only module?
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
    // Template parameter identifer
    Ident(String),
}

// TODO this is not really a Cuda type and should maybe be represented by a generic type construct
// TODO: Make crate private again if possible
#[derive(Clone, Debug)]
pub enum BufferKind {
    CpuMem,
    GpuGlobal,
    //TODO: Do we need this
    Ident(String),
}

//TODO: Make crate private again if possible
#[derive(Clone, Debug)]
pub enum ScalarTy {
    Auto,
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