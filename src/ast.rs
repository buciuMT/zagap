use lazy_static::lazy_static;
use pest::Parser;
use std::collections::{BTreeMap, HashMap};
#[derive(Clone, Copy)]
pub enum InbuiltType {
    Void,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    USIZE,
    ISIZE,
    C8,
    C16,
    C32,
    F32,
    F64,
    F128,
}
lazy_static! {
    pub static ref STR2TYPE: BTreeMap<&'static str, InbuiltType> = BTreeMap::from([
        ("void", InbuiltType::Void),
        ("bool", InbuiltType::Bool),
        ("i8", InbuiltType::I8),
        ("i16", InbuiltType::I16),
        ("i32", InbuiltType::I32),
        ("i64", InbuiltType::I64),
        ("u8", InbuiltType::U8),
        ("u16", InbuiltType::U16),
        ("u32", InbuiltType::U32),
        ("u64", InbuiltType::U64),
        ("uSIZE", InbuiltType::USIZE),
        ("iSIZE", InbuiltType::ISIZE),
        ("c8", InbuiltType::C8),
        ("c16", InbuiltType::C16),
        ("c32", InbuiltType::C32),
        ("f32", InbuiltType::F32),
        ("f64", InbuiltType::F64),
        ("f128", InbuiltType::F128),
    ]);
}

#[derive(Clone)]
pub enum ZagapType {
    Ptr(Box<ZagapType>),
    Struct(usize),
    Array { t: Box<ZagapType>, size: usize },
    Inbuilt(InbuiltType),
}
impl Default for ZagapType {
    fn default() -> Self {
        ZagapType::Inbuilt(InbuiltType::Void)
    }
}

#[derive(Default, Clone)]
pub struct Struct_def<'a> {
    pub export_name: Option<&'a str>,
    pub elements: BTreeMap<&'a str, ZagapType>,
}

#[derive(Default, Clone)]
pub struct FunctionDef<'a> {
    pub import: bool,
    pub export_name: Option<&'a str>,
    pub ret: ZagapType,
    pub args: Vec<(&'a str, ZagapType)>,
    pub code: CodeBlock<'a>,
}

#[derive(Default, Clone)]
pub struct ProgramTable<'a> {
    pub structs: Vec<Struct_def<'a>>,
    pub funcs: Vec<FunctionDef<'a>>,
    pub str2func: HashMap<&'a str, usize>,
    pub str2struct: HashMap<&'a str, usize>,
}
#[derive(Default, Clone)]
pub struct CodeBlock<'a> {
    pub statements: Vec<Statement<'a>>,
    pub vars: BTreeMap<&'a str, (usize, ZagapType)>,
}

#[derive(Clone)]
pub enum Statement<'a> {
    Assigment(Expr<'a>, Expr<'a>),
    Expr(Expr<'a>),
    Code(CodeBlock<'a>),
    Cfor {
        init: Box<Statement<'a>>,
        cond: Expr<'a>,
        post: Box<Statement<'a>>,
        code: CodeBlock<'a>,
    },
    Cwhile(Expr<'a>, CodeBlock<'a>),
    Cloop(CodeBlock<'a>),
    Ret(Expr<'a>),
    Cti,
    Brk,
}

#[derive(Clone)]
pub enum UnaryOp {
    Minus,
    Lnot,
    Bnot,
}

#[derive(Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Asterix,
    Div,
    Modulo,
    Leq,
    Lless,
    Llesseq,
    Lmore,
    Lmoreeq,
    Lneq,
    Land,
    Lor,
    Band,
    Bor,
    Bxor,
    Bshiftr,
    Bshiftl,
}

#[derive(Clone)]
pub enum Expr<'a> {
    Etypechange(Box<Expr<'a>>, ZagapType),
    EUnary {
        op: UnaryOp,
        val: Box<Expr<'a>>,
    },
    EBinary {
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
        op: BinaryOp,
    },
    EDref(Box<Expr<'a>>),
    EPtr(Box<Expr<'a>>),
    EFuncCall(usize, Vec<Expr<'a>>),
    EClit(char),
    Eslit(&'a str),
    Enlit(f64),
    EVar(usize),
    EElement(Box<Expr<'a>>, &'a str),
    EIndex {
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
    },
}
