use lazy_static::lazy_static;
use pest::Parser;
use std::collections::{BTreeMap, HashMap};
#[derive(Clone, Copy)]
pub enum InbuiltType {
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
    pub static ref str2type: BTreeMap<&'static str, InbuiltType> = BTreeMap::from([
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

pub enum ZagapType {
    Ptr(Box<ZagapType>),
    Struct(usize),
    Array { t: Box<ZagapType>, size: usize },
    Inbuilt(InbuiltType),
}

#[derive(Default)]
pub struct Struct_def<'a> {
    pub export_name: Option<&'a str>,
    pub elements: BTreeMap<&'a str, ZagapType>,
}

pub struct Function_def<'a> {
    export_name: Option<&'a str>,
    ret: ZagapType,
    args: Vec<ZagapType>,
    code: CodeBlock<'a>,
}
#[derive(Default)]
pub struct ProgramTable<'a> {
    pub structs: Vec<Struct_def<'a>>,
    pub funcs: Vec<Function_def<'a>>,
    pub str2func: HashMap<&'a str, usize>,
    pub str2struct: HashMap<&'a str, usize>,
}
#[derive(Default)]
pub struct CodeBlock<'a> {
    statements: Vec<Statement<'a>>,
    vars: BTreeMap<&'a str, (usize, ZagapType)>,
}

pub enum Statement<'a> {
    Assigment(Lval<'a>, Rval<'a>),
    Expr(Rval<'a>),
}

pub enum Lval<'a> {
    LDref(Box<Lval<'a>>),
    LPtr(Box<Lval<'a>>),
    LFuncCall(usize, Vec<Rval<'a>>),
    LVar(usize),
    LElement(Box<Lval<'a>>, &'a str),
}

pub enum UnaryOp {
    Minus,
    Lnot,
    Bnot,
}
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

pub enum Rval<'a> {
    Lval(Lval<'a>),
    Unary {
        op: UnaryOp,
        val: Box<Rval<'a>>,
    },
    Binary {
        lhs: Box<Rval<'a>>,
        rhs: Box<Rval<'a>>,
        op: BinaryOp,
    },
}
