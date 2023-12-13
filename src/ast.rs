/// Detailed Context Free Grammar rules are in sysy.lalrpop

#[derive(Debug)]
/// CompUnit    ::= FuncDef;
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
/// FuncDef     ::= FuncType IDENT "(" ")" Block;
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
/// FuncType    ::= "int";
pub enum FuncType {
    Int,
}

#[derive(Debug)]
/// Block       ::= "{" Stmt "}";
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug)]
/// Stmt        ::= Return;
pub enum Stmt {
    Return(Return),
}

#[derive(Debug)]
/// Return        ::= "return" Return ";";
pub struct Return {
    pub exp: Exp,
}

#[derive(Debug)]
/// Exp         ::= LOrExp;
pub struct Exp {
    pub lor_exp: LOrExp,
}

#[derive(Debug)]
/// PrimaryExp  ::= "(" Exp ")" | Number;
/// Number      ::= INT_CONST;
pub enum PrimaryExp {
    Number(i32),
    ParenExp(Box<Exp>),
}

#[derive(Debug)]
/// UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOpExp(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug)]
/// UnaryOp     ::= "+" | "-" | "!";
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug)]
/// MulExp      ::= UnaryExp | MulExp ("*" | "/" | "%") UnaryExp;
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulUnaryExp(Box<MulExp>, MulOp, UnaryExp),
}

#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
/// AddExp      ::= MulExp | AddExp ("+" | "-") MulExp;
pub enum AddExp {
    MulExp(MulExp),
    AddMulExp(Box<AddExp>, AddOp, MulExp),
}

#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
}

#[derive(Debug)]
/// RelExp      ::= AddExp | RelExp ("<" | ">" | "<=" | ">=") AddExp;
pub enum RelExp {
    AddExp(AddExp),
    RelAddExp(Box<RelExp>, RelOp, AddExp),
}

#[derive(Debug)]
pub enum RelOp {
    LT,
    GT,
    LE,
    GE,
}

#[derive(Debug)]
/// EqExp       ::= RelExp | EqExp ("==" | "!=") RelExp;
pub enum EqExp {
    RelExp(RelExp),
    EqRelExp(Box<EqExp>, EqOp, RelExp),
}

#[derive(Debug)]
pub enum EqOp {
    Eq,
    Neq,
}

#[derive(Debug)]
/// LAndExp     ::= EqExp | LAndExp "&&" EqExp;
pub enum LAndExp {
    EqExp(EqExp),
    LAndEqExp(Box<LAndExp>, EqExp),
}

#[derive(Debug)]
/// LOrExp      ::= LAndExp | LOrExp "||" LAndExp;
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrAndExp(Box<LOrExp>, LAndExp),
}
