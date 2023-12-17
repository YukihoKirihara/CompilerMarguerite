/// Definition of Nodes in the Abstract Syntax Tree (AST)
/// Detailed Context Free Grammar rules are in sysy.lalrpop

/// I. Compilation Unit

/// CompUnit    ::= FuncDef;
#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

/// II. Declaration of variables

/// Decl        ::= ConstDecl | VarDecl;
#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

/// ConstDecl   ::= "const" BType ConstDef {"," ConstDef} ";";
#[derive(Debug)]
pub struct ConstDecl {
    pub btype: BType,
    pub const_defs: Vec<ConstDef>,
}

/// BType       ::= "int";
#[derive(Debug)]
pub enum BType {
    Int,
}

/// ConstDef    ::= IDENT "=" ConstInitVal;
#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: ConstInitVal,
}

/// ConstInitVal    ::= ConstExp;
#[derive(Debug)]
pub struct ConstInitVal {
    pub const_exp: ConstExp,
}

/// VarDecl     ::= BType VarDef {"," VarDef} ";";
#[derive(Debug)]
pub struct VarDecl {
    pub btype: BType,
    pub var_defs: Vec<VarDef>,
}

/// VarDef      ::= IDENT | IDENT "=" InitVal;
#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub init_val: Option<InitVal>,
}

/// InitVal     ::= Exp;
#[derive(Debug)]
pub struct InitVal {
    pub exp: Exp,
}

/// III. Function Structures

/// FuncDef     ::= FuncType IDENT "(" ")" Block;
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

/// FuncType    ::= "int";
#[derive(Debug)]
pub enum FuncType {
    Int,
}

/// Block       ::= "{" { BlockItem} "}";
#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

/// BlockItem   ::= Decl | Stmt;
#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

/// IV. Statements

/// Stmt        ::= Assign
///             | IdleExp
///             | Block
///             | IfClause
///             | WhileClause
///             | Break
///             | Continue
///             | Return
#[derive(Debug)]
pub enum Stmt {
    Assign(Assign),
    IdleExp(IdleExp),
    Block(Block),
    IfClause(IfClause),
    WhileClause(WhileClause),
    Continue(Continue),
    Break(Break),
    Return(Return),
}

/// Assign      ::= LVal "=" Exp ";";
#[derive(Debug)]
pub struct Assign {
    pub lval: LVal,
    pub exp: Exp,
}

/// IdleExp     ::= [Exp] ";"
#[derive(Debug)]
pub struct IdleExp {
    pub exp: Option<Exp>,
}

/// IfClause    ::= "if" "(" Exp ")" Stmt ["else" Stmt]
#[derive(Debug)]
pub struct IfClause {
    pub cond: Exp,
    pub true_stmt: Box<Stmt>,
    pub false_stmt: Option<Box<Stmt>>,
}

/// WhileClause ::= "while" "(" Exp ")" Stmt
#[derive(Debug)]
pub struct WhileClause {
    pub cond: Exp,
    pub loop_stmt: Box<Stmt>,
}

/// Break       ::= "break" ";"
#[derive(Debug)]
pub struct Break {}

/// Continue    ::= "continue" ";"
#[derive(Debug)]
pub struct Continue {}

/// Return      ::= "return" Exp ";";
#[derive(Debug)]
pub struct Return {
    pub exp: Exp,
}

/// V. Expressions

/// Exp         ::= LOrExp;
#[derive(Debug)]
pub struct Exp {
    pub lor_exp: LOrExp,
}

/// LVal        ::= IDENT;
#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}

/// PrimaryExp  ::= "(" Exp ")" | LVal | Number;
/// Number      ::= INT_CONST;
#[derive(Debug)]
pub enum PrimaryExp {
    ParenExp(Box<Exp>),
    LVal(LVal),
    Number(i32),
}

/// UnaryExp    ::= PrimaryExp | UnaryOp UnaryExp;
#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOpExp(UnaryOp, Box<UnaryExp>),
}

/// UnaryOp     ::= "+" | "-" | "!";
/// "+" (Positive) is neglected as an identical operator.
#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

/// MulExp      ::= UnaryExp | MulExp MulOp UnaryExp;
#[derive(Debug)]
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulUnaryExp(Box<MulExp>, MulOp, UnaryExp),
}

/// MulOp       ::= "*" | "/" | "%"
#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

/// AddExp      ::= MulExp | AddExp AddOp MulExp;
#[derive(Debug)]
pub enum AddExp {
    MulExp(MulExp),
    AddMulExp(Box<AddExp>, AddOp, MulExp),
}

/// AddOp       ::= "+" | "-"
#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
}

/// RelExp      ::= AddExp | RelExp RelOp AddExp;
#[derive(Debug)]
pub enum RelExp {
    AddExp(AddExp),
    RelAddExp(Box<RelExp>, RelOp, AddExp),
}

/// RelOp       ::= "<" | ">" | "<=" | ">="
#[derive(Debug)]
pub enum RelOp {
    LT,
    GT,
    LE,
    GE,
}

/// EqExp       ::= RelExp | EqExp EqOp RelExp;
#[derive(Debug)]
pub enum EqExp {
    RelExp(RelExp),
    EqRelExp(Box<EqExp>, EqOp, RelExp),
}

/// EqOp        ::= "==" | "!="
#[derive(Debug)]
pub enum EqOp {
    Eq,
    Neq,
}

/// LAndExp     ::= EqExp | LAndExp "&&" EqExp;
#[derive(Debug)]
pub enum LAndExp {
    EqExp(EqExp),
    LAndEqExp(Box<LAndExp>, EqExp),
}

/// LOrExp      ::= LAndExp | LOrExp "||" LAndExp;
#[derive(Debug)]
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrAndExp(Box<LOrExp>, LAndExp),
}

/// ConstExp    ::= Exp;
#[derive(Debug)]
pub struct ConstExp {
    pub exp: Exp,
}
