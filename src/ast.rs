/// Definition of Nodes in the Abstract Syntax Tree (AST)
/// Detailed Context Free Grammar rules are in sysy.lalrpop

/// I. Compilation Unit

/// CompUnit        ::= [GlobalItem];
#[derive(Debug)]
pub struct CompUnit {
    pub global_items: Vec<GlobalItem>,
}

/// GlobalItem      ::= (Decl | FuncDef);
#[derive(Debug)]
pub enum GlobalItem {
    Decl(Decl),
    FuncDef(FuncDef),
}

/// II. Declaration of variables

/// Decl        ::= ConstDecl | VarDecl;
#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

/// ConstDecl   ::= "const" "int" ConstDef {"," ConstDef} ";";
#[derive(Debug)]
pub struct ConstDecl {
    pub const_defs: Vec<ConstDef>,
}

/// ConstDef        ::= IDENT {"[" ConstExp "]"} "=" ConstInitVal;
#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub dims: Vec<ConstExp>,
    pub const_init_val: ConstInitVal,
}

/// ConstInitVal    ::= ConstExp | "{" [ConstInitVal {"," ConstInitVal}] "}";
#[derive(Debug)]
pub enum ConstInitVal {
    ConstExp(ConstExp),
    ConstArrayInitVal(Vec<ConstInitVal>),
}

/// VarDecl     ::= "int" VarDef {"," VarDef} ";";
#[derive(Debug)]
pub struct VarDecl {
    pub var_defs: Vec<VarDef>,
}

/// VarDef          ::= IDENT {"[" ConstExp "]"}
///                 | IDENT {"[" ConstExp "]"} "=" InitVal;
#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub dims: Vec<ConstExp>,
    pub init_val: Option<InitVal>,
}

/// InitVal         ::= Exp | "{" [InitVal {"," InitVal}] "}";
#[derive(Debug)]
pub enum InitVal {
    Exp(Exp),
    ArrayInitVal(Vec<InitVal>),
}

/// III. Function Structures

/// FuncDef     ::= FuncType IDENT "(" [FuncFParams] ")" Block;
/// FuncFParams     ::= FuncFParam {"," FuncFParam};
#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub func_fparams: Vec<FuncFParam>,
    pub ident: String,
    pub block: Block,
}

/// FuncType    ::= "void" | "int";
#[derive(Debug)]
pub enum FuncType {
    Void,
    Int,
}

/// FuncFParam      ::= "int" IDENT ["[" "]" {"[" ConstExp "]"}];
#[derive(Debug)]
pub struct FuncFParam {
    pub ident: String,
    pub sub_dims: Vec<ConstExp>,
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

/// LVal            ::= IDENT {"[" Exp "]"};
#[derive(Debug)]
pub struct LVal {
    pub ident: String,
    pub idxs: Vec<Exp>,
}

/// PrimaryExp  ::= "(" Exp ")" | LVal | Number;
/// Number      ::= INT_CONST;
#[derive(Debug)]
pub enum PrimaryExp {
    ParenExp(Box<Exp>),
    LVal(LVal),
    Number(i32),
}

/// UnaryExp    ::= PrimaryExp | FuncExp | UnaryOp UnaryExp;
#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    FuncExp(FuncExp),
    UnaryOpExp(UnaryOp, Box<UnaryExp>),
}

/// FuncExp         ::= IDENT "(" [FuncRParams] ")";
/// FuncRParams     ::= FuncRParam {"," FuncRParam};
#[derive(Debug)]
pub struct FuncExp {
    pub ident: String,
    pub func_rparams: Vec<FuncRParam>,
}

/// FuncRParam  ::= Exp
#[derive(Debug)]
pub struct FuncRParam {
    pub exp: Exp,
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
