use koopa::ir::Value;

/// Variables in the C program
pub enum Variable {
    // Mutable variable
    Value(Value),
    // Constant variable
    Const(i32),
}
