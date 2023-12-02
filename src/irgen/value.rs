use koopa::ir::Value as IrValue;

pub enum Value {
    // Koopa IR value
    Value(IrValue),
    Const(i32),
}
