/// Register names to avoid typos
macro_rules! T0 {
    () => {
        "t0"
    };
}
pub(crate) use T0;

macro_rules! T1 {
    () => {
        "t1"
    };
}
pub(crate) use T1;

macro_rules! T2 {
    () => {
        "t2"
    };
}
pub(crate) use T2;

macro_rules! SP {
    () => {
        "sp"
    };
}
pub(crate) use SP;

macro_rules! A0 {
    () => {
        "a0"
    };
}
pub(crate) use A0;

macro_rules! Ai {
    ($i:expr) => {
        format!("a{}", $i)
    };
}
pub(crate) use Ai;
