
struct S { a: i32 }
struct T { a: i32 }
fn main() { let s : S = S { a: 89 }; let t : T = T { a: 987 }; s = t; return; }
