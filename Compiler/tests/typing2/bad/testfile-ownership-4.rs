
struct S { a: i32, b: i32 } // pas un type copy
fn main() {
    let v = S { a:42, b:43 };
    let v2 = v;
    let x = v;
}
