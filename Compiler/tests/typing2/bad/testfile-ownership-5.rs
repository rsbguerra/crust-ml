
struct S { a: i32, b: i32 } // pas un type copy
fn take(s: S) {
}
fn main() {
    let v = S { a:42, b:43 };
    take(v);
    let x = v.a;
}
