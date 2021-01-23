
fn take(v: &mut Vec<i32>) {
    v[0] = 42;
    return;
}
fn main() {
    let v : Vec<i32> = vec![1, 2, 3];
    take(&mut v);
    let x : i32 = v[0];
    return;
}
