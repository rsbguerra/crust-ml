fn take(v: &Vec<i32>) {
    v[0] = 42
}
fn main() {
    let v = vec![1, 2, 3];
    take(&v);
    let x = v[0];
}
