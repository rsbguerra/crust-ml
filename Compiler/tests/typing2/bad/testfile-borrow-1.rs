fn main() {
    let x = 42;
    let mut r = &x;
    {
        let x = 89;
        r = &x;
    };               // x vit moins longtemps que r
    let a = *r;
}
