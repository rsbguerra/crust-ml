struct S { a: i32, b: bool }
struct T { x: S, y: S }

fn main() {
    let x = S { a: 42, b: true };
    let y = S { a: 89, b: false };
    let mut t = T { x: x, y: y };
    if t.x.a == 42 { print!("yes\n") } else { print!("oups\n") };
    if t.x.b       { print!("yes\n") } else { print!("oups\n") };
    if t.y.a == 89 { print!("yes\n") } else { print!("oups\n") };
    if ! t.y.b     { print!("yes\n") } else { print!("oups\n") };
    let z = t.x;
    if z.a == 42 { print!("yes\n") } else { print!("oups\n") };
    if z.b       { print!("yes\n") } else { print!("oups\n") };
    t.y = z;
    if t.y.a == 42 { print!("yes\n") } else { print!("oups\n") };
    if t.y.b       { print!("yes\n") } else { print!("oups\n") };
}
