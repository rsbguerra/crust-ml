struct Point{
    x:i32,
    y:i32
}

fn main() {
    let p1:Point = Point{x:12, y:14};
    let p2:Point = Point{x:15, y:54};

    let p:Vec<Point> = vec![p1, p2];
    

    let v :Point = p[1];
    print!(v.x);
    return;
}