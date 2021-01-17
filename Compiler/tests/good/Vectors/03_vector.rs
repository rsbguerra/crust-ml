struct Point{
    x:i32,
    y:i32
}

fn main() {
    let p1:Point = Point{x:12, y:14};
    let p2:Point = Point{x:15, y:54};

    let p:Vec<Point> = vec![p1, p2];
    
    print!(p[1].x);
    return;
}