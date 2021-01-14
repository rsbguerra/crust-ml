struct Point3D
{
    x : i32, // 0
    y : i32, // 8
    z : i32 // 16
}

struct Point2D
{
    x : i32, // 8
    y : i32 // 16
}

fn main() -> i32
{
    let p : Point2D = Point2D{x:6, y:7};

    print!(p.x);

    return 0;
} 
