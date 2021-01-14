struct Point2D
{
  x : i32, // 0
  y : i32  // 8
}

struct Point3D
{
  x : i32, // 0
  y : i32, // 8
  z : i32  // 16
}

fn main() -> i32
{
    let p1 : Point2D = Point2D{x:1, y:2};
    let p2 : Point3D = Point3D{x:3, y:4, z:12};
    let p3 : Point2D = Point2D{x:5, y:6};
    let i:i32 = 42;
    let i2:i32 = 68;
    let i3:i32 = 69;
    
    //print!(p.x); // p.x = p 
    // (pos at rbp) + Point2D[x] 
    // -(8 + 0) = -8
    
    return 0;
}
