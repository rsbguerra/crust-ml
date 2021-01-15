struct Point2D
{
  x : i32,
  y : i32
}

fn add(p:Point2D) -> i32
{
  return p.x + p.y;
}

fn main() -> i32
{
  let p : Point2D = Point2D{x:2, y:6};
  let px : i32 = p.y;

  println!(add(p));
  
  return 0;
}
