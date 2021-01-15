<<<<<<< Updated upstream
// Single line comment
fn main(){
  return;
}
=======
struct Point2D
{
  x:i32,
  y:i32
}

fn make_point2D() -> Point2D
{
  let p : Point2D = Point2D{x:5, y:9};


  println!(p.x);
  println!(p.y);
  
  return p;
}

fn main() -> i32
{
  let p1 : Point2D = make_point2D( );

  return 0;
}
>>>>>>> Stashed changes
