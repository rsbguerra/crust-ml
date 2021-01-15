struct Point2D {
    x : i32,
    y : i32
  }
  
fn add(p:Point2D) -> i32{
return p.x + p.y;
}

fn main(){
    let p:Point2D{x:12, y:14}

    println!(add(p))
}