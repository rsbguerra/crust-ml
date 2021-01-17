struct Point{
  x:i32,
  y:i32
}



fn main(){
  let a : i32 = 10;
  let b : i32 = 20;
  let c : i32 = 30;
  let d : &mut i32 = &mut b;
  
  *d = 4;
  
  println!( *d );

  return;
}

