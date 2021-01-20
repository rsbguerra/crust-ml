fn main() 
{
  let x:i32 = 123;
  let y:i32 = 126;

  let mut v:Vec<i32> = vec![x, y];

  v = vec![y];


  println!(v[0]);

  return;
}
