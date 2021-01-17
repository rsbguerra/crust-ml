fn main()
{
  let x:i32 = 6;
  let y:&i32 = &x;

  println!(*y);

  return;
}
