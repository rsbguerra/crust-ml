fn main() 
{
  let v:Vec<i32> = vec![1,2,4];

  let size:i32 = v.len();
  let mut i:i32 = 0;
 
  while(i < size)
  {
    println!(v[i]);
    i = i+1;
  }

  return;
}
