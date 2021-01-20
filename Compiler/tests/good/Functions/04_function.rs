fn ToTen(i : i32)
{
    if i > 10
    {
        return;
    }
    
    println!(i);
    
    return ToTen(i+1);
}

fn main()
{
  ToTen(0);
  
  return;
}
