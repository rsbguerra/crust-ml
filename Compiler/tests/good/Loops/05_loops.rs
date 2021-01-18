fn main()
{
    let mut x : i32 = 0;
    
    while true
    {
        x = x+1;
        if (x > 5){
            break;
        }
        else{
          println!(x);
          continue;
        }
    }
    return;
}
