fn print_vec(v:Vec<i32>){
    let size:i32 = v.len();
    let mut i:i32 = 0;
    
    while(i<size){
        println!(v[i]);
        i = i+1;
    }

    return;
}

fn main() {
 let p:Vec<i32> = vec![1,2,4];
 print_vec(p);

 return;
}