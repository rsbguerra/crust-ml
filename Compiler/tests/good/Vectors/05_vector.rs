fn print_vec(v:Vec<i32>){
    let size:i32 = v.len();
    let mut i:i32 = 0;
    
    while(i<size){
        println!(v[i]);
        i = i+1;
    }
}

fn main() {
    let x:&i32 = 123;
    let y:&i32 = 126;

    let p:Vec<&i32> = vec![x, y];
    return;
}