fn main() {
    let a = 15;

    if a < 10 {
    	println!("Contition is true");
    } else {
    	println!("Contition is false");
    }

    if a % 4 == 0 {
    	println!("A is divisible by 4");
    } else if a % 3 == 0 {
    	println!("A is divisible by 3");

    } else if a % 2 == 0 {
    	println!("A is divisible by 2");
 
    } else {
    	println!("A is not divisible by 4, 3, or 2");
    }

}
