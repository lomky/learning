extern crate rand;

use std::io;
use rand::Rng;
use std::cmp::Ordering;

fn main() {
	let target = rand::thread_rng().gen_range(1,101);
    println!("Would You Like To Play A Game?");

	loop {
	    println!("Please enter your guess! (1-100)");
	    
	    let mut guess = String::new();
	    io::stdin().read_line(&mut guess)
	        .expect("failed to read line!");

	    println!("You guessed {}",guess);
	    
	    let guess: u32 = guess.trim().parse().expect("Type a number");

	    match guess.cmp(&target){
	    	Ordering::Less => println!("Too low!"),
	    	Ordering::Greater => println!("Too High!"),
	    	Ordering::Equal => {
	    		println!("You win!");
	    		break;
	    	},
	    }
	}
}
