use std::io::{self, Stdin, Stdout, Write};

use rand::prelude::*;

use crate::expression::Expression;

mod die;
mod expression;

fn main() -> io::Result<()> {
    let mut rng = thread_rng();
    let mut user_input = UserInput::default();
    loop {
        let s = user_input.prompt("> ")?;
        if let Some(expression) = &s.trim().parse::<Expression>().ok() {
            let result = expression.evaluate(&mut rng);
            println!("{:?} = {} = {}", expression, expression, result);
        } else {
            println!("couldn't read expression!");
        }
    }
}

struct UserInput {
    stdin: Stdin,
    stdout: Stdout,
}

impl UserInput {
    fn new(stdin: Stdin, stdout: Stdout) -> Self {
        Self { stdin, stdout }
    }

    fn prompt(&mut self, p: &str) -> io::Result<String> {
        print!("{}", p);
        self.stdout.flush()?;
        let mut s = String::new();
        self.stdin.read_line(&mut s)?;
        Ok(s)
    }
}

impl Default for UserInput {
    fn default() -> Self {
        UserInput::new(io::stdin(), io::stdout())
    }
}
