mod configuration;
mod parser;

use log::{info};

fn main() {
    configuration::init_log();

    info!("hey");
    println!("Hello, world!");
}
