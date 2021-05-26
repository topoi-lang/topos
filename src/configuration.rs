use env_logger::Builder;
use std::time::Instant;
use std::time::Duration;

///! ## Configuration layer
///! most of the configuration arguments are from the environment

pub fn init_log() {
    let mut builder = Builder::from_env("LOG");
    builder.init();
}

/// Used to calculate a execution time of a function
pub fn timing<T>(body: impl FnOnce() -> T) -> (T, Duration) {
    let start = Instant::now();
    let result = body();
    let end = Instant::now();
    (result, end - start)
}
