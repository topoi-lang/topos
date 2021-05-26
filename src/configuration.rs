use env_logger::Builder;
use log::LevelFilter;

pub fn init_log() {
    let mut builder = Builder::from_env("LOG");
    builder.init();
}
