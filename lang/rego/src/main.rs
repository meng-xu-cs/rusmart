use rusmart_smt_derive::derive;
use rusmart_utils::config::initialize;

fn main() {
    initialize();
    derive(env!("CARGO_MANIFEST_DIR"))
}
