use rusmart_smt_derive::derive;

fn main() {
    match derive(env!("CARGO_MANIFEST_DIR")) {
        Ok(()) => (),
        Err(e) => panic!("{}", e),
    }
}
