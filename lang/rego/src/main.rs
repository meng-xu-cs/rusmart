use rusmart_smt_derive::derive;
use rusmart_utils::config::WKS;

fn main() {
    match derive(env!("CARGO_MANIFEST_DIR"), WKS.studio.join("rego")) {
        Ok(()) => (),
        Err(e) => panic!("{}", e),
    }
}
