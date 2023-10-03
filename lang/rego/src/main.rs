fn main() {
    rusmart_utils::config::initialize();
    rusmart_smt_derive::derive(env!("CARGO_MANIFEST_DIR"))
}
