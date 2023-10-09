fn main() {
    // NOTE:
    //
    // although not explicitly set, having a `build.rs`,
    // even with an empty `main` entrypoint,
    // allows both the `CARGO_MANIFEST_DIR` and `OUT_DIR` environment variable
    // to be visible in the macro derivation process.
    //
    // Both variables are needed for deriving SMT-related macro.
}
