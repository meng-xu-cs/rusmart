use std::path::Path;

use log::debug;

/// Entrypoint to the SMT model derivation process
pub fn derive<P: AsRef<Path>>(input: P) {
    let path_crate = input.as_ref();
    debug!("deriving for crate {}", path_crate.to_string_lossy());
}
