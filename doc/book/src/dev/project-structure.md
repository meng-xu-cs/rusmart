## Project Structure

The project directory structure is as follows:

- [**doc**](doc.md)

- [**deps**](deps.md)

- [**utils**](crates/utils.md)

- [**cli**](crates/cli.md)

- [**smt/stdlib**](crates/smt/stdlib.md)

- [**smt/remark**](crates/smt/remark.md)

- [**smt/derive**](crates/smt/derive.md)

- [**smt/testing**](crates/smt/testing.md)

- [**lang/demo**](crates/lang/demo.md)

- [**lang/rego**](crates/lang/rego.md)

Note that the `doc` and `deps` are not packages but directories that contain documentation and dependencies, respectively. The `utils`, `cli`, `smt/stdlib`, `smt/remark`, `smt/derive`, `smt/testing`, `lang/demo`, and `lang/rego` directories contain the actual packages, each containing a `Cargo.toml` file that specifies the crate's metadata and dependencies. The `src` directory within each package contains the source code.