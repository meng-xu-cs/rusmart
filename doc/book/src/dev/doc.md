### Documentation

Contains the comprehensive documentation for the project. Briefly, this directory contains the `book.toml` file, which is the configuration file for the project's documentation. It contains the following content:

```
[book]
authors = ["Meng Xu"]
language = "en"
multilingual = false
src = "src"
title = "Rusmart Language"

[build]
create-missing = false

[rust]
edition = "2021"

[output.html.playground]
runnable = true
```

The configuration file specifies the book's authors, language, title, etc. The `src` field indicates the directory where the `SUMMARY` markdown file is located. The `build` section specifies whether to create missing files; in this case, we do not want to create missing files if they are used in the `SUMMARY` file. The `rust` section specifies the Rust edition to use. Finally, the `output.html.playground` section specifies whether the rust playground in the HTML is runnable.

The `src` directory contains the markdown files that make up the documentation. The `SUMMARY.md` file in the `src` directory is used as the root file to structure the book and define the chapters or sections that are included. The `book` directory contains the generated HTML documentation from the `SUMMARY.md` file when running `mdbook build` and `mdbook serve` inside the `doc` directory. The first command renders the HTML documentation, while the second command serves the documentation locally on a web server. Conventionally, the `book` directory is added to the `.gitignore` file to avoid committing the generated documentation to the repository. Also, the `md` files are divided into different sections, such as `dev`, `user`, etc. for clearer development purposes. Last but not least, Cargo has a convenient way to create the book setup by running `mdbook init <name of book>` in the terminal. This command provides a template for the book setup, including the `book.toml` file and the `src` directory with the `SUMMARY.md` file. For more information on how to use `mdbook`, please refer to the [official documentation](https://rust-lang.github.io/mdBook/).