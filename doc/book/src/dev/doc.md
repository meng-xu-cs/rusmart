### Documentation

Contains the comprehensive documentation for the project. Briefly, this directory contains the `book.toml` file, which is the configuration file for the project's documentation. This approach is mainly used when a book or vast documentation is intended to be written for the project. It contains the following content:

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

The configuration file specifies the book's authors, language, title, etc. The `src` field indicates the directory where the markdown files are located. The `build` section specifies whether to create missing files. The `rust` section specifies the Rust edition to use. Finally, the `output.html.playground` section specifies whether the rust playground in the HTML is runnable.

The `src` directory contains the markdown files that make up the documentation. The `SUMMARY.md` file in the `src` directory is used as the root file to structure the book and define the chapters or sections that are included. The `book` directory contains the generated HTML documentation from the `SUMMARY.md` file when running `mdbook build` and `mdbook serve` inside the `doc` directory. The first command generates the HTML documentation, while the second command serves the documentation locally on a web server. Conventionally, the `book` directory is added to the `.gitignore` file to avoid committing the generated documentation to the repository. Also, the `md` files are divided into different sections, such as `dev`, `user`, etc. for clearer development purposes.