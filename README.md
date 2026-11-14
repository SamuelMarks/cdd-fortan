# cdd-fortran

![doc coverage](https://img.shields.io/badge/doc%20coverage-100%25-brightgreen)
![test coverage](https://img.shields.io/badge/test%20coverage-100%25-brightgreen)
![standards](https://img.shields.io/badge/Fortran-FORTRAN%20I%20--%202023-blue)
[![License](https://img.shields.io/badge/license-Apache--2.0%20OR%20MIT-blue.svg)](https://opensource.org/licenses/Apache-2.0)

Self-hosted, strongly-typed **Fortran SDK** (`cdd_fortran`) and **CLI** (`cdd-fortran`) for lossless bidirectional conversion, zero-dependency JSON codec generation, and trivia-preserving surgical synchronization between **JSON** (JSON Schema Draft 2020-12 / OpenAPI 3.x and raw JSON samples) and **Fortran User-Defined Types (UDTs)** across all **12 Fortran standards** (`FORTRAN I` through `Fortran 2023`).

## Capabilities

| Subcommand | SDK Entry Point | Description |
| :--- | :--- | :--- |
| `json2fortran` | `json_to_fortran_udt` | Emit era-idiomatic Fortran UDT definitions from JSON Schema or sample JSON (`--with-codecs` optional). |
| `fortran2json` | `fortran_udt_to_json` | Extract JSON Schema (`--json-mode schema`) or sample JSON (`--json-mode sample`) from Fortran UDTs. |
| `gen-codec` | `generate_json_codecs` | Generate zero-dependency dialect-pure JSON serializer and parser routines. |
| `sync` | `update_fortran_from_json` | Surgically add/remove UDT & codec fields in place while preserving 100% of surrounding comments, card columns, and trivia. |

## Supported Dialect Matrix (`FORTRAN I` – `Fortran 2023`)

- **Legacy Fixed-Form (`-s f57 | f58 | f58-iii | f62 | f66 | f77`)**: `DIMENSION` + `EQUIVALENCE`, blank/named `COMMON`, `BLOCK DATA`, DEC `STRUCTURE`/`RECORD` (`--dec-structure`), Hollerith/BCD & `CHARACTER*N` codecs.
- **Modern Free- & Fixed-Form (`-s f90 | f95 | f2003 | f2008 | f2018 | f2023`)**: `MODULE` derived types, `PURE` procedures, OOP `TYPE, EXTENDS(...)` + type-bound procedures, `SUBMODULE` splits (`--submodules`), `IMPLICIT NONE (TYPE, EXTERNAL)`, and Fortran 2023 `ENUMERATION TYPE`.

## Build, Test, Documentation & Coverage

### CMake + CTest

```bash
cmake -B build -DENABLE_COVERAGE=ON
cmake --build build --parallel
ctest --test-dir build --output-on-failure
cmake --build build --target docs
cmake --build build --target coverage
```

### Fortran Package Manager (`fpm`)

```bash
fpm build --profile release
fpm install --profile release --prefix ./build --bindir .
fpm test --profile release
```

## CLI Quick Start

```bash
# Convert JSON Schema to Fortran 2023 derived type + JSON codecs
./build/cdd-fortran json2fortran -s f2023 --with-codecs -i schema.json -o model.f90

# Extract JSON Schema from an existing FORTRAN 77 or modern Fortran source file
./build/cdd-fortran fortran2json -s f77 --json-mode schema -i legacy.f -o extracted.schema.json

# Surgically synchronize an existing Fortran file with an updated JSON Schema
./build/cdd-fortran sync -s f2023 -t model.f90 -i schema.json --in-place
```

## Pre-commit Hooks

This repository uses [pre-commit](https://pre-commit.com/) to auto-format and recheck code formatting, compile with strict `-Werror` flags, lint Fortran/Python/Shell/GitHub Actions/Doxygen documentation, run the full test suite, and automatically keep the **doc coverage** and **test coverage** shields in `README.md` up to date:

```bash
pip install pre-commit
pre-commit install --hook-type pre-commit --hook-type pre-push
pre-commit run --all-files
```

---

## License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or <https://www.apache.org/licenses/Apache-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <https://opensource.org/licenses/MIT>)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
