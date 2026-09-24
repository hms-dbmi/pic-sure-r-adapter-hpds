# Contributing to the PIC-SURE R adapter

Please read the [PIC-SURE contributing guide](https://github.com/hms-dbmi/pic-sure/blob/main/CONTRIBUTING.md)
first. It covers the code of conduct, filing issues, and how pull requests are reviewed across
every PIC-SURE repository.

## Building and testing this repo

This is an R package named `picsure`. Tests use [testthat](https://testthat.r-lib.org) and live
in `tests/testthat`.

```r
devtools::load_all()
testthat::test_dir("tests/testthat")
devtools::document()
```

`testthat::test_dir` is what this repository's own CI runs. `R CMD check .` does the package check but
executes no tests, because there is no `tests/testthat.R` harness for it to pick up. Run
`devtools::document()` after changing roxygen comments, to regenerate `man/`.
