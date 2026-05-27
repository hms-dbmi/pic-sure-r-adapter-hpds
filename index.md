# picsure

`picsure` is an R client for the [PIC-SURE API](https://pic-sure.org/).
It provides functions to connect to PIC-SURE networks, search the data
dictionary, build nested query clauses, run queries, and export results.
The package is a thin wrapper around the Python
[`picsure`](https://github.com/hms-dbmi/pic-sure-python-adapter-hpds)
package, wired through
[`reticulate`](https://rstudio.github.io/reticulate/). The required
Python environment is provisioned automatically on first use.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("hms-dbmi/pic-sure-r-adapter-hpds@query_v3")
```

The package requires R ≥ 4.1 and `reticulate` ≥ 1.41. On first call,
reticulate resolves the Python `picsure` package into an isolated
`uv`-backed virtualenv; this takes a few seconds the first time only.

## Quick start

``` r

library(picsure)

# Connect
bdc <- picsure::connect(
  platform = picsure::Platform$BDC_AUTHORIZED,
  token    = Sys.getenv("PICSURE_TOKEN")
)

# Search
results <- picsure::searchDictionary(bdc, "sex")

# Build a query
sex_filter <- picsure::buildClause(
  "\\phs000001\\pht000001\\phv00000001\\sex\\",
  type       = "FILTER",
  categories = list("male")
)
age_filter <- picsure::buildClause(
  "\\phs000001\\pht000001\\phv00000005\\age\\",
  type = "FILTER", min = 40
)
query <- picsure::buildClauseGroup(list(sex_filter, age_filter), operator = "AND")

# Run
count        <- picsure::runQuery(bdc, query, type = "count")
participants <- picsure::runQuery(bdc, query, type = "participant")

# Export
picsure::exportAsPFB(bdc, query, "~/cohort.pfb")
```

## Documentation

- [Getting
  started](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/getting-started.Rmd)
- [Search and
  facets](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/search-and-facets.Rmd)
- [Building
  queries](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/building-queries.Rmd)
- [Running and
  exporting](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/running-and-exporting.Rmd)
- [Migrating from
  v1](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/migrating-from-v1.Rmd)

## Migrating from v1

The v1 adapter on `main` is unchanged; this rewrite lives on `query_v3`.
See
[`vignettes/migrating-from-v1.Rmd`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/migrating-from-v1.Rmd)
for a side-by-side mapping of every v1 function to its v3 equivalent.

## License

GPL (≥ 3). See
[`LICENSE`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/LICENSE).
