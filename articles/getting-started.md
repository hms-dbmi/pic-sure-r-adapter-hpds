# Getting started with picsure

This vignette walks through installing `picsure`, connecting to a
PIC-SURE network, running a simple search, and executing a one-clause
query.

## Install

``` r

remotes::install_github("hms-dbmi/pic-sure-r-adapter-hpds@query_v3")
library(picsure)
```

## Python environment

`picsure` declares its Python dependency via
[`reticulate::py_require()`](https://rstudio.github.io/reticulate/reference/py_require.html),
and reticulate uses `uv` to materialize an isolated environment on the
first call that touches Python (typically
[`connect()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/connect.md)).
There is no manual `pip`, `conda`, or `virtualenv` step, and no separate
`install_picsure()` to invoke.

The first such call takes a few seconds; the environment is cached and
reused by later R sessions. To pay that cost up front — for example,
before a demo, or while still on VPN — just call `picsure::connect(...)`
early in the session.

Requires `reticulate >= 1.41` (already declared in `DESCRIPTION`); `uv`
is fetched by reticulate as needed.

## Connect

``` r

bdc <- picsure::connect(
  platform = "BDC Authorized",
  token    = Sys.getenv("PICSURE_TOKEN")
)
```

Your token comes from the “User Profile” tab on the PIC-SURE web UI.
Valid platform names include `"Demo"`, `"BDC Open"`, and
`"BDC Authorized"`.

## Search the dictionary

``` r

picsure::searchDictionary(bdc, "sex")
```

[`searchDictionary()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/searchDictionary.md)
returns a data frame of matching variables. See
[`vignette("search-and-facets")`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/articles/search-and-facets.md)
for filtering by study or other facets.

## Build and run a query

``` r

sex_filter <- picsure::createClause(
  "\\phs000001\\pht000001\\phv00000001\\sex\\",
  type       = "FILTER",
  categories = list("male")
)
query <- picsure::buildClauseGroup(list(sex_filter), operator = "AND")

count <- picsure::runQuery(bdc, query, type = "count")
count
```

See
[`vignette("building-queries")`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/articles/building-queries.md)
for nested AND/OR trees and the other clause types.
