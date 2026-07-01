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
remotes::install_github("hms-dbmi/pic-sure-r-adapter-hpds@v2.0.0")
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

# Genomic filters (authorized platforms)
gene_filter <- picsure::buildGenomicFilter("Gene_with_variant", values = c("BRCA1", "BRCA2"))
rare_filter <- picsure::buildGenomicFilter(
  "Variant_frequency_as_text", values = picsure::VariantFrequency$RARE
)
genomic_query <- picsure::buildQuery(genomicFilters = list(gene_filter, rare_filter))
genomic_count <- picsure::runQuery(bdc, genomic_query, type = "count")

# Discover valid genomic values (paginated)
picsure::searchGenomicValues(bdc, "Gene_with_variant", query = "BRCA")
picsure::genomicConsequences()  # offline; returns severity/consequence table

# Export
picsure::exportAsPFB(bdc, query, "~/cohort.pfb")
```

## Documentation

- [Getting
  started](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/getting-started.Rmd)
- [Search and
  facets](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/search-and-facets.Rmd) -
  includes genomic value discovery
- [Building
  queries](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/building-queries.Rmd) -
  includes genomic filters
- [Running and
  exporting](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/running-and-exporting.Rmd)
- [Migrating from
  v1](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/migrating-from-v1.Rmd)

## Migrating from v1

This is the 2.0.0 rewrite of the adapter, released as the `v2.0.0` tag.
The legacy 1.x adapter remains on `main` during the migration window.
See
[`vignettes/migrating-from-v1.Rmd`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/migrating-from-v1.Rmd)
for a side-by-side mapping of every 1.x function to its 2.0.0
equivalent.

## License

GPL (≥ 3). See
[`LICENSE`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/LICENSE).
