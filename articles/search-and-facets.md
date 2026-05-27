# Search and facets

[`picsure::searchDictionary()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/searchDictionary.md)
returns matching dictionary entries as a data frame.
[`picsure::facets()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/facets.md)
narrows a search to a subset — by study, by consent group, etc.

## Plain search

``` r

library(picsure)
bdc <- picsure::connect(platform = picsure::Platform$BDC_AUTHORIZED, token = Sys.getenv("PICSURE_TOKEN"))

results <- picsure::searchDictionary(bdc, "sex")
head(results)
```

## Build a FacetSet

``` r

fs <- picsure::facets(bdc)
fs <- picsure::addFacet(fs, "study_ids", "phs000007")
```

[`addFacet()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/addFacet.md)
accepts a vector of values to add several at once:

``` r

fs <- picsure::addFacet(fs, "study_ids", c("phs000200", "phs000286"))
```

Remove with `removeFacet(fs, key, value)`. Both functions mutate the
underlying Python FacetSet and return it invisibly for chaining.

## Search with facets

``` r

picsure::searchDictionary(bdc, "sex", facets = fs)
```

The server restricts the search to variables inside the facets.

## Lighter payloads

Pass `include_values = FALSE` to omit variable values from the response
— useful when you only need variable names and descriptions:

``` r

picsure::searchDictionary(bdc, "age", include_values = FALSE)
```

The search term is always matched server-side; you receive every
matching row. There is no `limit` or `offset` — narrow with a more
specific term or a facet filter if a search returns too many results.
