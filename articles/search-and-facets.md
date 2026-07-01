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

## Genomic value discovery

On authorized platforms,
[`searchGenomicValues()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/searchGenomicValues.md)
returns valid values for a given genomic annotation key (e.g. gene
names, consequence labels). Results are paginated via `page` and `size`
arguments.

``` r

# Find gene names starting with "BRCA"
picsure::searchGenomicValues(bdc, "Gene_with_variant", query = "BRCA")

# Page through consequence values
picsure::searchGenomicValues(bdc, "Variant_consequence_calculated", page = 1, size = 50)
```

Both calls return a `data.frame`. Use the values in
[`buildGenomicFilter()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildGenomicFilter.md).

[`genomicConsequences()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/genomicConsequences.md)
requires no session - it returns the full list of known variant
consequences with their severity ranking offline:

``` r

consequences <- picsure::genomicConsequences()
head(consequences)
```
