# Remove an entry from a FacetSet.

Mutates the underlying Python FacetSet and returns the same handle for
chaining.

## Usage

``` r
removeFacet(facet_set, key, value)
```

## Arguments

- facet_set:

  A FacetSet from \[\`facets()\`\]\[picsure::facets\].

- key:

  Facet key.

- value:

  Facet value (scalar only).

## Value

The same FacetSet, invisibly, for chaining.

## Examples

``` r
if (FALSE) { # \dontrun{
fs <- picsure::facets(bdc)
fs <- picsure::addFacet(fs, "study_ids", "phs000007")
fs <- picsure::removeFacet(fs, "study_ids", "phs000007")
} # }
```
