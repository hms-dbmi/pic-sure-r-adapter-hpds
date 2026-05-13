# Add an entry to a FacetSet.

Mutates the underlying Python FacetSet and returns the same handle for
chaining. If \`value\` is a vector of length \> 1, adds one entry per
value (all under the same \`key\`).

## Usage

``` r
addFacet(facet_set, key, value)
```

## Arguments

- facet_set:

  A FacetSet from \[\`facets()\`\]\[picsure::facets\].

- key:

  Facet key, e.g. \`"study_ids"\`.

- value:

  Facet value (scalar or vector).

## Value

The same FacetSet, invisibly, for chaining.

## Examples

``` r
if (FALSE) { # \dontrun{
fs <- picsure::facets(bdc)
fs <- picsure::addFacet(fs, "study_ids", "phs000007")
fs <- picsure::addFacet(fs, "study_ids", c("phs000200", "phs000286"))
} # }
```
