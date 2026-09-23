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

  Facet category name, e.g. \`"dataset_id"\` or \`"data_type"\`. Exactly
  one category. A facet entry selects values inside a single category,
  so a vector of keys raises a \`picsureError\`. The valid category
  names come from the server, not from this package; a name the
  deployment does not publish raises a \`picsureError\` listing the ones
  it does.

- value:

  Facet value, or a vector of values under the same \`key\`.

## Value

The same FacetSet, invisibly, for chaining.

## Examples

``` r
if (FALSE) { # \dontrun{
fs <- picsure::facets(bdc)
fs <- picsure::addFacet(fs, "dataset_id", "phs000007")
fs <- picsure::addFacet(fs, "dataset_id", c("phs000200", "phs000286"))
} # }
```
