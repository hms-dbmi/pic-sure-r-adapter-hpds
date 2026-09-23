# Remove an entry from a FacetSet.

Mutates the underlying Python FacetSet and returns the same handle for
chaining. Every entry under \`key\` matching \`value\` is dropped;
removing a value that was never added leaves the FacetSet unchanged.

## Usage

``` r
removeFacet(facet_set, key, value)
```

## Arguments

- facet_set:

  A FacetSet from \[\`facets()\`\]\[picsure::facets\].

- key:

  Facet key. Exactly one category, as for
  \[\`addFacet()\`\]\[picsure::addFacet\].

- value:

  Facet value, or a vector of values under the same \`key\`.

## Value

The same FacetSet, invisibly, for chaining.

## Details

The Python FacetSet has no per-value removal, so the category is
rebuilt: the survivors are computed first, the category is cleared, and
the survivors are added back. If adding them back fails, the original
selection is restored before the error is raised, so a failed removal
never leaves the category empty.

## Examples

``` r
if (FALSE) { # \dontrun{
fs <- picsure::facets(bdc)
fs <- picsure::addFacet(fs, "dataset_id", "phs000007")
fs <- picsure::removeFacet(fs, "dataset_id", "phs000007")
} # }
```
