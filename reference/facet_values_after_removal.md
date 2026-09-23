# Compute what a facet category holds once some values are removed.

The Python FacetSet exposes \`add()\`, \`view()\`, and \`clear()\` but
no per-value removal, so \[\`removeFacet()\`\]\[picsure::removeFacet\]
reads the current selections, drops the ones being removed, and rewrites
the category. \`view()\` reports every available category, so a key with
no selections and an unknown key both yield an empty vector.

## Usage

``` r
facet_values_after_removal(view, key, value)
```

## Arguments

- view:

  The named list returned by the Python FacetSet's \`view()\`.

- key:

  Facet category name.

- value:

  Value, or vector of values, being removed.

## Value

A character vector of the values to keep, in their original order.
