# Look up valid values for a genomic annotation key (authorized platforms).

Returns one page of values for a genomic key (e.g. all genes with
variants), as a data.frame with a single \`value\` column. Raise
\`size\` to pull more per call, or step \`page\` to walk the full set.

## Usage

``` r
searchGenomicValues(
  session,
  genomicConceptPath,
  query = "",
  page = 1,
  size = 100,
  ...
)
```

## Arguments

- session:

  A session from \[\`connect()\`\]\[picsure::connect\] on an authorized
  platform.

- genomicConceptPath:

  The genomic key, e.g. \`"Gene_with_variant"\` or
  \`"Variant_consequence_calculated"\`.

- query:

  Optional search term to narrow results (e.g. \`"BRCA"\`).

- page:

  1-based page number. A single positive whole number; a fractional or
  non-numeric value raises a \`picsureError\` rather than being silently
  truncated or turned into \`NA\`.

- size:

  Page size (values per call). Same validation as \`page\`.

- ...:

  Additional keyword arguments forwarded to the Python call.

## Value

A data.frame with a single character \`value\` column. (The Python
adapter's pagination metadata lives on the DataFrame's \`.attrs\` and
does not survive reticulate conversion; paginate via \`page\`/\`size\`.)
