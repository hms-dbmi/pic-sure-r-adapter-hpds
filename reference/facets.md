# Build a FacetSet for narrowing search results.

Returns an opaque \`FacetSet\` handle tied to this session. Add entries
with \[\`addFacet()\`\]\[picsure::addFacet\] and remove them with
\[\`removeFacet()\`\]\[picsure::removeFacet\]. Pass the final FacetSet
to \[\`dictionarySearch()\`\]\[picsure::dictionarySearch\] via the
\`facets\` argument.

## Usage

``` r
facets(session)
```

## Arguments

- session:

  A session object produced by \[\`connect()\`\]\[picsure::connect\].

## Value

An opaque FacetSet handle.

## Examples

``` r
if (FALSE) { # \dontrun{
fs <- picsure::facets(bdc)
fs <- picsure::addFacet(fs, "study_ids", "phs000007")
picsure::dictionarySearch(bdc, "sex", facets = fs)
} # }
```
