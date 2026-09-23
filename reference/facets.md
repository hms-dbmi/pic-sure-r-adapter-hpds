# Build a FacetSet for narrowing search results.

Returns an opaque \`FacetSet\` handle tied to this session. Add entries
with \[\`addFacet()\`\]\[picsure::addFacet\] and remove them with
\[\`removeFacet()\`\]\[picsure::removeFacet\]. Pass the final FacetSet
to \[\`searchDictionary()\`\]\[picsure::searchDictionary\] via the
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

## Details

The handle is created from the categories the \*deployment\* publishes,
so the valid category names come from the server rather than from this
package. The dictionary ETL creates two on every deployment:
\`"dataset_id"\`, the study or dataset (e.g. a dbGaP accession), and
\`"data_type"\`, \`"categorical"\` or \`"continuous"\`. Deployments with
genomic data also get \`"data_source"\`. A category the server did not
publish raises a \`picsureError\` listing the ones it did.

## Examples

``` r
if (FALSE) { # \dontrun{
fs <- picsure::facets(bdc)
fs <- picsure::addFacet(fs, "dataset_id", "phs000007")
picsure::searchDictionary(bdc, "sex", facets = fs)
} # }
```
