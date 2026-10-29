# Load a saved PIC-SURE query by ID and execute it in one call.

Fetches a previously-saved query by UUID and runs it, as the type it was
saved as unless \`type\` names another. With a \`type\`, semantically
equivalent to \[\`loadQueryByID()\`\]\[picsure::loadQueryByID\] followed
by \[\`runQuery()\`\]\[picsure::runQuery\], but delegated to the Python
adapter as a single call. Returns the same result shapes as
\`runQuery()\`.

## Usage

``` r
runQueryByID(session, query_id, type = NULL)
```

## Arguments

- session:

  A session object produced by \[\`connect()\`\]\[picsure::connect\].

- query_id:

  The UUID string of a previously-saved query.

- type:

  A \`QueryType\` member (e.g.
  \[\`QueryType\$COUNT\`\]\[picsure::QueryType\]) or a case-insensitive
  string: \`"count"\`, \`"participant"\`, \`"timestamp"\`,
  \`"cross_count"\`, \`"variant_count"\`, \`"variant_list"\`,
  \`"vcf_excerpt"\`, or \`"aggregate_vcf_excerpt"\`. Defaults to the
  type the query was saved as, so a dataset saved for export from the
  PIC-SURE UI comes back as participant (or timestamp) rows and a query
  saved as a count is counted.

## Value

Same as \[\`runQuery()\`\]\[picsure::runQuery\]: a \`CountResult\` for
\`"count"\` or \`"variant_count"\`, a named list of \`CountResult\`s
keyed by concept path for \`"cross_count"\`, a \`data.frame\` for
\`"participant"\` / \`"timestamp"\` / \`"vcf_excerpt"\` /
\`"aggregate_vcf_excerpt"\` (not served by BDC primary environments
yet), or a character vector for \`"variant_list"\` (not served by BDC
primary environments yet).

## Examples

``` r
if (FALSE) { # \dontrun{
df    <- picsure::runQueryByID(bdc, "11111111-2222-3333-4444-555555555555")
count <- picsure::runQueryByID(bdc, "11111111-2222-3333-4444-555555555555", type = "count")
} # }
```
