# Load a saved PIC-SURE query by ID and execute it in one call.

Fetches a previously-saved query by UUID and runs it. Semantically
equivalent to \[\`loadQueryByID()\`\]\[picsure::loadQueryByID\] followed
by \[\`runQuery()\`\]\[picsure::runQuery\], but delegated to the Python
adapter as a single call. Returns the same result shapes as
\`runQuery()\`.

## Usage

``` r
runQueryByID(session, query_id, type = "count")
```

## Arguments

- session:

  A session object produced by \[\`connect()\`\]\[picsure::connect\].

- query_id:

  The UUID string of a previously-saved query.

- type:

  A \`QueryType\` member (e.g.
  \[\`QueryType\$COUNT\`\]\[picsure::QueryType\]) or a case-insensitive
  string: \`"count"\` (default), \`"participant"\`, \`"timestamp"\`,
  \`"cross_count"\`, \`"variant_count"\`, \`"variant_list"\`,
  \`"vcf_excerpt"\`, or \`"aggregate_vcf_excerpt"\`.

## Value

Same as \[\`runQuery()\`\]\[picsure::runQuery\]: a \`CountResult\` for
\`"count"\` or \`"variant_count"\`, a dict-like mapping for
\`"cross_count"\`, a \`data.frame\` for \`"participant"\` /
\`"timestamp"\` / \`"vcf_excerpt"\` / \`"aggregate_vcf_excerpt"\` (not
served by BDC primary environments yet), or a character vector for
\`"variant_list"\` (not served by BDC primary environments yet).

## Examples

``` r
if (FALSE) { # \dontrun{
count <- picsure::runQueryByID(bdc, "11111111-2222-3333-4444-555555555555")
df    <- picsure::runQueryByID(bdc, "XXXXX-ID", type = "participant")
} # }
```
