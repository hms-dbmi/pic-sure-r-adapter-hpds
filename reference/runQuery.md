# Execute a query against a PIC-SURE session.

Runs the query tree produced by
\[\`buildQuery()\`\]\[picsure::buildQuery\] against the session's
resource and returns the result in the shape dictated by \`type\`:

## Usage

``` r
runQuery(session, query, type = "count", ...)
```

## Arguments

- session:

  A session object produced by \[\`connect()\`\]\[picsure::connect\].

- query:

  A clause group from \[\`buildQuery()\`\]\[picsure::buildQuery\].

- type:

  A \`QueryType\` member (e.g.
  \[\`QueryType\$COUNT\`\]\[picsure::QueryType\]) or a case-insensitive
  string: \`"count"\` (default), \`"participant"\`, \`"timestamp"\`, or
  \`"cross_count"\`.

- ...:

  Additional keyword arguments forwarded to the Python
  \`Session.runQuery()\` call.

## Value

For \`type = "count"\`, a Python \`CountResult\` object with \`\$value\`
(exact count, or \`NULL\` for obfuscated small cohorts), \`\$margin\`,
and \`\$cap\`. For \`type = "cross_count"\`, a dict-like mapping concept
paths to CountResults. For \`"participant"\` and \`"timestamp"\`, a
\`data.frame\`.

## Details

\- \`"count"\` — a \`CountResult\` object with \`\$value\` (exact count,
or \`NULL\` for obfuscated small cohorts), \`\$margin\`, and
\`\$cap\`. - \`"cross_count"\` — a dict-like mapping of concept paths to
\`CountResult\` objects. - \`"participant"\` — data.frame with one row
per matching participant across all SELECTed variables. -
\`"timestamp"\` — data.frame of participant-level timestamps for
longitudinal concepts.

## Examples

``` r
if (FALSE) { # \dontrun{
count <- picsure::runQuery(bdc, full_query, type = "count")
if (!is.null(count$value)) cat(count$value, "participants\n")

rows <- picsure::runQuery(bdc, full_query, type = "participant")
} # }
```
