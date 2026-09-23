# Execute a query against a PIC-SURE session.

Runs a query — a clause/clause-group from
\[\`buildClause()\`\]\[picsure::buildClause\] /
\[\`buildClauseGroup()\`\]\[picsure::buildClauseGroup\], or a Query from
\[\`buildQuery()\`\]\[picsure::buildQuery\] — against the session's
resource and returns the result in the shape dictated by \`type\`:

## Usage

``` r
runQuery(session, query, type = "count", ...)
```

## Arguments

- session:

  A session object produced by \[\`connect()\`\]\[picsure::connect\].

- query:

  A clause/clause-group handle (from
  \[\`buildClause()\`\]\[picsure::buildClause\] /
  \[\`buildClauseGroup()\`\]\[picsure::buildClauseGroup\]) or a Query
  handle (from \[\`buildQuery()\`\]\[picsure::buildQuery\]).

- type:

  A \`QueryType\` member (e.g.
  \[\`QueryType\$COUNT\`\]\[picsure::QueryType\]) or a case-insensitive
  string: \`"count"\` (default), \`"participant"\`, \`"timestamp"\`,
  \`"cross_count"\`, \`"variant_count"\`, \`"variant_list"\`,
  \`"vcf_excerpt"\`, or \`"aggregate_vcf_excerpt"\`.

- ...:

  Additional keyword arguments forwarded to the Python
  \`Session.runQuery()\` call.

## Value

For \`type = "count"\` or \`"variant_count"\`, a Python \`CountResult\`
object with \`\$value\` (exact count, or \`NULL\` for obfuscated small
cohorts), \`\$margin\`, and \`\$cap\`. For \`type = "cross_count"\`, a
named list of \`CountResult\`s keyed by concept path. For
\`"participant"\`, \`"timestamp"\`, \`"vcf_excerpt"\`, and
\`"aggregate_vcf_excerpt"\`, a \`data.frame\`. For \`"variant_list"\`, a
character vector.

A \`"timestamp"\` result is typed from the fixed timeseries schema HPDS
declares rather than inferred from the rows: \`PATIENT_NUM\` integer,
\`CONCEPT_PATH\` character, \`NVAL_NUM\` numeric, \`TVAL_CHAR\`
character, \`TIMESTAMP\` character. HPDS fills exactly one of
\`NVAL_NUM\` and \`TVAL_CHAR\` per row, so a query over numeric concepts
alone leaves \`TVAL_CHAR\` empty in every row, and inference used to
hand that column back as numeric. \`"participant"\` and the VCF-excerpt
results have one column per concept, so their columns are left as they
arrive.

## Details

\- \`"count"\` — a \`CountResult\` object with \`\$value\` (exact count,
or \`NULL\` for obfuscated small cohorts), \`\$margin\`, and
\`\$cap\`. - \`"cross_count"\`: a named list of \`CountResult\` objects
keyed by concept path. Reticulate converts the Python \`dict\`, and the
values stay Python objects. - \`"participant"\` — data.frame with one
row per matching participant across all included concepts. -
\`"timestamp"\` — data.frame of participant-level timestamps for
longitudinal concepts. - \`"variant_count"\` — a \`CountResult\` for the
number of distinct matching variants (preserving obfuscation, like
\`"count"\`). - \`"variant_list"\` - a character vector of variant spec
strings (not served by BDC primary environments yet). -
\`"vcf_excerpt"\` / \`"aggregate_vcf_excerpt"\` — a data.frame, one row
per variant (the aggregate form omits per-patient columns) (not served
by BDC primary environments yet).

## Examples

``` r
if (FALSE) { # \dontrun{
count <- picsure::runQuery(bdc, full_query, type = "count")
if (!is.null(count$value)) cat(count$value, "participants\n")

rows <- picsure::runQuery(bdc, full_query, type = "participant")
} # }
```
