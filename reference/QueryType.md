# Query result types for \`runQuery()\`.

Pass a member to \[\`runQuery()\`\]\[picsure::runQuery\]'s \`type\`
argument. Mirrors Python's \`picsure.QueryType\`.

## Usage

``` r
QueryType
```

## Format

A list of \`picsure_enum_member\` objects:

- \`COUNT\`:

  Returns a \`CountResult\` with the matching-participant count (or NULL
  on small-cohort obfuscation).

- \`PARTICIPANT\`:

  Returns a data.frame with one row per matching participant.

- \`TIMESTAMP\`:

  Returns a data.frame of participant-level timestamps for longitudinal
  concepts.

- \`CROSS_COUNT\`:

  Returns a list of \`CountResult\`s keyed by concept path.

## Examples

``` r
if (FALSE) { # \dontrun{
picsure::runQuery(session, query, type = picsure::QueryType$COUNT)
} # }
```
