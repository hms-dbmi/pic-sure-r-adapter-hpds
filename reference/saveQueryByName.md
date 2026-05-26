# Save a query to the authenticated user's profile and return its query ID.

Submits the query to PIC-SURE (creating a server-side query record),
then associates \`name\` with it via the \`/dataset/named/\` endpoint.
The returned UUID can later be passed to
\[\`loadQueryByID()\`\]\[picsure::loadQueryByID\] or
\[\`runQueryByID()\`\]\[picsure::runQueryByID\]. Not supported on
open-access platforms.

## Usage

``` r
saveQueryByName(session, query, name, overwrite = FALSE)
```

## Arguments

- session:

  A session object produced by \[\`connect()\`\]\[picsure::connect\].

- query:

  A clause or clause-group handle.

- name:

  A non-empty character scalar. Allowed characters: letters, digits,
  spaces, and \`- \_ \\ / ? + = \[ \] . ( ) : " '\`. Max 255 chars.

- overwrite:

  Logical. When \`FALSE\` (default), errors if a named query with
  \`name\` already exists for this user. When \`TRUE\`, the existing
  record is updated to point at the freshly-submitted query.

## Value

A character scalar – the PIC-SURE query ID.

## Examples

``` r
if (FALSE) { # \dontrun{
qid <- picsure::saveQueryByName(bdc, my_query, "Cohort 2026-Q2")
qid <- picsure::saveQueryByName(bdc, my_query, "Cohort 2026-Q2", overwrite = TRUE)
later <- picsure::loadQueryByID(bdc, qid)
} # }
```
