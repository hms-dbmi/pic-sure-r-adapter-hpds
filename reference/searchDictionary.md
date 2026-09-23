# Search the PIC-SURE data dictionary.

Runs a keyword search against the session's dictionary resource and
returns the matching variables as a data frame. An empty \`term\`
returns every variable the session has access to.

## Usage

``` r
searchDictionary(
  session,
  term = "",
  facets = NULL,
  include_values = TRUE,
  page = NULL,
  page_size = NULL,
  ...
)
```

## Arguments

- session:

  A session object produced by \[\`connect()\`\]\[picsure::connect\].

- term:

  Search string; empty string returns all variables.

- facets:

  NULL (default) or a \`FacetSet\` from
  \[\`facets()\`\]\[picsure::facets\]. When supplied, the server narrows
  results to variables inside the facets.

- include_values:

  If TRUE (default), variable values are included in the response. Set
  FALSE to omit them for a lighter payload.

- page:

  \`NULL\` (default) to return every match, or a zero-based page number
  to return that one page and nothing else. A single whole number, 0 or
  greater. It is sent to Python as an integer, which the adapter
  requires, so \`page = 0\` works as well as \`page = 0L\`. A
  fractional, negative, or non-numeric value raises a
  \`picsureValidationError\`.

- page_size:

  \`NULL\` (default) for the Python adapter's 500 rows per request, or a
  single positive whole number. With \`page\` set, it is the size of the
  page returned. Without, it is how many rows each request of the walk
  fetches. Validated and sent as an integer, like \`page\`.

- ...:

  Additional keyword arguments forwarded to the Python
  \`Session.searchDictionary()\` call.

## Value

A \`data.frame\` of matching dictionary entries, with column types fixed
by the dictionary schema whether or not the search matched anything:
\`conceptPath\`, \`name\`, \`display\`, \`description\`, \`dataType\`,
\`studyId\`, and \`studyAcronym\` are character, \`min\` and \`max\`
numeric, \`allowFiltering\` logical, and \`meta\` a list column.
\`values\` is a list column too, and is present only when
\`include_values = TRUE\`; with \`include_values = FALSE\` the adapter
never builds it and the returned frame has no such column. The rest of
the column set and its types are the same either way. A search that
matched nothing returns a zero-row frame with those same types, so
arithmetic on \`min\` / \`max\` behaves the same either way. The Python
adapter's paging metadata lives on the pandas DataFrame's \`.attrs\`,
which does not survive reticulate conversion.

## Details

With \`page\` left \`NULL\`, the Python adapter walks the server's
result pages and returns every match in one data frame. That unpaged
walk stops at 100,000 concepts: a search matching more raises a
\`picsureValidationError\` rather than loading them all. Read such a
result a page at a time instead, \`page = 0\`, then \`page = 1\`, and so
on, until a page comes back with fewer than \`page_size\` rows.
Narrowing \`term\` or \`facets\` also brings the match count down.

## Examples

``` r
if (FALSE) { # \dontrun{
bdc <- picsure::connect(platform = picsure::Platform$BDC_AUTHORIZED, token = my_token)
picsure::searchDictionary(bdc, "sex")
picsure::searchDictionary(bdc, "")  # all variables, up to 100,000
first_page <- picsure::searchDictionary(bdc, "", page = 0, page_size = 1000)
} # }
```
