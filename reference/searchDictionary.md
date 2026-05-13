# Search the PIC-SURE data dictionary.

Runs a keyword search against the session's dictionary resource and
returns the matching variables as a data frame. An empty \`term\`
returns every variable the session has access to.

## Usage

``` r
searchDictionary(session, term = "", facets = NULL, include_values = TRUE, ...)
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

- ...:

  Additional keyword arguments forwarded to the Python
  \`Session.searchDictionary()\` call.

## Value

A \`data.frame\` of matching dictionary entries.

## Examples

``` r
if (FALSE) { # \dontrun{
bdc <- picsure::connect(platform = "BDC Authorized", token = my_token)
picsure::searchDictionary(bdc, "sex")
picsure::searchDictionary(bdc, "")  # all variables
} # }
```
