# Connect to a PIC-SURE instance.

Opens a session against the named PIC-SURE platform using the supplied
personal token. Platforms are listed by \`picsure::platforms()\`.

## Usage

``` r
connect(platform, token, ...)
```

## Arguments

- platform:

  A \`Platform\` member (e.g.
  \[\`Platform\$BDC_OPEN\`\]\[picsure::Platform\]) or a platform-label
  string (e.g. \`"BDC Open"\`, \`"BDC Authorized"\`).

- token:

  Your personal PIC-SURE access token, obtained from the "User Profile"
  tab of your PIC-SURE instance.

- ...:

  Optional keyword arguments forwarded to the Python
  \`picsure.connect()\` call. Unknown keys raise a \`picsureError\` with
  the list of valid keys. Supported keys:

  \`resource_uuid\`

  :   UUID of a specific PIC-SURE resource to connect to; overrides the
      platform default.

  \`include_consents\`

  :   Logical. When \`TRUE\`, the session retrieves the user's consent
      metadata from the auth service. Defaults to the Python adapter's
      choice (currently \`TRUE\`).

  \`requires_auth\`

  :   Logical. When \`FALSE\`, the session is opened in unauthenticated
      mode (only useful for open resources). Defaults to the Python
      adapter's choice (currently \`TRUE\`).

## Value

An opaque session object. Pass it as the first argument to
\`picsure::dictionarySearch()\`, \`picsure::runQuery()\`, and friends.

## Details

On the first call of the R session, reticulate provisions an isolated
Python environment containing the \`picsure\` Python package. This may
take a few seconds the first time; subsequent calls reuse the cached
env.

## Examples

``` r
if (FALSE) { # \dontrun{
bdc <- picsure::connect(platform = "BDC Authorized", token = my_token)
} # }
```
