# Connect to a PIC-SURE instance.

Opens a session against the named PIC-SURE platform using the supplied
personal token. See \[\`Platform\`\]\[picsure::Platform\] for the known
platform members.

## Usage

``` r
connect(platform, token = "", ...)
```

## Arguments

- platform:

  A \`Platform\` member (e.g.
  \[\`Platform\$BDC_AUTHORIZED\`\]\[picsure::Platform\]) or a full URL
  string for an unlisted deployment (e.g.
  \`"https://my-picsure.example.com"\`). Human-readable label strings
  such as \`"BDC Authorized"\` are \*\*not\*\* accepted and raise a
  \`picsureError\`.

- token:

  Your personal PIC-SURE access token, obtained from the "User Profile"
  tab of your PIC-SURE instance. Optional for open-access platforms
  (\`Platform\$BDC_OPEN\`, \`Platform\$BDC_DEV_OPEN\`,
  \`Platform\$BDC_PREDEV_OPEN\`, \`Platform\$NHANES_OPEN\`); required
  for authenticated platforms. Defaults to \`""\`.

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

  \`supports_genomic\`

  :   Logical. Whether genomic operations (\`searchGenomicValues()\`,
      genomic-filtered queries) are permitted. Defaults to the
      platform's own flag (\`TRUE\` only for BDC authorized platforms);
      pass \`TRUE\` for a custom URL that serves genomic data.

  \`client_type\`

  :   Identifier sent to the backend audit log as the \`X-Client-Type\`
      header. Defaults to \`"R_ADAPTER"\`; you should not normally need
      to override it.

## Value

An opaque session object. Pass it as the first argument to
\`picsure::searchDictionary()\`, \`picsure::runQuery()\`, and friends.

## Details

On the first call of the R session, reticulate provisions an isolated
Python environment containing the \`picsure\` Python package. This may
take a few seconds the first time; subsequent calls reuse the cached
env.

## Examples

``` r
if (FALSE) { # \dontrun{
bdc <- picsure::connect(platform = picsure::Platform$BDC_AUTHORIZED, token = my_token)
open <- picsure::connect(platform = picsure::Platform$BDC_OPEN)
} # }
```
