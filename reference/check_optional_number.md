# Validate an optional single finite number, leaving its type alone.

For continuous bounds, where a negative or fractional value is
legitimate but \`NA\`, \`Inf\`, a string, and a vector are not. \`NULL\`
passes through so the Python default applies. The value is returned
unchanged rather than coerced: an R integer must stay an integer so
reticulate hands Python an \`int\` rather than a \`float\`.

## Usage

``` r
check_optional_number(value, arg, call = sys.call(-1L))
```

## Arguments

- value:

  The value to validate, or NULL.

- arg:

  The argument's name, for the error message.

- call:

  The call to report in the error, by default the caller's, so the
  rejection names the wrapper the researcher invoked.

## Value

\`value\` unchanged.
