# Validate a single positive whole number.

Rejects \`NA\`, \`NaN\`, \`Inf\`, a fraction, anything below 1, anything
above \`.Machine\$integer.max\`, non-numeric input, and any length other
than one, so \`page = 1.7\` is an error rather than a silent truncation
to 1. Each rejection names the argument and what arrived, and raises a
\`picsureValidationError\`, which is a \`picsureError\`.

## Usage

``` r
as_positive_whole_number(value, arg, call = sys.call(-1L))
```

## Arguments

- value:

  The value to validate.

- arg:

  The argument's name, for the error message.

- call:

  The call to report in the error, by default the caller's, so the
  rejection names the wrapper the researcher invoked.

## Value

\`value\` as an integer scalar.
