# Validate a single non-negative whole number.

The same checks as \[as_positive_whole_number()\], except that 0 is
accepted. For zero-based positions such as \`searchDictionary()\`'s
\`page\`.

## Usage

``` r
as_nonnegative_whole_number(value, arg, call = sys.call(-1L))
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
