# Validate a single non-empty string.

Checks length before content, so a vector argument raises a package
error naming the argument rather than base R's "'length = 2' in coercion
to 'logical(1)'" from an \`if()\` handed a vector.

## Usage

``` r
as_single_string(value, arg, hint = NULL, call = sys.call(-1L))
```

## Arguments

- value:

  The value to validate.

- arg:

  The argument's name, for the error message.

- hint:

  Optional sentence appended to the error message.

- call:

  The call to report in the error, by default the caller's, so the
  rejection names the wrapper the researcher invoked.

## Value

\`value\` unchanged.
