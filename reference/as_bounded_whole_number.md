# Validate a single whole number no smaller than \`minimum\`.

The shared body of \[as_positive_whole_number()\] and
\[as_nonnegative_whole_number()\]. Returns an integer, so reticulate
hands Python an \`int\` rather than the \`float\` a bare R number
becomes.

## Usage

``` r
as_bounded_whole_number(value, arg, minimum, call)
```

## Arguments

- value:

  The value to validate.

- arg:

  The argument's name, for the error message.

- minimum:

  The smallest accepted value, 0 or 1.

- call:

  The call to report in the error.

## Value

\`value\` as an integer scalar.
