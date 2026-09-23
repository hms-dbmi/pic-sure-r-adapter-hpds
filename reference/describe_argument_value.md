# Render a rejected argument value for an error message.

Says what actually arrived, so "\`page\` must be a single positive whole
number" is followed by the value that failed rather than leaving the
caller to guess which of several arguments was wrong.

## Usage

``` r
describe_argument_value(value)
```

## Arguments

- value:

  The rejected value, of any type.

## Value

A character scalar describing \`value\`, including for a non-atomic
value such as a data frame, a list, or a function.

## Details

Returns one string for any value, atomic or not. A one-column data frame
and a function both arrive here with length 1, and each breaks a scalar
result if treated as atomic: \`is.na()\` on a data frame returns one row
per row of the frame, and \`format()\` on a function returns one string
per deparsed line. Either way the rejection escapes as a base R
condition-length error, or as a condition whose message is a vector, and
the argument that was actually wrong goes unnamed.
