# Wrap a Python call so Python exceptions surface as picsureErrors.

Any condition of class \`python.builtin.Exception\` thrown inside
\`expr\` is caught and re-raised as a \`picsureError\`. The message
comes from the Python exception (which the Python package already
crafted for researchers), and the original exception is attached as
\`\$py_cause\`. Non-Python R errors pass through unchanged.

## Usage

``` r
with_picsure_error(expr)
```

## Arguments

- expr:

  An expression, typically a reticulate method call.

## Value

The value of \`expr\` if no error occurred.
