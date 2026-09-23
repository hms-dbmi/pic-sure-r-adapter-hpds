# Wrap a Python call so Python exceptions surface as picsureErrors.

Any condition of class \`python.builtin.Exception\` thrown inside
\`expr\` is caught and re-raised as a \`picsureError\`, or as the
specialized subclass matching the Python exception's own class. The
message is the Python package's own sentence with reticulate's class
prefix and \`py_last_error()\` footer stripped; the original condition
is attached as \`\$py_cause\` and the Python class name as
\`\$python_class\`. Set \`options(picsure.python_detail = TRUE)\` to put
the Python class back into the message. Non-Python R errors pass through
unchanged.

## Usage

``` r
with_picsure_error(expr)
```

## Arguments

- expr:

  An expression, typically a reticulate method call.

## Value

The value of \`expr\` if no error occurred.
