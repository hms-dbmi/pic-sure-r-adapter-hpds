# Construct a picsureError condition.

Construct a picsureError condition.

## Usage

``` r
picsureError(message, py_cause = NULL)
```

## Arguments

- message:

  The user-facing error message. When the source is a Python
  PicSureError, use the Python-crafted message verbatim — Python already
  wrote it for researchers.

- py_cause:

  Optional; the original Python exception object. Stored on the
  condition as \`\$py_cause\` for advanced debugging via
  \`reticulate::py_last_error()\`.

## Value

A condition of class \`c("picsureError", "error", "condition")\`.

## Examples

``` r
if (FALSE) { # \dontrun{
tryCatch(
  picsure::connect(platform = "BDC Authorized", token = "bad-token"),
  picsureError = function(e) {
    message("PIC-SURE error: ", conditionMessage(e))
  }
)
} # }
```
