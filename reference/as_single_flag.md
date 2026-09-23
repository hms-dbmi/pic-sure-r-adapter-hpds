# Validate a single TRUE or FALSE.

The package's only logical-scalar check. \`NA\`, a length-2 logical, a
number, and the string \`"TRUE"\` are all rejected, so a flag is never
silently coerced into one. Used for \`connect()\`'s \`dev_mode\`
setting, which can arrive as an argument or as an R option, and for
\`saveQueryByName()\`'s \`overwrite\`.

## Usage

``` r
as_single_flag(value, label, call = sys.call(-1L))
```

## Arguments

- value:

  The value to validate.

- label:

  How the value is named in the error message, already formatted.
  Argument sites pass a backticked name such as “ "\`overwrite\`" “. A
  setting that can also come from an R option passes
  \`"options(picsure.dev_mode)"\` when the option supplied it, which is
  why this takes a rendered label rather than a bare argument name.

- call:

  The call to report in the error, by default the caller's, so the
  rejection names the wrapper the researcher invoked.

## Value

\`value\` unchanged.
