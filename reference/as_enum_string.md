# Resolve a string OR a typed enum member to its string identifier.

Used by \`to_py_enum()\` and any other site that needs a string
representation of either a \`picsure_enum_member\` or a plain string.
Members are required to match \`expected_subclass\` to prevent passing a
\`PhenotypicFilterType\` member where a \`GroupOperator\` is expected.

## Usage

``` r
as_enum_string(
  value,
  expected_subclass,
  enum_name,
  field = "name",
  call = sys.call(-1L)
)
```

## Arguments

- value:

  NULL, a single string, or a \`picsure_enum_member\`.

- expected_subclass:

  The required \`picsure\_\*\` subclass (e.g.
  \`"picsure_phenotypic_filter_type"\`).

- enum_name:

  Human-readable enum name for error messages (e.g.
  \`"PhenotypicFilterType"\`).

- field:

  For members, which field to extract: \`"name"\` (default) or
  \`"value"\`.

- call:

  The call to report in the error, by default the caller's, so the
  rejection names the wrapper the researcher invoked.

## Value

NULL if \`value\` is NULL; otherwise a character scalar.
