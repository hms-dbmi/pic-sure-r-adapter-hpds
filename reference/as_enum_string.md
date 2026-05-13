# Resolve a string OR a typed enum member to its string identifier.

Used by \`to_py_enum()\` and any other site that needs a string
representation of either a \`picsure_enum_member\` or a plain string.
Members are required to match \`expected_subclass\` to prevent passing a
\`ClauseType\` member where a \`GroupOperator\` is expected.

## Usage

``` r
as_enum_string(value, expected_subclass, enum_name, field = "name")
```

## Arguments

- value:

  NULL, a single string, or a \`picsure_enum_member\`.

- expected_subclass:

  The required \`picsure\_\*\` subclass (e.g.
  \`"picsure_clause_type"\`).

- enum_name:

  Human-readable enum name for error messages (e.g. \`"ClauseType"\`).

- field:

  For members, which field to extract: \`"name"\` (default) or
  \`"value"\`.

## Value

NULL if \`value\` is NULL; otherwise a character scalar.
