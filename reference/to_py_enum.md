# Resolve a case-insensitive string OR a typed enum member against a Python enum proxy.

The R API accepts strings like "FILTER", "and", or members like
\[\`picsure::ClauseType\$FILTER\`\]\[picsure::ClauseType\] and maps them
to the Python enum member at call time.

## Usage

``` r
to_py_enum(value, enum_obj, enum_name, expected_subclass)
```

## Arguments

- value:

  NULL, a single string, or a \`picsure_enum_member\`.

- enum_obj:

  The Python enum proxy (or a named list in tests).

- enum_name:

  The enum name, used only in error messages.

- expected_subclass:

  The required \`picsure\_\*\` subclass for member inputs. Members of
  other subclasses are rejected before any proxy lookup happens.

## Value

NULL if value is NULL; otherwise the corresponding enum member.
