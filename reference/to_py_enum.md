# Resolve a case-insensitive string OR a typed enum member against a Python enum proxy.

\`resolve_enum_member_name()\` decides which member was asked for; this
fetches it. A caller that also needs the name should resolve the name
itself and use \[\`.py_enum_member()\`\] rather than call both.

## Usage

``` r
to_py_enum(value, enum_obj, enum_name, expected_subclass, call = sys.call(-1L))
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

- call:

  The call to report in the error, by default the caller's, so the
  rejection names the wrapper the researcher invoked. Threaded on to
  \`as_enum_string()\` as well, so a member of the wrong enum and an
  unknown member name report the same call.

## Value

NULL if value is NULL; otherwise the corresponding enum member.
