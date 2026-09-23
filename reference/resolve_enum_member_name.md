# Resolve a case-insensitive string OR a typed enum member to the Python enum's own spelling of the member name.

The R API accepts strings like "FILTER", "and", or members like
\[\`picsure::PhenotypicFilterType\$FILTER\`\]\[picsure::PhenotypicFilterType\].
Resolution goes through the enum's \`\_\_members\_\_\` map, so only a
real member name can come back. This is the single place the package
decides what an enum-shaped argument means: a wrapper that needs both
the Python member and a decision of its own about which member was asked
for calls this once and uses the answer for both, rather than resolving
twice by two rules.

## Usage

``` r
resolve_enum_member_name(
  value,
  enum_obj,
  enum_name,
  expected_subclass,
  call = sys.call(-1L)
)
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

NULL if \`value\` is NULL; otherwise the member name as the enum itself
spells it.
