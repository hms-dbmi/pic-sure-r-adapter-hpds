# Filter clause types.

Pass a member to \[\`createSubQuery()\`\]\[picsure::createSubQuery\]'s
\`type\` argument. Mirrors Python's \`picsure.ClauseType\`.

## Usage

``` r
ClauseType
```

## Format

A list of \`picsure_enum_member\` objects:

- \`FILTER\`:

  Filter by categorical values or numeric range.

- \`ANYRECORD\`:

  Match records where the concept path \*or any descendant\* has a value
  (wire: \`ANY_RECORD_OF\`).

- \`SELECT\`:

  Include the concept path(s) in query output.

- \`REQUIRE\`:

  Require the concept path to have a non-null value (wire:
  \`REQUIRED\`).

## Examples

``` r
if (FALSE) { # \dontrun{
picsure::createSubQuery(
  "\\phs1\\pht1\\phv1\\sex\\",
  type = picsure::ClauseType$FILTER,
  categories = "male"
)
} # }
```
