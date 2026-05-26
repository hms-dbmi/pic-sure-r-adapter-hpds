# Phenotypic filter clause types.

Pass a member to \[\`buildClause()\`\]\[picsure::buildClause\]'s
\`type\` argument. Mirrors Python's \`picsure.PhenotypicFilterType\`.

## Usage

``` r
PhenotypicFilterType
```

## Format

A list of \`picsure_enum_member\` objects:

- \`FILTER\`:

  Filter by categorical values or numeric range.

- \`ANYRECORD\`:

  Match records where the concept path \*or any descendant\* has a value
  (wire: \`ANY_RECORD_OF\`).

- \`REQUIRE\`:

  Require the concept path to have a non-null value (wire:
  \`REQUIRED\`).

## Details

To include concept paths in query output without filtering, use
\[\`buildQuery()\`\]\[picsure::buildQuery\]'s \`includeConcepts\`
argument — output columns are no longer a clause type.

## Examples

``` r
if (FALSE) { # \dontrun{
picsure::buildClause(
  "\\phs1\\pht1\\phv1\\sex\\",
  type = picsure::PhenotypicFilterType$FILTER,
  categories = "male"
)
} # }
```
