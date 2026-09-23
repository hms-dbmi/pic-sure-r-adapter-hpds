# Variant severity buckets for the virtual \`Variant_severity\` key.

Pass a member to
\[\`buildGenomicFilter()\`\]\[picsure::buildGenomicFilter\]'s \`values\`
argument together with
\[\`GenomicFilterKey\$VARIANT_SEVERITY\`\]\[picsure::GenomicFilterKey\].
The builder expands each bucket to the matching
\`Variant_consequence_calculated\` values. Mirrors Python's
\`picsure.VariantSeverity\`.

## Usage

``` r
VariantSeverity
```

## Format

A list of \`picsure_enum_member\` objects:

- \`HIGH\`:

  High-severity consequences.

- \`MEDIUM\`:

  Medium-severity consequences.

- \`LOW\`:

  Low-severity consequences.

## Examples

``` r
if (FALSE) { # \dontrun{
picsure::buildGenomicFilter(
  picsure::GenomicFilterKey$VARIANT_SEVERITY,
  values = picsure::VariantSeverity$HIGH
)
} # }
```
