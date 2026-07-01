# Variant population-frequency buckets.

Pass a member to
\[\`buildGenomicFilter()\`\]\[picsure::buildGenomicFilter\]'s \`values\`
argument for the \`"Variant_frequency_as_text"\` key. Mirrors Python's
\`picsure.VariantFrequency\`.

## Usage

``` r
VariantFrequency
```

## Format

A list of \`picsure_enum_member\` objects:

- \`RARE\`:

  Rare variants.

- \`COMMON\`:

  Common variants.

- \`NOVEL\`:

  Novel variants.

## Examples

``` r
if (FALSE) { # \dontrun{
picsure::buildGenomicFilter(
  "Variant_frequency_as_text",
  values = picsure::VariantFrequency$RARE
)
} # }
```
