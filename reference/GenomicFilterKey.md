# Genomic annotation keys for \`buildGenomicFilter()\`.

Pass a member to
\[\`buildGenomicFilter()\`\]\[picsure::buildGenomicFilter\]'s \`key\`
argument, or the equivalent string (validated by the Python adapter).
Mirrors Python's \`picsure.GenomicFilterKey\`.

## Usage

``` r
GenomicFilterKey
```

## Format

A list of \`picsure_enum_member\` objects:

- \`GENE_WITH_VARIANT\`:

  Gene affected by a variant.

- \`VARIANT_CONSEQUENCE_CALCULATED\`:

  Calculated variant consequence.

- \`VARIANT_FREQUENCY_AS_TEXT\`:

  Population-frequency bucket (see
  \[\`VariantFrequency\`\]\[picsure::VariantFrequency\]).

- \`VARIANT_CLASS\`:

  Variant class.

- \`VARIANT_SEVERITY\`:

  Virtual severity key (see
  \[\`VariantSeverity\`\]\[picsure::VariantSeverity\]).

## Details

\`VARIANT_SEVERITY\` is a virtual key: the builder expands a
\[\`VariantSeverity\`\]\[picsure::VariantSeverity\] bucket into the
matching \`Variant_consequence_calculated\` values.

## Examples

``` r
if (FALSE) { # \dontrun{
picsure::buildGenomicFilter(
  picsure::GenomicFilterKey$GENE_WITH_VARIANT,
  values = c("BRCA1")
)
} # }
```
