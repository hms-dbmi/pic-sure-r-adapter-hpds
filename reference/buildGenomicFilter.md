# Create a single genomic (variant) filter.

Builds an opaque GenomicFilter handle for the \`genomicFilters\`
argument of \[\`buildQuery()\`\]\[picsure::buildQuery\]. A filter is
\*\*categorical\*\*: it matches when the annotation named by \`key\` is
one of \`values\`.

## Usage

``` r
buildGenomicFilter(key, values = NULL, ...)
```

## Arguments

- key:

  The genomic annotation to filter on: a
  \[\`GenomicFilterKey\`\]\[picsure::GenomicFilterKey\] member
  (preferred) or the equivalent string (validated Python-side).
  \`GenomicFilterKey\$VARIANT_SEVERITY\` is a virtual key — the Python
  adapter expands the requested
  \[\`VariantSeverity\`\]\[picsure::VariantSeverity\] buckets into
  \`Variant_consequence_calculated\` values. Variant-spec (SNP) keys are
  rejected.

- values:

  Required. Categorical value(s): a character vector, or
  \[\`VariantFrequency\`\]\[picsure::VariantFrequency\] members (coerced
  to their string value).

- ...:

  Additional keyword arguments forwarded to the Python
  \`picsure.buildGenomicFilter()\` call.

## Value

An opaque GenomicFilter handle.

## Examples

``` r
if (FALSE) { # \dontrun{
gene <- picsure::buildGenomicFilter("Gene_with_variant", values = c("BRCA1"))
rare <- picsure::buildGenomicFilter(
  "Variant_frequency_as_text", values = picsure::VariantFrequency$RARE
)
} # }
```
