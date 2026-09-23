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

- \`LOW_FREQUENCY\`:

  Low-frequency variants.

- \`ULTRA_RARE\`:

  Ultra-rare variants.

- \`NOVEL\`:

  Novel variants. Deprecated in the Python adapter: it is absent from
  every annotation set observed on a PIC-SURE deployment, and is kept
  only so existing code keeps working. Prefer a value returned by
  \`searchGenomicValues()\`.

## Details

The members are a convenience, not an allowlist. The real vocabulary is
whatever the deployment's variant annotations carry, so
\`searchGenomicValues(session, "Variant_frequency_as_text")\` is the
authoritative list, and \`buildGenomicFilter()\` accepts any string for
this key.

## Examples

``` r
if (FALSE) { # \dontrun{
picsure::buildGenomicFilter(
  "Variant_frequency_as_text",
  values = picsure::VariantFrequency$RARE
)
} # }
```
