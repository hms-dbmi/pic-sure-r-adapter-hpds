# Known PIC-SURE deployment platforms.

Pass a member to \[\`connect()\`\]\[picsure::connect\]'s \`platform\`
argument. Mirrors Python's \`picsure.Platform\`. Each member exposes the
connection URL, default resource UUID, label, and policy flags.

## Usage

``` r
Platform
```

## Format

A list of \`picsure_enum_member\` (subclass \`picsure_platform\`)
objects:

- \`BDC_AUTHORIZED\`:

  BDC production, authenticated.

- \`BDC_OPEN\`:

  BDC production, open.

- \`BDC_DEV_AUTHORIZED\`:

  BDC dev, authenticated.

- \`BDC_DEV_OPEN\`:

  BDC dev, open.

- \`BDC_PREDEV_AUTHORIZED\`:

  BDC predev, authenticated.

- \`BDC_PREDEV_OPEN\`:

  BDC predev, open.

- \`NHANES_AUTHORIZED\`:

  NHANES, authenticated.

- \`NHANES_OPEN\`:

  NHANES, open.

## Examples

``` r
if (FALSE) { # \dontrun{
picsure::connect(platform = picsure::Platform$BDC_OPEN, token = "")
} # }
```
