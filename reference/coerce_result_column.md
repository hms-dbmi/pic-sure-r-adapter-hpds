# Coerce one result column to a declared type.

A list column reaching a scalar type means the server sent a nested
value where the schema expects one per row. Those cells are collapsed to
a single string rather than deparsed, which is what \`as.character()\`
on a list would do.

## Usage

``` r
coerce_result_column(column, type, name = "<unnamed>")
```

## Arguments

- column:

  The column as it arrived.

- type:

  One of \`"character"\`, \`"numeric"\`, \`"integer"\`, \`"logical"\`,
  \`"list"\`.

- name:

  The column's name, used in the warning.

## Value

The column, coerced.

## Details

A value that does not parse as the declared type becomes \`NA\`. The
result is still returned, and one warning per column names the column
and how many values were lost, so a server that starts sending text
where the schema expects a number is visible without breaking the call.

An empty cell in a column the schema declares \`"character"\` reaches R
as a floating-point \`NaN\`: \`pandas.read_csv\` infers an all-empty
column as \`float64\`, and reticulate hands that across as a
\`numeric\`. Those cells become \`NA_character\_\` rather than the
three-character string \`"NaN"\` that \`as.character()\` alone would
produce, so a timeseries \`TVAL_CHAR\` reads as missing whether or not
the rows happened to carry any text. Nothing is reported as lost,
because \`NaN\` already satisfies \`is.na()\`.
