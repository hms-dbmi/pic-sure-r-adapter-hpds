# Type a result data frame from its known schema.

Applies a declared column type to every column the schema names and the
frame actually has. Columns the schema does not name are left alone, so
a server-driven column set (a participant result's concept-path columns,
or a deployment-specific dictionary field) survives untouched, and no
column is added, dropped, or reordered.

## Usage

``` r
apply_result_schema(data, schema)
```

## Arguments

- data:

  A data frame, or any other value (returned unchanged).

- schema:

  A named character vector mapping column name to one of
  \`"character"\`, \`"numeric"\`, \`"integer"\`, \`"logical"\`, or
  \`"list"\`.

## Value

\`data\` with the named columns retyped.
