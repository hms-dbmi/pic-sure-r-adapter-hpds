# Drop NULL entries from a list.

Used to normalize keyword-argument lists before handing them to Python:
reticulate maps R NULL to Python None, which bypasses Python defaults on
optional args. Drop NULLs first so Python sees its own defaults.

## Usage

``` r
drop_nulls(x)
```

## Arguments

- x:

  A list.

## Value

A list with all NULL-valued entries removed.
