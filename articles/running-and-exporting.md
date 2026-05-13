# Running and exporting

Given a query built with
[`createSubQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/createSubQuery.md) +
[`buildQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildQuery.md),
you can ask the server for:

- a participant count (possibly obfuscated for small cohorts),
- a participant-level data frame,
- a timestamped longitudinal data frame,
- a PFB file written directly to disk.

CSV and TSV output work differently: they serialize a DataFrame you
already have, so the flow is “runQuery first, then write”.

## Run for a count

``` r

count <- picsure::runQuery(bdc, full_query, type = "count")
```

`count` is a `CountResult` object with three fields:

- `count$value` — the exact count, or `NULL` for obfuscated small
  cohorts.
- `count$margin` — the server-side margin for the count (when reported).
- `count$cap` — the upper bound used when a count is obfuscated.

``` r

if (!is.null(count$value)) {
  cat(count$value, "participants\n")
} else {
  cat("fewer than", count$cap, "participants\n")
}
```

## Cross counts

Pass `type = "cross_count"` to receive a dict-like object keyed by
concept path, each value a `CountResult`.

``` r

cc <- picsure::runQuery(bdc, full_query, type = "cross_count")
```

## Pull participant rows

``` r

participants <- picsure::runQuery(bdc, full_query, type = "participant")
nrow(participants)
head(participants)
```

## Timestamped data

``` r

timeseries <- picsure::runQuery(bdc, full_query, type = "timestamp")
```

## Export to disk

[`exportAsPFB()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/exportAsPFB.md)
runs the query and writes the PFB file in a single call:

``` r

picsure::exportAsPFB(bdc, full_query, "~/cohort.pfb")
```

PFB export requires the Python `picsure[pfb]` optional dependency. If
the Python env was provisioned without it,
[`exportAsPFB()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/exportAsPFB.md)
raises a `picsureError` pointing you at the install hint.

[`exportCSV()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/exportCSV.md)
and
[`exportTSV()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/exportTSV.md)
serialize an R `data.frame` — the typical flow is to run the query first
and then write the result:

``` r

df <- picsure::runQuery(bdc, full_query, type = "participant")
picsure::exportCSV(bdc, df, "~/cohort.csv")
picsure::exportTSV(bdc, df, "~/cohort.tsv")
```

All three export functions return the path invisibly, so you can chain:

``` r

pfb_path <- picsure::exportAsPFB(bdc, full_query, tempfile(fileext = ".pfb"))
```

## Reload a previously-saved query

If you have the UUID of a previously-saved query,
[`loadQueryByID()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/loadQueryByID.md)
fetches the saved body from the backend and rebuilds it as a Clause /
ClauseGroup that drops straight back into
[`runQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/runQuery.md)
or any of the export functions:

``` r

previous <- picsure::loadQueryByID(bdc, "11111111-2222-3333-4444-555555555555")
count <- picsure::runQuery(bdc, previous, type = "count")
```

The loaded handle is the same shape as one built with
[`buildQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildQuery.md),
so you can also nest it inside a new group to refine the cohort.
