# Changelog

## picsure 2.0.0

First release of the rewritten R adapter. This is a clean break from the
1.x adapter: the public API is entirely new, there are no deprecation
shims, and every query now targets the PIC-SURE **v3 query API**. See
[`vignette("migrating-from-v1")`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/articles/migrating-from-v1.md)
for a function-by-function mapping from the 1.x adapter to 2.0.0.

### What changed

- **Architecture.** The package is now a thin wrapper over the Python
  `picsure` adapter via `reticulate`. The required Python environment is
  provisioned automatically on first use.
- **Connecting.** `connect(platform, token)` replaces the 1.x
  `initializeSession()` / `setResource()` calls. Platforms are selected
  with the `Platform` enum (e.g. `Platform$BDC_AUTHORIZED`).
- **Query construction.** Build nested AND/OR cohorts with
  [`buildClause()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildClause.md),
  [`buildClauseGroup()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildClauseGroup.md),
  and
  [`buildQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildQuery.md).
  Output concepts are attached via `buildQuery(includeConcepts = ...)`
  rather than the 1.x `SELECT` clause.
- **Query editing.**
  [`removeSubQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/removeSubQuery.md)
  and
  [`replaceClause()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/replaceClause.md)
  edit an existing query tree by structural match.
- **Execution.** `runQuery(session, query, type = ...)` returns a
  `CountResult` (count), a named list of `CountResult`s (cross-count),
  or a data frame (participant / timestamp).
  [`runQueryByID()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/runQueryByID.md),
  [`loadQueryByID()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/loadQueryByID.md),
  and
  [`saveQueryByName()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/saveQueryByName.md)
  cover saved queries.
- **Search.**
  [`searchDictionary()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/searchDictionary.md)
  searches the data dictionary;
  [`facets()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/facets.md),
  [`addFacet()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/addFacet.md),
  and
  [`removeFacet()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/removeFacet.md)
  build facet filters.
- **Export.**
  [`exportAsPFB()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/exportAsPFB.md),
  [`exportCSV()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/exportCSV.md),
  and
  [`exportTSV()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/exportTSV.md).
- **Errors.** Failures surface as `picsureError` R conditions carrying
  researcher-facing messages, replacing the 1.x adapter’s raw `httr`
  errors.
- **Enums.** `PhenotypicFilterType`, `GroupOperator`, `QueryType`, and
  `Platform` mirror the Python adapter’s enums.

The legacy 1.x adapter remains available on the `main` branch during the
migration window.
