# picsure 2.0.0

First release of the rewritten R adapter. This is a clean break from the
1.x adapter: the public API is entirely new, there are no deprecation
shims, and every query now targets the PIC-SURE **v3 query API**. See
`vignette("migrating-from-v1")` for a function-by-function mapping from
the 1.x adapter to 2.0.0.

## What changed

- **Architecture.** The package is now a thin wrapper over the Python
  `picsure` adapter via `reticulate`. The required Python environment is
  provisioned automatically on first use.
- **Connecting.** `connect(platform, token)` replaces the 1.x
  `initializeSession()` / `setResource()` calls. Platforms are selected
  with the `Platform` enum (e.g. `Platform$BDC_AUTHORIZED`).
- **Query construction.** Build nested AND/OR cohorts with
  `buildClause()`, `buildClauseGroup()`, and `buildQuery()`. Output
  concepts are attached via `buildQuery(includeConcepts = ...)` rather
  than the 1.x `SELECT` clause.
- **Query editing.** `removeSubQuery()` and `replaceClause()` edit an
  existing query tree by structural match.
- **Execution.** `runQuery(session, query, type = ...)` returns a
  `CountResult` (count), a named list of `CountResult`s (cross-count),
  or a data frame (participant / timestamp). `runQueryByID()`,
  `loadQueryByID()`, and `saveQueryByName()` cover saved queries.
- **Search.** `searchDictionary()` searches the data dictionary;
  `facets()`, `addFacet()`, and `removeFacet()` build facet filters.
- **Export.** `exportAsPFB()`, `exportCSV()`, and `exportTSV()`.
- **Errors.** Failures surface as `picsureError` R conditions carrying
  researcher-facing messages, replacing the 1.x adapter's raw `httr`
  errors.
- **Enums.** `PhenotypicFilterType`, `GroupOperator`, `QueryType`, and
  `Platform` mirror the Python adapter's enums.

The legacy 1.x adapter remains available on the `main` branch during the
migration window.
