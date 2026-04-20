# picsure 0.1.0 (development)

First release of the rewritten `picsure` R adapter. Breaking changes from
v1 — see `vignettes/migrating-from-v1.Rmd` for the migration guide.

## Breaking changes

- Every v1 function is removed. There are no deprecation shims.
- `initializeSession(url, token)` → `connect(platform, token)`. The `url` /
  `psama_url` pair is replaced by a `platform` enum string; call
  `connect(platform = "BDC Authorized", ...)` or
  `connect(platform = "Demo", ...)`.
- `newQuery(session)` → `createClause(path, type, ...)` +
  `buildClauseGroup(clauses, root)`. Query construction is now declarative
  and supports nested AND/OR trees.
- `addClause(q, ...)` → compose clauses via `createClause()` and combine
  them via `buildClauseGroup()`.
- `runQuery(q, "COUNT"|"DATA_FRAME")` → `runQuery(session, query, type = "count"|"participant"|"timestamp")`.
- `bdc.*` prefix is removed. Network-specific behavior is keyed on the
  `platform` argument to `connect()`.

## New features

- **Facet filters.** `facets(session)` + `addFacet()` / `removeFacet()`.
- **Nested query trees.** AND / OR groups can be nested arbitrarily.
- **PFB export.** `exportPFB(session, query, path)` (requires
  `picsure[pfb]` on the Python side).
- **Case-insensitive enum strings.** Pass `"filter"`, `"Filter"`, or
  `"FILTER"` for `ClauseType`; same for `GroupOperator` (`"AND"` / `"OR"`).

## Infrastructure

- R floor raised to 4.1.
- Single runtime dependency: `reticulate` ≥ 1.41.
- Python environment provisioned on first call via `reticulate::py_require()`.
- Test suite split into unit tier (mocked Python, on every PR) and
  integration tier (`PICSURE_INTEGRATION=1`, nightly).
- pkgdown site auto-deployed from `main`.
