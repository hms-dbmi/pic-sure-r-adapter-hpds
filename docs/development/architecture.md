# Architecture

`picsure` is a thin R wrapper over the Python
[`picsure`](https://github.com/hms-dbmi/pic-sure-python-adapter-hpds)
package, wired through [`reticulate`](https://rstudio.github.io/reticulate/).
Every public R function delegates to the Python module after light
argument validation and type coercion. On first call, reticulate
provisions an isolated `uv`-backed virtualenv containing the upstream
Python package; subsequent calls reuse it.

## The reticulate bridge

The bridge lives in [`R/zzz.R`](../../R/zzz.R). The relevant moves:

- A package-private binding `picsure_py` starts as `NULL`. Every
  wrapper in the package refers to this binding (`picsure_py$connect`,
  `picsure_py$ClauseType`, etc.).
- `.onLoad()` calls `reticulate::py_require(.PICSURE_PY_SPEC)` to
  declare the Python dependency. The spec is a PEP 508 direct
  reference pointing at the upstream package's `main` branch (the
  Python package is not yet on PyPI); see the comment in `R/zzz.R`
  for the bump procedure.
- `.onLoad()` then assigns `picsure_py <<- reticulate::import("picsure",
  delay_load = TRUE)`. `delay_load = TRUE` is the load-time invariant:
  `library(picsure)` stays fast, and tests that never touch Python
  never trigger env creation. The Python env is materialized lazily on
  the first real attribute access on `picsure_py`.
- `.onAttach()` prints a one-line notice that first-call provisioning
  may take a few seconds.

reticulate stores the resolved virtualenv under uv's managed location
(by default under the user's uv cache directory). Inside the Docker
dev container the cache is a named volume — see
[`docker.md`](docker.md#whats-mounted).

Tests stub the Python boundary by swapping the `picsure_py` binding
via `testthat::local_mocked_bindings()`; see
[`testing.md`](testing.md).

## Tracing a query

```
picsure::runQuery(bdc, query, type = "count")
  |
  v
R/query.R:runQuery
  - validates `query`
  - coerces `type` ("count" | QueryType$COUNT | ...) -> Python QueryType
    via utils_coerce.R:to_py_enum
  - drop_nulls(...) so reticulate doesn't translate R NULL -> Python None
  |
  v
session$runQuery(query = ..., type = ...)        # reticulate proxy call
  |
  v
Python picsure.Session.runQuery
  - executes the query against the PIC-SURE backend
  - returns a CountResult / dict / DataFrame
  |
  v  (reticulate auto-converts pandas.DataFrame -> R data.frame)
R wrapper unwraps the result; with_picsure_error converts any
python.builtin.Exception into a `picsureError` condition
  |
  v
caller
```

The same pattern applies to every public function: validate, coerce,
delegate to a `picsure_py$*` or `session$*` callable inside
`with_picsure_error(...)`.

## Package layout

| File                                         | Responsibility                                                                                                                                                                                       |
|----------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [`R/connect.R`](../../R/connect.R)           | `connect()`: validates platform/token, resolves Platform enum members (R-side or reticulate-wrapped) to the Python `Platform` member, forwards a whitelist of optional kwargs (`CONNECT_EXTRA_KWARGS`). |
| [`R/clauses.R`](../../R/clauses.R)           | `createSubQuery()` and `buildQuery()`: clause and clause-group constructors. Coerces `type` to `ClauseType` and `operator` to `GroupOperator`.                                                       |
| [`R/query.R`](../../R/query.R)               | `runQuery()`, `loadQueryByID()`, `runQueryByID()`, `saveQueryByName()`, `removeSubQuery()`, `replaceClause()`: query execution against an open session (with `QueryType` coercion), named-query persistence, and local structural tree edits that forward 1:1 to the Python adapter. |
| [`R/search.R`](../../R/search.R)             | `searchDictionary()`: dictionary keyword search, optionally narrowed by a `FacetSet`.                                                                                                                |
| [`R/facets.R`](../../R/facets.R)             | `facets()`, `addFacet()`, `removeFacet()`: build and mutate FacetSets. `addFacet()` accepts a value vector and adds one entry per value.                                                             |
| [`R/export.R`](../../R/export.R)             | `exportAsPFB()` (re-runs the query and writes PFB), `exportCSV()` / `exportTSV()` (write an already-materialized data.frame).                                                                       |
| [`R/enums.R`](../../R/enums.R)               | R-side enum constants `ClauseType`, `GroupOperator`, `QueryType`, `Platform`. Each member is an S3 list of class `c(<subclass>, "picsure_enum_member")`. Mirrors the Python enums.                  |
| [`R/errors.R`](../../R/errors.R)             | `picsureError()` condition constructor, `with_picsure_error()` wrapper that catches `python.builtin.Exception` and re-raises as `picsureError`.                                                     |
| [`R/platforms.R`](../../R/platforms.R)       | `platforms()`: returns the label strings of the Python `Platform` enum. `.platform_labels()` is the field-read helper unit tests exercise without a live Python session.                            |
| [`R/utils_coerce.R`](../../R/utils_coerce.R) | Type-coercion helpers used at the R<->Python boundary: `drop_nulls()`, `as_enum_string()`, `to_py_enum()`.                                                                                          |
| [`R/zzz.R`](../../R/zzz.R)                   | `.onLoad` / `.onAttach`; declares `.PICSURE_PY_SPEC` and the package-private `picsure_py` binding.                                                                                                 |

## Public API surface

The exported names (see [`NAMESPACE`](../../NAMESPACE)) are:

- Connection: `connect`, `platforms`
- Enums: `ClauseType`, `GroupOperator`, `QueryType`, `Platform`
- Search and facets: `searchDictionary`, `facets`, `addFacet`, `removeFacet`
- Query: `createSubQuery`, `buildQuery`, `runQuery`, `loadQueryByID`, `runQueryByID`, `saveQueryByName`, `removeSubQuery`, `replaceClause`
- Export: `exportAsPFB`, `exportCSV`, `exportTSV`
- Errors: `picsureError`

Registered S3 methods (via `S3method(...)` in `NAMESPACE`, not direct
exports — they dispatch on `format()`, `print()`, and `as.character()`
when called on the matching class):
`format.picsure_enum_member`, `print.picsure_enum_member`,
`as.character.picsure_enum_member`, `print.picsure_platform`.

All public functions are camelCase to mirror the Python wrapper 1:1.
Internal helpers are snake_case.

## R <-> Python type coercion

Type handling at the boundary lives in
[`R/utils_coerce.R`](../../R/utils_coerce.R). Three helpers, three
concerns:

- **`drop_nulls(x)`** — removes `NULL`-valued entries from a kwargs
  list before handing it to `do.call(py_callable, kwargs)`. reticulate
  maps R `NULL` to Python `None`, which would override the Python
  function's own default. Dropping `NULL`s first lets Python supply
  its defaults. The wrappers in `connect.R`, `clauses.R`, `query.R`,
  and `search.R` all compose this before calling Python.
- **`as_enum_string(value, expected_subclass, enum_name, field)`** —
  resolves either a plain string or a `picsure_enum_member` to its
  string identifier. Rejects members of the wrong subclass (e.g. a
  `GroupOperator` member where a `ClauseType` is expected) before
  doing anything else.
- **`to_py_enum(value, enum_obj, enum_name, expected_subclass)`** —
  case-insensitively resolves a string or a typed enum member against
  a Python enum proxy (or, in tests, a named R list). Returns the
  Python enum member that reticulate then passes through unchanged.

Notes on the gotchas the wrappers actually encode:

- `NULL` in an optional kwarg means "use Python default". Never let
  it propagate.
- Strings are passed through to Python verbatim; reticulate handles
  the `str` conversion. Character vectors of length > 1 are accepted
  by `createSubQuery(keys = ...)` and forwarded as a Python list.
- `data.frame` values returned by Python (e.g. participant tables)
  arrive as native R data frames via reticulate's pandas converter.
- The `Platform` R member carries both the Python `value` shape (a
  named list mirroring the Python `PlatformConfig` dataclass) and
  flat fields (`url`, `resource_uuid`, `label`, `include_consents`,
  `requires_auth`) so R users can read fields either way without a
  reticulate proxy hop.

## Error model

Source of truth is [`R/errors.R`](../../R/errors.R).

- `picsureError(message, py_cause = NULL)` constructs a condition of
  class `c("picsureError", "error", "condition")`. The original
  Python exception, when present, is attached as `$py_cause` for
  advanced debugging via `reticulate::py_last_error()`.
- `with_picsure_error(expr)` wraps any reticulate call so that a
  `python.builtin.Exception` raised inside `expr` is caught and
  re-raised as a `picsureError`, preserving the message Python crafted
  for end users. Non-Python R errors pass through unchanged.

Every public wrapper that crosses the boundary uses
`with_picsure_error(...)` so users always see a clean
`picsureError` rather than a raw `python.builtin.Exception`.

## Enum parity

[`R/enums.R`](../../R/enums.R) declares the R-side enums
(`ClauseType`, `GroupOperator`, `QueryType`, `Platform`) as named
lists of `picsure_enum_member` objects. These mirror the upstream
Python enums one-to-one: same member names, same `$value` strings,
same `Platform` fields.

The invariant is guarded by
[`tests/testthat/test-enums-parity.R`](../../tests/testthat/test-enums-parity.R),
which uses a live Python session to read `picsure.<Enum>.__members__`
and compare names, values, and (for `Platform`) every dataclass field.
There's also a meta-test that the set of enums exported by the Python
package exactly matches the set mirrored in R — if the upstream adds
a new enum, this test fails until you mirror it here. See
[`testing.md`](testing.md#the-enum-parity-invariant).
