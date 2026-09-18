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
  `picsure_py$PhenotypicFilterType`, etc.).
- `.onLoad()` calls `reticulate::py_require(.PICSURE_PY_SPEC)` to
  declare the Python dependency. The spec is a PEP 508 direct
  reference (the Python package is not yet on PyPI) pinned to an
  immutable commit SHA on the upstream `pic_sure_api_rewrite` branch.
  `@main` is pre-rewrite and 404s at `connect()` against the rewrite
  gateway, so it must not be used here; the comment in `R/zzz.R` says
  why a pull-request head is never a valid pin, and
  [`pinned-python-build.md`](pinned-python-build.md) is the checklist of
  every site a bump touches.
- `.onLoad()` then assigns `picsure_py <<- reticulate::import("picsure",
  delay_load = list(on_load = .picsure_warn_on_pin_mismatch))`. The
  delayed load is the load-time invariant: `library(picsure)` stays
  fast, and tests that never touch Python never trigger env creation.
  The Python env is materialized lazily on the first real attribute
  access on `picsure_py`.
- `.picsure_warn_on_pin_mismatch()` checks, once per session, that the
  Python `picsure` build which actually loaded is the pinned one. It
  compares the commit SHA (or release tag) in `.PICSURE_PY_SPEC`
  against the installed distribution's version, which `hatch-vcs`
  derives from `git describe`. A build naming a different commit or
  release only warns, because a local override is a supported
  development workflow; a build it cannot compare, having no
  distribution metadata or a version carrying neither commit nor tag,
  emits a package startup message instead. `delay_load`'s `on_load`
  hook runs it when the module resolves lazily; `.onLoad()` runs it
  directly when Python is already initialized, because reticulate then
  imports eagerly and skips the hook.
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
| [`R/clauses.R`](../../R/clauses.R)           | `buildClause()`, `buildClauseGroup()`, and `buildQuery()`: clause, clause-group, and query constructors. Coerces `type` to `PhenotypicFilterType` and `operator` to `GroupOperator`.                                                       |
| [`R/query.R`](../../R/query.R)               | `runQuery()`, `loadQueryByID()`, `runQueryByID()`, `saveQueryByName()`, `removeSubQuery()`, `replaceClause()`: query execution against an open session (with `QueryType` coercion), named-query persistence, and local structural tree edits that forward 1:1 to the Python adapter. |
| [`R/search.R`](../../R/search.R)             | `searchDictionary()`: dictionary keyword search, optionally narrowed by a `FacetSet`.                                                                                                                |
| [`R/facets.R`](../../R/facets.R)             | `facets()`, `addFacet()`, `removeFacet()`: build and mutate FacetSets. `addFacet()` accepts a value vector and adds one entry per value.                                                             |
| [`R/export.R`](../../R/export.R)             | `exportAsPFB()` (re-runs the query and writes PFB), `exportCSV()` / `exportTSV()` (write an already-materialized data.frame).                                                                       |
| [`R/enums.R`](../../R/enums.R)               | R-side enum constants `PhenotypicFilterType`, `GroupOperator`, `QueryType`, `Platform`. Each member is an S3 list of class `c(<subclass>, "picsure_enum_member")`. Mirrors the Python enums.                  |
| [`R/errors.R`](../../R/errors.R)             | `picsureError()` condition constructor, `with_picsure_error()` wrapper that catches `python.builtin.Exception` and re-raises as `picsureError`.                                                     |
| [`R/platforms.R`](../../R/platforms.R)       | `platforms()`: returns the label strings of the Python `Platform` enum. `.platform_labels()` is the field-read helper unit tests exercise without a live Python session.                            |
| [`R/utils_coerce.R`](../../R/utils_coerce.R) | Everything at the R<->Python boundary that is not a wrapper: kwarg shaping (`drop_nulls()`), argument validation (`as_positive_whole_number()`, `check_optional_number()`, `as_single_string()`, `as_single_flag()`, `describe_argument_value()`), enum resolution (`as_enum_string()`, `to_py_enum()`, the `.py_enum_*` lookups), and result typing (`apply_result_schema()`, `coerce_result_column()`, and the four result schemas). See the type-coercion section below. |
| [`R/zzz.R`](../../R/zzz.R)                   | `.onLoad` / `.onAttach`; declares `.PICSURE_PY_SPEC` and the package-private `picsure_py` binding.                                                                                                 |

## Public API surface

The exported names (see [`NAMESPACE`](../../NAMESPACE)) are:

- Connection: `connect`, `platforms`
- Enums: `PhenotypicFilterType`, `GroupOperator`, `QueryType`, `Platform`
- Search and facets: `searchDictionary`, `facets`, `addFacet`, `removeFacet`
- Query: `buildClause`, `buildClauseGroup`, `buildQuery`, `runQuery`, `loadQueryByID`, `runQueryByID`, `saveQueryByName`, `removeSubQuery`, `replaceClause`
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
[`R/utils_coerce.R`](../../R/utils_coerce.R), which covers four
concerns. The roxygen on each function is the detail; this is the map.

**Kwarg shaping.** `drop_nulls(x)` removes `NULL`-valued entries from a
kwargs list before `do.call(py_callable, kwargs)`. reticulate maps R
`NULL` to Python `None`, which would override the Python function's own
default. Dropping `NULL`s first lets Python supply its defaults. The
wrappers in `connect.R`, `clauses.R`, `query.R`, and `search.R` all
compose this before calling Python.

**Argument validation**, all raising `picsureValidationError`s that name
the argument and what arrived. `as_positive_whole_number()`,
`check_optional_number()`, `as_single_string()`, and `as_single_flag()`
are the checks; `describe_argument_value()` renders the rejected value
for their messages. Each one is the only implementation of its
predicate: `as_single_flag()` serves both `connect()`'s `dev_mode` and
`saveQueryByName()`'s `overwrite`, and `.check_facet_key()` in
`R/facets.R` is `as_single_string()` plus a facet-specific hint.

Every one of them takes a `call` argument defaulting to `sys.call(-1L)`
in its own frame and threads it into `.picsure_reject()`, so the
rejection reports the wrapper the researcher invoked rather than no call
at all. That default holds only when the validator is called straight
from the wrapper's body. Calling it inside an argument to another
closure, as in `drop_nulls(list(page = as_positive_whole_number(...)))`,
makes the promise resolve one frame deeper and the error reports
`drop_nulls(...)`, so the wrappers assign each validated value to a
local first. `connect()`'s settings go through `.picsure_setting()`,
which adds a frame of its own and therefore takes and forwards a `call`
too. `NULL` stays correct where no frame is a call anyone made, which is
the Python error boundary in `with_picsure_error()`.

**Enum resolution across the boundary.** `as_enum_string()` resolves a
plain string or a `picsure_enum_member` to its string identifier,
rejecting members of the wrong subclass (a `GroupOperator` where a
`PhenotypicFilterType` is expected) first. `to_py_enum()` then resolves
that case-insensitively against a Python enum proxy and returns the
member reticulate passes through unchanged. `.py_enum_members()`,
`.py_enum_member_names()`, and `.py_enum_member()` are the lookups
underneath: they go through `__members__`, copying that `mappingproxy`
into a real `dict` because reticulate has no converter for it.

**Result typing from a declared schema.** `apply_result_schema()` types
the columns of a returned data frame from a schema, and
`coerce_result_column()` does one column, warning rather than failing
when values are lost. The schemas are `.DICTIONARY_RESULT_SCHEMA`,
`.TIMESERIES_RESULT_SCHEMA`, `.GENOMIC_VALUES_RESULT_SCHEMA`, and
`.CONSEQUENCES_RESULT_SCHEMA`. They exist because pandas infers a column
type from the rows, so an empty or all-empty column arrived in R with
whatever type the query happened to produce.

Notes on the gotchas the wrappers actually encode:

- `NULL` in an optional kwarg means "use Python default". Never let
  it propagate.
- Strings are passed through to Python verbatim; reticulate handles
  the `str` conversion. Character vectors of length > 1 are accepted
  by `buildClause(keys = ...)` and forwarded as a Python list.
- `data.frame` values returned by Python (e.g. participant tables)
  arrive as native R data frames via reticulate's pandas converter.
- The `Platform` R member carries both the Python `value` shape (a
  named list mirroring the Python `PlatformConfig` dataclass) and
  flat fields (`url`, `label`, `include_consents`, `requires_auth`,
  `supports_genomic`) so R users can read fields either way without a
  reticulate proxy hop.

## Error model

Source of truth is [`R/errors.R`](../../R/errors.R).

- `picsureError(message, py_cause = NULL, class = NULL)` constructs the
  condition. `class` names one of the specialized classes, whose
  ancestors `.PICSURE_CONDITION_PARENTS` fills in, and every condition
  carries `picsureError` whatever `class` is, so the documented
  `tryCatch(picsureError = ...)` handler catches all of them. The tree
  and what each class means are in `?picsure::picsureError`; it is not
  redrawn here.
- The original Python exception, when present, is attached as
  `$py_cause` for advanced debugging via
  `reticulate::py_last_error()`, and its class name as `$python_class`.
- `with_picsure_error(expr)` wraps any reticulate call so that a
  `python.builtin.Exception` raised inside `expr` is caught and
  re-raised as a `picsureError`, preserving the message Python crafted
  for end users. It maps the Python exception to its matching subclass
  through `.PICSURE_PY_CONDITION_CLASSES`, falling back to the
  backend's `errorType` string. Non-Python R errors pass through
  unchanged.
- The R side derives a condition's ancestry from its own table rather
  than from the installed Python exception's MRO, so the hierarchy
  keeps its shape when the pinned commit moves. What changes with the
  pin is how finely the leaf is identified.

Every public wrapper that crosses the boundary uses
`with_picsure_error(...)` so users always see a clean
`picsureError` rather than a raw `python.builtin.Exception`.

## Enum parity

[`R/enums.R`](../../R/enums.R) declares the R-side enums
(`PhenotypicFilterType`, `GroupOperator`, `QueryType`, `Platform`) as named
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
