# Bumping the pinned Python build

This package has no HTTP code of its own. Every request it makes is made by
the Python `picsure` adapter, pinned to one immutable commit in
[`R/zzz.R`](../../R/zzz.R). Backend compatibility is whatever that commit
resolves to, so moving the pin is a change to the package's behaviour, not a
dependency refresh.

A bump touches more than the spec string. The R side deliberately keeps
copies of Python knowledge: the enums, the connect kwargs, the exception
classes, the result-column schemas. Some of those copies have a test that
reads the pinned build and fails when they drift; some have none and have to
be read against the Python source by hand. This is the list, in the order
worth working through, with which is which.

## Before you start

Confirm the commit is reachable from a branch:

```bash
git branch -r --contains <sha>
```

A pull-request head resolves for some tools and not for reticulate. The
comment above `.PICSURE_PY_SPEC` in [`R/zzz.R`](../../R/zzz.R) explains why,
and that reasoning belongs next to the pin rather than here.

## The checklist

### 1. The pin itself

- [`R/zzz.R`](../../R/zzz.R): `.PICSURE_PY_SPEC`, and `.PICSURE_PY_TAG`,
  which is the release tag when the new commit is one a tag points at and
  `NA_character_` otherwise.
- [`tests/testthat/test-zzz.R`](../../tests/testthat/test-zzz.R): four
  assertions carry the pin. Three of them break on any bump:
  - "Python dependency pin parses as the consent-routing adapter direct
    reference" names the repository URL and the exact SHA.
  - ".picsure_pinned_sha reads the commit off the dependency spec" names the
    exact SHA again.
  - "a hatch-vcs version built from the pinned commit is recognized as a
    match" carries the pinned commit's abbreviation inside a sample version
    string and compares it against the live pin.

  The fourth, ".PICSURE_PY_TAG is NA while the pinned commit carries no
  release tag", breaks only when the new commit is one a tag points at, and
  it then flips from an `is.na()` check to an identity check against the new
  tag.

  Grepping for the old abbreviation over-reports: ".picsure_build_sha reads
  the commit out of a hatch-vcs version" spells it too, but only as input to
  the parser, and that test passes whatever the pin is.

**Guard:** `test-zzz.R` fails until the spec and every assertion that names
it are updated together, and `.picsure_warn_on_pin_mismatch()` warns at
runtime when the build that actually loaded is not the pinned one.

### 2. Provision the new build, then run the boundary tests

```r
devtools::test()
```

Start a fresh R session so reticulate provisions the new commit. The
`test-*-reticulate.R` files and `test-enums-parity.R` read the real Python
objects, so most of what follows announces itself here. Everything a guard
catches is listed below anyway, so the list stays useful when a test is
skipped for want of an interpreter.

One of those guards has no entry further down because it mirrors no R-side
constant:

- [`tests/testthat/test-python-surface-reticulate.R`](../../tests/testthat/test-python-surface-reticulate.R)
  reads the 10 session methods and 6 module-level callables the wrappers call
  back off `picsure.Session` and the `picsure` module. The names are
  hard-coded in `R/query.R`, `R/facets.R`, `R/export.R`, `R/search.R` and
  `R/clauses.R`, and written out again in
  [`tests/testthat/helper-mocks.R`](../../tests/testthat/helper-mocks.R), so
  before this guard existed a renamed method left the whole unit run green
  and surfaced only against a live deployment. It introspects the classes
  rather than connecting, so a bump that renames one fails here, with no
  token and no backend.

### 3. The enums

- [`R/enums.R`](../../R/enums.R): the seven enum definitions.
- [`tests/testthat/test-enums-parity.R`](../../tests/testthat/test-enums-parity.R)
  holds the `r_enums` vector in the "All Python picsure enums are mirrored in R"
  test.
- [`tests/testthat/helper-mocks.R`](../../tests/testthat/helper-mocks.R):
  the fake `picsure_py`'s enums, which the unit tier resolves against.

**Guard:** `test-enums-parity.R` compares names and values against
`__members__` in both directions, and a meta-test fails when the Python
package gains an enum R does not mirror. **No guard** on the fakes in
`helper-mocks.R`: a new member is only needed there if a unit test uses it,
and nothing says so.

### 4. `connect()`'s keyword arguments

- [`R/connect.R`](../../R/connect.R): `CONNECT_EXTRA_KWARGS`, the
  `\describe{}` list of supported keys in `connect()`'s `@param ...` block,
  and the paragraph after it naming the keys that are not accepted yet.
- [`tests/testthat/test-connect-reticulate.R`](../../tests/testthat/test-connect-reticulate.R)
  holds `pending_pin_bump`.

**Guard:** `test-connect-reticulate.R` reads `inspect.signature(picsure.connect)`
and fails both ways, when the whitelist names a key the build rejects and
when the build starts accepting a key still listed as pending. **No guard**
on the prose: the `@param` text and the not-accepted-yet paragraph are
checked by hand, and the failing test's message says what to move where.

### 5. The exception classes

- [`R/errors.R`](../../R/errors.R): `.PICSURE_PY_CONDITION_CLASSES`, and a row
  in `.PICSURE_CONDITION_PARENTS` for any R class the map gains, naming that
  class's one immediate parent. The rest of the chain is derived, so a leaf
  added under `picsureServerError` inherits `picsureConnectionError` without
  anyone having to say so.
- [`tests/testthat/test-errors-reticulate.R`](../../tests/testthat/test-errors-reticulate.R)
  holds the `expected` vector in "each Python error class maps to its R condition
  class".

**Guard:** `test-errors-reticulate.R` lists every public class in
`picsure.errors` and fails when one is unmapped; `test-errors.R` fails when a
mapped class has no ancestry row. **Partly unguarded:** the `expected` vector
skips classes the build does not define, so a newly-defined class is simply
not asserted until someone adds it.

### 6. The error-model prose

- [`R/errors.R`](../../R/errors.R): the "What the pinned Python build
  distinguishes" section in `picsureError()`'s roxygen, the file-header
  comment naming where R runs ahead of the pinned hierarchy, and the note on
  `.PICSURE_ERROR_TYPE_CLASSES` saying the class lookup always wins on the
  pinned build.
- [`architecture.md`](architecture.md): the error-model section, and the
  Python-dependency section at the top.

**No guard.** All of it names what the pinned build can and cannot raise, and
all of it goes stale silently.

### 7. The result schemas and the verify vocabulary

- [`R/utils_coerce.R`](../../R/utils_coerce.R): `.DICTIONARY_RESULT_SCHEMA`,
  `.CONSEQUENCES_RESULT_SCHEMA`, `.GENOMIC_VALUES_RESULT_SCHEMA`,
  `.TIMESERIES_RESULT_SCHEMA`.
- [`R/connect.R`](../../R/connect.R): `.VERIFY_TRUE_STRINGS` and
  `.VERIFY_FALSE_STRINGS`.

**Guard:** `test-search-reticulate.R` checks `.DICTIONARY_RESULT_SCHEMA`
against `_COLUMNS_WITH_VALUES` in `picsure._services.search`, and
`.CONSEQUENCES_RESULT_SCHEMA` against what `genomicConsequences()` actually
returns. `picsure._services.search` is a private subpackage, so the
dictionary check **fails** rather than skipping when it stops importing: a
build that renames it takes the schema's only live guard with it, and until
the test is re-anchored the schema has to be read against the Python source
by hand, like the rows in the table below.

**No guard** on the other three, because each mirrors an inline Python
literal with no importable name:

| R-side copy | Read this by hand |
|---|---|
| `.GENOMIC_VALUES_RESULT_SCHEMA` | `search_genomic_values()` in `picsure/_services/genomic_search.py`, the `pd.DataFrame({...})` at the end |
| `.VERIFY_TRUE_STRINGS` / `.VERIFY_FALSE_STRINGS` | `_resolve_verify()` in `picsure/_transport/client.py`, the two word tuples |
| `.TIMESERIES_RESULT_SCHEMA` | HPDS's `TimeseriesProcessor` header, which is server-side rather than adapter-side, so it moves with the backend and not with this pin |

A schema that names a column Python renamed types nothing at all:
`apply_result_schema()` works over `intersect(names(schema), names(data))`, so
the mismatch is silent and the empty-versus-non-empty dtype divergence the
schema exists to prevent comes straight back.

### 8. The declared interpreter version

- [`DESCRIPTION`](../../DESCRIPTION): `SystemRequirements`, against
  `requires-python` in the new build's `pyproject.toml`.

**No guard.**

### 9. The integration tier

```bash
export PICSURE_INTEGRATION=1
export PICSURE_TEST_PLATFORM=BDC_OPEN
export PICSURE_TEST_TOKEN=...
export PICSURE_TEST_REQUIRE_PATH='\\phs...\\...'
Rscript -e 'pkgload::load_all(".", quiet = TRUE)' \
        -e 'testthat::test_dir("tests/testthat/integration", env = new.env(parent = asNamespace("picsure")))'
```

Needs VPN and a backend running the matching server protocol. See
[`testing.md`](testing.md#integration-tests) for the rest of the variables.
This is the only check that the new commit talks to a real deployment; no
unit test can stand in for it.

### 10. Changelog and generated docs

- [`NEWS.md`](../../NEWS.md): what changed for users, including any
  `connect()` argument that has just become available.
- `devtools::document()`, then confirm `git status` is clean.
  `man/connect.Rd` and `man/picsureError.Rd` are the two that move most.

**No guard** in `R CMD check`: a stale `man/` passes it. `git status` after
`document()` is the check.
