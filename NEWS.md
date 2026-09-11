# picsure (development version)

- **Breaking:** `connect()` no longer accepts `resource_uuid`. PIC-SURE v3
  routes by URL path (`/hpds/auth` vs `/hpds/open`, chosen by the `Platform`
  member), so a resource UUID never selected anything. The Python adapter
  still takes the argument for backwards compatibility — it stores it on the
  session and ignores it — but forwarding a value that cannot affect the
  result is worse than refusing it, so this wrapper drops it from the
  accepted extras: passing it raises a `picsureError` listing the valid ones.
  There is no replacement; delete the argument. `Platform` members never
  carried a resource UUID, so nothing else in the R API changes.

- Errors now arrive as **specialized condition classes**, all of which still
  inherit `picsureError`, so an existing `tryCatch(picsureError = ...)`
  handler keeps working unchanged. New classes: `picsureAuthError` (with
  `picsureAuthenticationError` and `picsureAuthorizationError` beneath it,
  and `picsureConsentDeniedError` beneath that), `picsureConnectionError`
  (with `picsureTLSError` and `picsureServerError`, and
  `picsureConsentLookupError` beneath that), `picsureQueryError`, and
  `picsureValidationError`. Catch `picsureAuthError` for "refresh the token",
  `picsureConnectionError` for "the deployment is unreachable". See
  `?picsure::picsureError` for the tree and what each class means.

  Two things worth knowing. Arguments this package rejects itself used to be
  plain `simpleError`s, so a handler written from the documented
  `tryCatch(picsureError = ...)` missed them; they are now
  `picsureValidationError`s. And the R side derives a condition's ancestry
  from its own table rather than from the installed Python exception's MRO,
  so the hierarchy does not change shape when the pin moves — but how finely
  the leaf is identified does. The currently pinned Python build is flatter:
  it defines `PicSureAuthError` but no `PicSureAuthenticationError`,
  `PicSureAuthorizationError`, `PicSureTLSError`, or `PicSureServerError`.
  Against it, a refusal is a plain `picsureAuthError` and a transport failure
  a plain `picsureConnectionError`; the authentication-versus-authorization
  and TLS-versus-5xx splits start arriving only once the pin is bumped to a
  build that defines those classes.

- Two R options are now read on every `connect()` call and forwarded as
  call-time arguments:

  ```r
  options(picsure.ssl_verify = FALSE)   # or TRUE, or a path to a CA bundle
  options(picsure.dev_mode   = TRUE)
  ```

  An explicit `verify =` / `dev_mode =` argument to `connect()` wins over the
  option, and an unusable value raises a `picsureValidationError` naming the
  option rather than failing later inside Python.

  They exist because the environment variables that used to be the only way
  in — `PICSURE_SSL_VERIFY` and `PICSURE_DEV_MODE` — work only if they are
  set *before the Python interpreter starts*. CPython snapshots the
  environment into `os.environ` at startup and never refreshes it, and
  reticulate runs that interpreter inside the R process, so once the first
  call has provisioned Python a later `Sys.setenv(PICSURE_SSL_VERIFY =
  "false")` changes the R process's environment and Python cannot see it.
  (The Python adapter reads the variables per call, not at import; the
  interpreter's *start* is the boundary, not `import picsure`.) An option
  read on every `connect()` has no such window.

- `connect()`'s documentation of `include_consents`, `requires_auth`, and
  `supports_genomic` was wrong: it described each as having one fixed
  default. All three are resolved per platform. For a `Platform` member each
  defaults to that member's own flag; for a **custom URL string**
  `requires_auth` defaults to `TRUE` while `include_consents` and
  `supports_genomic` default to `FALSE`. The URL case matters — a
  consent-gated deployment reached by URL connects with an empty consent
  list, and since the consent list is what scopes dictionary results,
  `searchDictionary()` then returns every concept in the index instead of the
  ones the user's consents cover. Pass `include_consents = TRUE` explicitly
  in that case. `supports_genomic` is also `TRUE` for
  `Platform$NHANES_AUTHORIZED`, not only for the BDC authorized platforms.

- Facet documentation and error messages named `study_ids` and
  `data_source`, neither of which any current deployment publishes. The
  category names come from the server's facets endpoint; today it serves
  `dataset_id` and `data_type`. Examples and messages now use those.

- `platforms()` works against a real interpreter. It read the Python
  `Platform` enum's `__members__`, which crosses the reticulate boundary as a
  `mappingproxy` — a type reticulate has no converter for — so iterating it
  failed with "cannot coerce type 'environment' to vector of type 'list'" on
  every call. The mapping is now copied into a `dict` and converted
  explicitly. The test double that hid this (a plain character vector of
  labels, a shape Python never produces) has been replaced by tests that build
  a genuine Python enum through reticulate.

- Enum parity against the Python adapter now fails loudly, and once, when no
  Python interpreter is available, naming the interpreter it looked for and
  stating that parity was not verified. The old guard trusted
  `inherits(picsure_py, "python.builtin.module")`, which the `delay_load`
  proxy satisfies before Python exists, so a missing interpreter produced
  eight opaque "Installation of Python not found" errors that were
  indistinguishable from the enums genuinely disagreeing.

- `removeFacet()` works. It called `FacetSet.remove()`, a method the Python
  adapter has never defined, so every call failed with an `AttributeError`.
  It now rewrites the category's selections without the removed values, and
  accepts a vector of values like `addFacet()` does.

- `connect()` now sources the user's consent list from PSAMA's
  `/psama/user/me/consents` endpoint instead of the query template. This is a
  change in the pinned Python adapter; the R API is unchanged, including the
  `include_consents` argument. Connecting with `include_consents = TRUE`
  requires a backend that serves that endpoint.

- Dictionary, timeseries, genomic-value, and consequence results are now
  typed from a declared schema rather than inferred from the rows that came
  back, so a search that matched nothing hands back the same column types as
  one that matched everything. Previously an empty dictionary result arrived
  with every column character — `min`, `max`, and `allowFiltering` included —
  and a `type = "timestamp"` query over numeric concepts alone handed back
  `TVAL_CHAR` as numeric. Columns the schema does not name (a participant
  result's concept-path columns, a deployment-specific dictionary field) are
  left exactly as they arrive.

- Argument validation is consistent and names what arrived. `page` and `size`
  went through `as.integer()` with no check, so `page = 1.7` was silently
  truncated and `page = "two"` became `NA` and was forwarded to Python; a
  length-2 facet `key` reached an `if()` and produced base R's "'length = 2'
  in coercion to 'logical(1)'". Every rejection is now a
  `picsureValidationError` naming the argument and the value it got.

- A fresh install can provision its Python backend again. The pinned Python
  adapter commit was a pull-request head, a commit reachable from no branch.
  Tools that fetch a SHA directly resolve one, so `uv pip install` succeeded
  and the pin looked fine, but `reticulate` can fall back to fetching branches
  and tags and then resolving the SHA locally, which fails outright for a
  commit no branch contains — and fails for good if the pull request is ever
  deleted. The pin now names the squash-merged commit on
  `pic_sure_api_rewrite`, whose source tree is identical.

- The package now warns when the Python `picsure` that loaded is not the
  pinned build. An already-active virtual environment takes precedence over
  the pin whenever `reticulate` attaches to it, and nothing reported the
  substitution, so whole test runs could pass against an unpinned build. The
  version installed is now read through `importlib.metadata` and checked
  against the pinned commit, naming both on disagreement. It is a
  `packageStartupMessage`, not an error: a deliberate local override stays a
  supported workflow.

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
  with the `Platform` enum (e.g. `Platform$BDC_AUTHORIZED`); `platforms()`
  lists the human-readable labels.
- **Query construction.** Build nested AND/OR cohorts with
  `buildClause()`, `buildClauseGroup()`, and `buildQuery()`, and add
  variant filters with `buildGenomicFilter()`. Output concepts are
  attached via `buildQuery(includeConcepts = ...)` rather than the 1.x
  `SELECT` clause.
- **Query editing.** `removeSubQuery()` and `replaceClause()` edit an
  existing query tree by structural match.
- **Execution.** `runQuery(session, query, type = ...)` returns a
  `CountResult` (`"count"`, `"variant_count"`), a named list of
  `CountResult`s keyed by concept path (`"cross_count"`), a data frame
  (`"participant"`, `"timestamp"`, `"vcf_excerpt"`,
  `"aggregate_vcf_excerpt"`), or a character vector (`"variant_list"`).
  `runQueryByID()`, `loadQueryByID()`, and `saveQueryByName()` cover
  saved queries.
- **Search.** `searchDictionary()` searches the data dictionary;
  `facets()`, `addFacet()`, and `removeFacet()` build facet filters;
  `searchGenomicValues()` and `genomicConsequences()` cover genomic
  annotation values.
- **Export.** `exportAsPFB()`, `exportCSV()`, and `exportTSV()`.
- **Errors.** Failures surface as `picsureError` R conditions carrying
  researcher-facing messages, replacing the 1.x adapter's raw `httr`
  errors.
- **Enums.** `PhenotypicFilterType`, `GroupOperator`, `QueryType`,
  `Platform`, `GenomicFilterKey`, `VariantFrequency`, and
  `VariantSeverity` mirror the Python adapter's enums.

The legacy 1.x adapter remains available on the `main` branch during the
migration window.
