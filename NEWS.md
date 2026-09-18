# picsure 2.0.0.9000

- **Breaking:** `connect()` no longer accepts `resource_uuid`. PIC-SURE v3
  routes by URL path (`/hpds/auth` vs `/hpds/open`, chosen by the `Platform`
  member), so a resource UUID never selected anything. The Python adapter
  still takes the argument for backwards compatibility, storing it on the
  session and ignoring it. Forwarding a value that cannot affect the
  result is worse than refusing it, so this wrapper drops it from the
  accepted extras: passing it raises a `picsureValidationError` listing the
  valid ones.
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
  so the hierarchy does not change shape when the pin moves. How finely
  the leaf is identified does. The currently pinned Python build is flatter:
  it defines `PicSureAuthError` but no `PicSureAuthenticationError`,
  `PicSureAuthorizationError`, `PicSureTLSError`, or `PicSureServerError`.
  Against it, a server-side token refusal is a plain `picsureAuthError` and a
  transport failure a plain `picsureConnectionError`. `picsureTLSError` cannot
  arrive at all yet, and `picsureAuthenticationError` and
  `picsureAuthorizationError` cannot arrive as the leaf that identifies a
  condition; those three start arriving only once the pin is bumped to a build
  that defines the matching Python classes. The rest of the tree is live today,
  ancestors included, so `picsureServerError` does reach a handler: not as a
  leaf, but on every `picsureConsentLookupError`, which the R table places
  beneath it, the same way `picsureAuthorizationError` comes along on every
  `picsureConsentDeniedError`.

- Two R options are now read on every `connect()` call and forwarded as
  call-time arguments. `picsure.ssl_verify` takes `TRUE`, `FALSE`, or the
  path to a CA bundle:

  ```r
  options(picsure.ssl_verify = FALSE)
  options(picsure.dev_mode   = TRUE)
  ```

  An explicit `verify =` / `dev_mode =` argument to `connect()` wins over the
  option, and both go through the same checks: `dev_mode` must be `TRUE` or
  `FALSE`, and `verify` must be `TRUE`, `FALSE`, or a path to a CA bundle. An
  unusable value, such as the string `"false"`, raises a
  `picsureValidationError` naming the argument or option and the logical to
  pass, rather than failing later inside Python. When the `verify` argument
  or `options(picsure.ssl_verify)` turns verification off, `connect()` prints
  a message naming which of the two did it. Verification turned off
  Python-side through `PICSURE_SSL_VERIFY` is silent on the R side, one more
  reason to prefer the option.

  They exist because the environment variables that used to be the only way
  in, `PICSURE_SSL_VERIFY` and `PICSURE_DEV_MODE`, work only if they are
  set *before the Python interpreter starts*. CPython snapshots the
  environment into `os.environ` at startup and never refreshes it, and
  reticulate runs that interpreter inside the R process, so once the first
  call has provisioned Python a later `Sys.setenv(PICSURE_SSL_VERIFY =
  "false")` changes the R process's environment and Python cannot see it.
  (The Python adapter reads the variables per call, not at import; the
  interpreter's *start* is the boundary, not `import picsure`.) An option
  read on every `connect()` has no such window.

- `connect()` does not accept `timeout` or `validate` yet. The currently
  pinned Python adapter has no such parameters, so either one is rejected up
  front like any other unknown key, with a `picsureValidationError` naming
  the argument and listing the extras that are valid. Both arrive when the
  pin moves to a build that takes them: `timeout` will be the per-request
  deadline in seconds for the data operations the session performs, counts,
  participant downloads, and export polls, defaulting to the Python adapter's
  ten minutes, and `validate = FALSE` will skip both the local token check
  and the one request that confirms the deployment is reachable and accepts
  the token, for offline or mocked use.

- A missing token on an auth-required platform now raises
  `picsureValidationError` rather than `picsureAuthenticationError`, matching
  the Python adapter, and `requires_auth = FALSE` on an `_AUTHORIZED`
  `Platform` member no longer demands a token.

- `connect()`'s documentation of `include_consents`, `requires_auth`, and
  `supports_genomic` was wrong: it described each as having one fixed
  default. All three are resolved per platform. For a `Platform` member each
  defaults to that member's own flag; for a **custom URL string**
  `requires_auth` defaults to `TRUE` while `include_consents` and
  `supports_genomic` default to `FALSE`. The URL case matters. A
  consent-gated deployment reached by URL connects with an empty consent
  list, and since the consent list is what scopes dictionary results,
  `searchDictionary()` then returns every concept in the index instead of the
  ones the user's consents cover. Pass `include_consents = TRUE` explicitly
  in that case. `supports_genomic` is also `TRUE` for
  `Platform$NHANES_AUTHORIZED`, not only for the BDC authorized platforms.

- Facet documentation and error messages named `study_ids`, a category no
  deployment publishes. The category names come from the server's facets
  endpoint, and the dictionary serves `dataset_id`, `data_type`, and
  `data_source`. Examples and messages now use those, and passing a name the
  server does not publish raises an error listing the ones it does.

- `platforms()` works against a real interpreter. It read the Python
  `Platform` enum's `__members__`, which crosses the reticulate boundary as a
  `mappingproxy`, a type reticulate has no converter for, so iterating it
  failed with "cannot coerce type 'environment' to vector of type 'list'" on
  every call. The mapping is now copied into a `dict` and converted
  explicitly. `platforms()` is now covered by tests that build a genuine
  Python enum through reticulate. The test double that hid the bug, a plain
  character vector of labels, is still in place: it is documented as
  `connect()`'s name-to-label lookup table and no longer stands in for the
  enum.

- Enum parity against the Python adapter now fails loudly, and once, when no
  Python interpreter is available, naming the interpreter it looked for and
  stating that parity was not verified. The old guard trusted
  `inherits(picsure_py, "python.builtin.module")`, which the `delay_load`
  proxy satisfies before Python exists, so a missing interpreter produced
  eight opaque "Installation of Python not found" errors that were
  indistinguishable from the enums genuinely disagreeing.

- `removeFacet()` works. It called `FacetSet.remove()`, a method the Python
  adapter has never defined, so every call failed with an `AttributeError`.
  It now rebuilds the category's selections without the removed values,
  accepts a vector of values like `addFacet()` does, and rejects an empty
  `value` with a `picsureValidationError`.

- `connect()` now sources the user's consent list from PSAMA's
  `/psama/user/me/consents` endpoint instead of the query template. This is a
  change in the pinned Python adapter; the R API is unchanged, including the
  `include_consents` argument. Connecting with `include_consents = TRUE`
  requires a backend that serves that endpoint.

- Dictionary, timeseries, genomic-value, and consequence results are now
  typed from a declared schema rather than inferred from the rows that came
  back, so a search that matched nothing hands back the same column types as
  one that matched everything. Previously an empty dictionary result arrived
  with every column character, `min`, `max`, and `allowFiltering` included,
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
  commit no branch contains, and fails for good if the pull request is ever
  deleted. The pin now names the squash-merged commit on
  `pic_sure_api_rewrite`, whose source tree is identical.

- The package now warns when the Python `picsure` that loaded is not the
  pinned build. An already-active virtual environment takes precedence over
  the pin whenever `reticulate` attaches to it, and nothing reported the
  substitution, so whole test runs could pass against an unpinned build. The
  version installed is now read through `importlib.metadata` and checked
  against the pinned commit. A build naming a different commit or release
  raises a warning naming both, not an error, so a deliberate local override
  stays a supported workflow. A build that cannot be compared, because it
  has no distribution metadata or its version carries no commit suffix and
  no tag to check against, emits a startup message saying the pin could not
  be confirmed. A build made at the tagged pinned commit counts as a match.

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
