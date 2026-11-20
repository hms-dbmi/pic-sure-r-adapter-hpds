# Releasing

## Overview

The R adapter is a thin wrapper over the Python adapter. Each R release pins one exact Python
commit, and reticulate installs that commit into its own environment the first time a user's session
needs Python. So an R release is always paired with a Python release, and it always comes after
that Python release.

The R adapter is not on CRAN. A release is a GitHub Release with a `vX.Y.Z` tag, and users install
the newest one:

```r
remotes::install_github("hms-dbmi/pic-sure-r-adapter-hpds@*release")
```

This guide is for the developers who cut releases and the product managers who schedule them. Read
the [Python adapter's release guide](https://github.com/hms-dbmi/pic-sure-python-adapter-hpds/blob/main/docs/development/releasing.md)
first: the R process follows the same steps, and this guide covers only what differs.

Rules that hold for every release:

- **Backward compatible, or deprecated first.** Same rule and same six-month window as Python.
- **Never ahead of the servers or the Python release.** The R release ships after the Python release
  it pins, which ships right after BDC and NHANES production are upgraded.
- **No GitHub Release is ever a prerelease.** `@*release` installs the newest GitHub Release,
  prereleases included, so an rc published as a Release would reach every user. Release candidates
  are tags only.
- **`client_type` is allowlisted.** The adapter sends `X-Client-Type: R_ADAPTER`, and the PIC-SURE
  gateway rejects any value outside its allowlist.

## Versioning and deprecation

Versions follow semantic versioning, with one link to Python: **R's major version follows Python's
major version.** R 4.x pins Python 4.x. Minor and patch numbers move on their own, so an R-only fix
does not wait for a Python release.

| Bump | When |
| --- | --- |
| Major | The pinned Python build moves to a new major version, or an R change breaks a caller |
| Minor | New R features, or a pin to a Python minor release, with existing calls still working |
| Patch | R bug fixes, or a pin to a Python patch release |

Every R release note names the Python version it pins.

### Deprecating something

The same six-month window as Python applies:

1. Ship the replacement in a minor release, and make the old function warn every time it runs with
   `.Deprecated("newName", msg = "...")`, where the message names the replacement and the removal
   release. If the package takes on lifecycle, use `lifecycle::deprecate_warn(..., always = TRUE)`,
   because its default warns once every 8 hours.
2. The warning and the `NEWS.md` entry name the replacement and the earliest release that removes
   the old function.
3. Remove it only in a major release, six months or more after the warning shipped.

A Python deprecation reaches R users only as text Python prints to the console. It is not an R
warning, so `withCallingHandlers()` and `options(warn = 2)` never see it. When R wraps a deprecated
Python call, the R function has to raise its own R warning that names the R replacement.

## Server compatibility

The R adapter calls PIC-SURE only through its pinned Python build, so it inherits that build's
server requirement.

| R adapter | Pins Python | Minimum PIC-SURE | Last PIC-SURE tested |
| --- | --- | --- | --- |
| 4.0.9 | 4.0.9 | 5.0.0 | Not recorded |
| 4.0.0 | 4.0.0 | 5.0.0 | Not recorded |
| 3.0.0 | 3.0.0 | 4.1.0, and 4.1.x only | 4.1.x |

The gate is the Python one. The Python release owner checks the routes before the Python rc. The R
release owner confirms the pinned Python release passed that gate, and updates this table in the R
release PR.

## Release steps

Each step has an owner. Owners are not assigned yet. Until they are, the person cutting the release
owns every step. Step 1 runs during the Python code freeze. Steps 2 to 4 start as soon as the
Python rc is published, so R QA runs alongside Python QA. Steps 6 onward wait for the final Python
release.

1. **Merge the work.** R changes reach `main` through reviewed PRs, each with a line in `NEWS.md`
   under the development heading.
   - Owner: to be assigned
2. **Open the rc pin PR.** Pin the Python rc as soon as it is published: its commit SHA and its rc
   tag (next section). In the same PR, set `DESCRIPTION` to the release version (`4.1.0`), rename the
   `NEWS.md` heading to `# picsure 4.1.0`, and update the compatibility table. Before merging:
   - `devtools::document()` leaves `man/` and `NAMESPACE` unchanged.
   - `check.yml` is green across its R versions, and
     `Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))'` is clean locally. It
     checks a built tarball, as CI does.
   - `devtools::build_vignettes()` and `pkgdown::build_site()` succeed.
   - Owner: to be assigned
3. **Tag the R release candidate.** Push an annotated tag on the tip of `main`, with **no GitHub
   Release**: `git tag -a v4.1.0rc1 -m "v4.1.0rc1" && git push origin v4.1.0rc1`.
   - Owner: to be assigned
4. **QA the release candidate.** Run the QA checklist below. If anything fails, fix by PR and tag
   `v4.1.0rc2`. If the failure is in Python, wait for the next Python rc and re-pin as soon as it is
   published.
   - Owner: to be assigned
5. **Wait for Python.** The Python release ships after the BDC and NHANES production upgrades.
   - Owner: to be assigned
6. **Open the final pin PR.** Re-pin to the final Python tag. The SHA usually stays the same,
   because the final Python tag sits on the rc commit, but the pinned tag always changes
   (`v4.1.0rc1` becomes `v4.1.0`). Merge after review.
   - Owner: to be assigned
7. **Publish the final release.** Create a GitHub Release for tag `vX.Y.Z` on that merge commit. It
   is not a prerelease, and it is the latest release.
   - Owner: to be assigned
8. **Verify.** In a fresh R session, install with `@*release`, check `packageVersion("picsure")`,
   and call `connect()` so reticulate builds the pinned environment without a pin-mismatch warning.
   - Owner: to be assigned
9. **Reopen development.** Bump `DESCRIPTION` to `X.Y.Z.9000` and add a
   `# picsure (development version)` heading at the top of `NEWS.md`.
   - Owner: to be assigned

## Pinning a Python build

The pin is two constants in [`R/zzz.R`](../../R/zzz.R):

```r
.PICSURE_PY_SPEC <- "picsure @ git+https://github.com/hms-dbmi/pic-sure-python-adapter-hpds.git@<40-character SHA>"
.PICSURE_PY_TAG <- "v4.1.0"
```

- `.PICSURE_PY_SPEC` names the exact commit. It must be a commit on a branch of the Python repo,
  never a pull-request head: reticulate can only fetch branches and tags, so a PR-head pin fails for
  real users. A squash merge creates a new commit, so read the SHA off `main` after the Python PR
  merges, and confirm it with `git branch -r --contains <sha>`.
- `.PICSURE_PY_TAG` is the Python tag on that commit: the rc tag for an R rc (`"v4.1.0rc1"`), the
  final tag for a release. The pin check needs it to recognize a build made at a tag.

Moving the pin touches more than these two lines. Work through
[`pinned-python-build.md`](pinned-python-build.md) every time. Its ten sections cover the pin and
its tests in `tests/testthat/test-zzz.R`, the boundary tests, enums, `connect()` arguments,
exception classes, error prose, result schemas, the declared Python version, the integration tier,
and the changelog and generated docs. The checklist says which sites a test catches and which you
have to check by hand against the Python source.

When users upgrade, reticulate sees the new pin and rebuilds its environment on the first call that
needs Python. If a user has `RETICULATE_PYTHON` set, reticulate uses that interpreter instead, and
the adapter warns on the first call that needs Python that the build does not match the pin.

## Release candidates and QA

An R release candidate is a tag and nothing else. Name it after the R version, numbered from 1
(`v4.1.0rc1`), and never create a GitHub Release for it. `@*release` resolves to the newest GitHub
Release, prereleases included, so an rc Release would go straight to every user who installs.

A workflow in the repo deletes any GitHub Release that is marked as a prerelease or whose tag
contains `rc`. It keeps the tag and opens an issue naming who created the Release. *Pending: the
workflow does not exist yet. Until it lands, this rule rests on the person cutting the release.*

### Installing

QA installs the rc by its tag:

```r
remotes::install_github("hms-dbmi/pic-sure-r-adapter-hpds@v4.1.0rc1")
```

Users install the latest release:

```r
remotes::install_github("hms-dbmi/pic-sure-r-adapter-hpds@*release")
```

Anyone who needs a reproducible install, such as a pipeline or a published analysis, should pin
the exact tag instead: `@v4.1.0`.

### QA checklist

1. **Change the install cells by hand.** The R notebooks on the `release_2026` branch of
   [Access-to-Data-using-PIC-SURE-API](https://github.com/hms-dbmi/Access-to-Data-using-PIC-SURE-API/tree/release_2026)
   install with `ref="query_v3"` or `ref="main"`. Before running each notebook and Rmd, change that
   to the rc tag. Nothing does this for you.
2. **Start a fresh R session** so reticulate builds the pinned Python environment. The pin check
   runs on the first call that needs Python, not at `library(picsure)`. A pin-mismatch warning there
   means another build loaded: unset `RETICULATE_PYTHON`, deactivate any virtualenv, and restart.
3. **Use a server that already runs the target PIC-SURE version.** The notebooks connect to
   `BDC_AUTHORIZED`, which is production.
4. **Run the integration tests on the rc tag:** Actions > Integration tests > Run workflow, then
   pick the tag.
5. **Run every R notebook and Rmd end to end.**
6. **Try each change in the `NEWS.md` entry.**
7. **Record the result** on the release PR or the tracking ticket: pass or fail, the PIC-SURE
   version, and who ran it. An R rc has no GitHub Release to record it on.

## Publishing the final release

After the final pin PR merges, open **Releases > Draft a new release**, type the tag `vX.Y.Z`, and
target the merge commit on `main`. Leave **Set as a pre-release** unticked and tick **Set as the
latest release**. Write the notes (next section), then publish.

No workflow builds or uploads anything for an R release. The GitHub Release is the release: from
that moment `@*release` resolves to it. Publishing it also runs the pkgdown workflow, which rebuilds
the reference site.

Only members of the developers team can push `v*` tags, which a tag ruleset enforces. *Pending: the
ruleset does not exist yet.*

If a release turns out to be broken, do not delete it. Users who installed it need the tag to
exist. Fix forward with a patch release, which becomes the newest Release and what `@*release`
installs.

## NEWS, DESCRIPTION and release notes

Between releases, `DESCRIPTION` carries a development version, the last release plus `.9000`
(`4.0.9.9000` before 4.1.0 is released), and `NEWS.md` opens with `# picsure (development version)`.
Changes collect under that heading as they merge.

Releases up to v4.0.9 did not follow this. `DESCRIPTION` reads `2.0.0.9000` at each of those tags
and `NEWS.md` still opens with `# picsure 2.0.0.9000`. The next release PR sets both.

The rc pin PR turns them into a release:

- `DESCRIPTION`: `Version: 4.1.0`. R versions cannot carry an `rc` suffix, so the rc tag and the
  final tag both build `4.1.0`. The tag name is what tells them apart.
- `NEWS.md`: rename the heading to `# picsure 4.1.0`, and add a line naming the pinned Python
  version.

After the release, the first PR bumps `DESCRIPTION` to `4.1.0.9000` and adds a fresh development
heading.

The GitHub Release body uses the same fixed sections as Python, plus the pinned Python version, then
GitHub's generated PR list. Write "None" rather than dropping a section.

```markdown
## Pins
Python adapter v4.1.0.

## Breaking changes
None.

## Requires
PIC-SURE 5.0.0 or later.

## Deprecations
None.
```

## Hotfix releases

A hotfix is always a patch release. There are two kinds.

**A Python hotfix.** After Python releases `v4.0.10`, R ships a pin-only patch:

1. Open a PR on `main` that moves the pin to the Python hotfix's SHA and tag, sets `DESCRIPTION` to
   the next R patch version, and adds a `NEWS.md` entry. The Python hotfix commit sits on its
   `hotfix/` branch, which counts as a branch for the pin rule.
2. Tag an rc, QA it the same way, then cut the GitHub Release.

**An R-only hotfix.**

1. Branch from the release tag being fixed: `git switch -c hotfix/v4.0.10 v4.0.9`, then push it.
   This works only for the latest release. `@*release` takes the first Release GitHub lists, the
   most recent one whatever its version number.
2. Fix by PR into that branch, with a `NEWS.md` entry and `DESCRIPTION` set to `4.0.10`.
3. Tag `v4.0.10rc1` on the branch (tag only, no Release) and QA the fix.
4. Create the GitHub Release for `v4.0.10` on the branch. This is the one case where a release tag is
   not on `main`.
5. The same day, open a second PR that brings the fix into `main`.
