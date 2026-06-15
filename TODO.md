# TODO

Tracking items that need follow-up. Remove the item from this file once
it’s done; delete the file when it’s empty.

## Eventual cutover: retire 1.x on `main`

The 2.0.0 rewrite lives on `query_v3` and is released as the `v2.0.0`
tag. The legacy 1.x adapter stays on `main` during the migration window,
so both branches coexist and CI builds both. When the team decides to
retire 1.x and make the rewrite the canonical `main`, the following
`query_v3` references need to be removed or updated.

### Documentation

`README.md` — the install snippet and “Migrating from v1” section point
at the `v2.0.0` tag. Revisit if `main` becomes the rewrite (a bare
`@main` or default install may then suffice).

`vignettes/getting-started.Rmd` — same install snippet.

`CONTRIBUTING.md` — the “Note on branches” callout and the “Branching
and pull requests” section both target `query_v3`; retarget to `main`
after the cutover.

### CI workflows

`.github/workflows/check.yml` and `.github/workflows/pkgdown.yml` —
`query_v3` can be dropped from the `branches` lists once it is the
default branch.

### Verification

After the cutover, run `grep -rn "query_v3" .` from the repo root and
confirm zero matches outside of git history.

Delete this file once every item above is checked off.
