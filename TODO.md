# TODO

Tracking items that need follow-up. Remove the item from this file
once it's done; delete the file when it's empty.

## Post-merge cleanup: `query_v3` → `main`

The v3 rewrite is in flight on `query_v3` with v1 frozen on `main`.
Once `query_v3` merges into `main` and the rewrite is live, the
following references to `query_v3` need to be removed or updated.
The dev docs were written against the steady state (`main` as the
canonical branch), so most files need no changes — only the items
below.

### Documentation

- [ ] `CONTRIBUTING.md` — remove the "Transient note" callout near
      the top of the file. The "Branching and pull requests" section
      already targets `main` and needs no further edit.
- [ ] `README.md` — update the install snippet (currently
      `remotes::install_github("hms-dbmi/pic-sure-r-adapter-hpds@query_v3")`)
      to drop the `@query_v3` ref.
- [ ] `README.md` — remove or rewrite the "Migrating from v1"
      paragraph that says "this rewrite lives on `query_v3`."

### CI workflows

- [ ] `.github/workflows/check.yml` — remove `query_v3` from the
      `on.pull_request.branches` and `on.push.branches` lists.
- [ ] `.github/workflows/pkgdown.yml` — same as above.
- [ ] After updating the workflows, no follow-up edit to
      `docs/development/testing.md` is needed (the prose already says
      "Runs on PRs and pushes to `main`").

### Verification

- [ ] After the cleanup commit, run
      `grep -rn "query_v3" .` from the repo root and confirm zero
      matches outside of git history.
- [ ] Delete this file once every item above is checked off.
