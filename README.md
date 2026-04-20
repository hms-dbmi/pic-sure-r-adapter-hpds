# picsure (R) — v3 rewrite in progress

This branch (`query_v3`) contains an in-progress rewrite of the PIC-SURE R
adapter. The new implementation is a thin wrapper over the Python
[`picsure`](https://github.com/hms-dbmi/pic-sure-python-adapter-hpds) package,
wired through [`reticulate`](https://rstudio.github.io/reticulate/).

For the design spec, see
[`docs/superpowers/specs/2026-04-20-r-adapter-rewrite-design.md`](docs/superpowers/specs/2026-04-20-r-adapter-rewrite-design.md).

A full README with installation and usage instructions will land with the
final implementation plan before this branch merges to `main`. Until then,
the v1 R adapter on `main` remains the supported version.
