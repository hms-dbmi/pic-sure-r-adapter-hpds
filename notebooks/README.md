# Notebooks

Committed Jupyter notebooks live here. Examples, demos, and
walkthroughs that ship with the project belong in this directory.

Both **R notebooks** (IRkernel) and **Python notebooks** (ipykernel)
are supported in the dev container — pick the kernel from JupyterLab's
launcher. R notebooks let you exercise the package as users do;
Python notebooks let you poke at the underlying picsurepy adapter
directly when you're debugging the reticulate bridge.

## The numbered tour

The numbered notebooks (`0_*` through `5_*`) walk through the package's
main surface area in order — each one runs standalone if you already
know that step, but together they're the recommended on-ramp:

| Notebook                        | What it covers                                                       |
|---------------------------------|----------------------------------------------------------------------|
| `0_Connect.ipynb`               | Opening sessions against authorized, open, and NHANES platforms.     |
| `1_Search.ipynb`                | `searchDictionary()`; narrowing with FacetSets.                      |
| `2_Query.ipynb`                 | One categorical FILTER → COUNT.                                      |
| `3_Open_Query.ipynb`            | REQUIRE clause → CROSS_COUNT on an open platform.                    |
| `4_Complex_Open_Queries.ipynb`  | Continuous FILTERs, AND/OR groups, nested groups.                    |
| `5_Export_Query_By_ID.ipynb`    | Reloading a saved query by UUID and running it.                      |

All six notebooks read your PIC-SURE access token from `notebooks/token.txt`
(one line, no trailing newline required). The file is gitignored by
the repo-wide `*.token` / `token.txt` rules — don't commit it.

## Why a dedicated directory?

The project gitignores `*.ipynb` everywhere *except* under
`notebooks/`. That gives you two things:

- **Committed notebooks land in one predictable place.** Reviewers
  don't have to hunt across the tree to find what's user-facing.
- **Scratch experiments stay out of git automatically.** A
  `scratch.ipynb` at the repo root or inside `R/` is invisible to
  `git add`. Use those freely without worrying about accidental
  commits.

## Defaults inside the dev container

`docker compose up notebook` launches JupyterLab with
`--ServerApp.preferred_dir=/workspace/notebooks`, so **File → New
Notebook** lands here by default. You can still navigate elsewhere via
the file browser; new notebooks created outside this directory just
won't be tracked.

## Conventions

- Clear all cell outputs before committing
  (`Kernel → Restart Kernel and Clear Outputs of All Cells`) so diffs
  don't drown in base64 image blobs.
- Keep the notebook self-contained: a brief markdown description of
  what it demonstrates, library/import calls at the top, and inputs
  that work against a published PIC-SURE dataset
  (e.g. `BDC_PREDEV_OPEN`).
- Never hardcode tokens. Read them from `.env` via `Sys.getenv()` (R)
  or `os.environ` (Python).
