# Docker development environment

A pre-configured container that lets contributors run `picsure`, its
tests, vignettes, and a JupyterLab / RStudio Server sandbox without
installing R, system libraries, Python, or project dependencies
locally. The only host requirement is Docker.

## Quick start

```bash
docker compose run --rm dev          # interactive shell (devtools, R CMD check, git)
docker compose up notebook           # JupyterLab on http://127.0.0.1:8888
docker compose up rstudio            # RStudio Server on http://127.0.0.1:8787
```

First start of the `dev` or `notebook` service installs the package's
DESCRIPTION dependencies into a named volume; subsequent starts reuse
it. The install repeats automatically when `DESCRIPTION` changes.

## Three services, one image

| Service    | What it's for                                                              | Port  |
|------------|----------------------------------------------------------------------------|-------|
| `dev`      | Interactive shell — `devtools::load_all()`, `devtools::test()`, R CMD check, git | —     |
| `notebook` | JupyterLab with both R (IRkernel) and Python (ipykernel) kernels           | 8888  |
| `rstudio`  | RStudio Server, no-auth, host-only — the canonical R IDE                   | 8787  |

All three use the same image and share the same `/opt/R/library`, so
anything you `install.packages()` in one service is immediately
available in the other two.

The notebook service includes `jupyterlab-lsp` + `python-lsp-server`
for inline as-you-type completion in Python cells, and the
`languageserver` R package for the same in R cells.

### Where notebooks live

JupyterLab opens with **File → New Notebook** defaulting to
`/workspace/notebooks/`. That directory is the only place where
`*.ipynb` files are tracked by git — see
[`notebooks/README.md`](../notebooks/README.md) for the convention.
Scratch notebooks created anywhere else are gitignored automatically.

## What's mounted

| Host path                | Container path                  | Why                                                  |
|--------------------------|---------------------------------|------------------------------------------------------|
| `./`                     | `/workspace`                    | Repo. Live edits in either direction.                |
| `picsure-r-library` vol. | `/opt/R/library`                | R package library, isolated from the host's macOS library. |
| `uv-cache` vol.          | `/home/rstudio/.cache/uv`       | Persistent uv download cache for reticulate.         |
| `~/.gitconfig`           | `/home/rstudio/.gitconfig`      | Your git identity, so commits inside are yours.      |
| `~/.ssh`                 | `/home/rstudio/.ssh`            | SSH keys for `git push`.                             |
| `./.env`                 | (via `env_file`, optional)      | Integration-test tokens.                             |

The R library lives at `/opt/R/library` (set via `R_LIBS_USER`) so the
host's macOS library never conflicts with the container's Linux one —
package binaries don't cross OSes.

## The iteration loop

Inside any shell (the `dev` service or a Terminal pane in RStudio):

```r
# Load the package without installing — picks up edits to R/ instantly.
devtools::load_all()

# Run the full test suite.
devtools::test()

# Run a single test file.
devtools::test_active_file("tests/testthat/test-query.R")

# Build the pkgdown site (output: docs/).
pkgdown::build_site()
```

`R CMD check` runs the same way it does in CI:

```bash
docker compose run --rm dev R CMD check .
```

The `.Rbuildignore` additions (see below) keep `docker/`,
`docker-compose.yml`, `.dockerignore`, `.devcontainer/`, `notebooks/`,
and `docs/development-docker.md` out of the package tarball so
`R CMD check` doesn't flag them.

## IDE setup

### RStudio Server (recommended for R-first workflows)

```bash
docker compose up rstudio
```

Open <http://127.0.0.1:8787> in your browser. Auth is disabled and the
port is bound to loopback only — the IDE is not reachable from your
LAN. RStudio's working directory is `/workspace`, and the project's R
library is already on `.libPaths()`.

If port 8787 is taken on your host, override the port in
`docker-compose.override.yml`:

```yaml
services:
  rstudio:
    ports:
      - "127.0.0.1:8788:8787"
```

### VS Code (Dev Containers)

The repo ships a `.devcontainer/devcontainer.json` that targets the
`dev` compose service.

1. Install [Docker Desktop](https://www.docker.com/products/docker-desktop/)
   and the [**Dev Containers**](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
   extension (`ms-vscode-remote.remote-containers`).
2. Open the repo folder in VS Code.
3. Either click the **"Reopen in Container"** prompt, or run
   `Dev Containers: Reopen in Container` from the command palette
   (`Cmd/Ctrl + Shift + P`).
4. VS Code builds the image (first time only), starts the `dev`
   service, and reopens itself attached to the container. The R
   extension (`reditorsupport.r`) auto-installs and uses the
   `languageserver` R package baked into the image for LSP features.
5. Open a terminal (`` Ctrl + ` ``) — you're now in a bash shell
   inside `/workspace`.

### Positron

[Positron](https://positron.posit.co/) is Posit's newer
RStudio-successor IDE. It supports remote sessions over SSH; if you
want to use it against this container, expose port 22 in
`docker-compose.override.yml`, install `openssh-server` via an
override Dockerfile, and connect from Positron's **New Remote Session**
panel. Most users will be happier with the `rstudio` service plus
their browser.

### Editor without R plugin support

You don't need an IDE integration at all. Edit files in any editor on
the host; run everything in the container shell:

```bash
docker compose run --rm dev          # one shell session
# ... edit files in your editor of choice ...
# ... commands run in the docker shell ...
```

This is the minimal-friction path and works for any editor that can
edit text.

## reticulate / Python interop

`library(picsure)` provisions its Python dependency (picsurepy) on
first call via reticulate + uv. The container has `uv` on `PATH` and
the uv cache is persisted on a named volume, so first-call latency is
bounded by the package download.

The container's entrypoint pre-warms the reticulate environment on
first start, so opening an R session and calling
`bdc.initializeClient(...)` for the first time doesn't sit on a uv
resolve for ten seconds.

## Adding AI coding agents

The container is agent-agnostic. To use an agent that runs as a CLI
inside the container, drop a `docker-compose.override.yml` next to
`docker-compose.yml` with the mounts and env it needs. Compose merges
override files automatically.

The agent's CLI needs to be on `PATH` inside the container. Two ways:

1. **Install at container start.** Add an `install.sh` to your
   override that runs in your shell session (`pipx install ...`,
   `npm i -g ...`).
2. **Bake into a personal image.** Create a `docker/Dockerfile.local`
   that `FROM picsure-r-dev:latest` and installs the agent globally,
   then point the override at it via `build: dockerfile:`.

### Example: mounting agent config and credentials

```yaml
# docker-compose.override.yml
services:
  dev:
    volumes:
      # Claude Code
      - ${HOME}/.claude:/home/rstudio/.claude
      - ${HOME}/.config/claude-code:/home/rstudio/.config/claude-code
      # GitHub CLI (gh, gh-copilot)
      - ${HOME}/.config/gh:/home/rstudio/.config/gh
    environment:
      ANTHROPIC_API_KEY: ${ANTHROPIC_API_KEY:-}
      OPENAI_API_KEY:    ${OPENAI_API_KEY:-}
```

### A note on credentials

The default mounts (`~/.gitconfig`, `~/.ssh`) are **read-write**. A
process in the container can modify your host git config or
known_hosts. This is intentional — it makes `git push`, `ssh`, and
agent-driven workflows just work — but only mount agent credentials
you're willing to expose to anything running in the container.

## `.Rbuildignore` additions

The Docker-related files are added to `.Rbuildignore` so they don't
end up in the package tarball that `R CMD build` produces, and so
`R CMD check` doesn't complain about non-standard top-level files:

- `docker/` — Dockerfile + entrypoint
- `docker-compose.yml`
- `.dockerignore`
- `.devcontainer/`
- `notebooks/` — Jupyter notebooks aren't part of the installed package
- `docs/development-docker.md` — `docs/` is already excluded, listed
  here for clarity

## Bumping the R version

The base image is pinned to `rocker/rstudio:4.4`. To target a
different R version (e.g. 4.3 to match a downstream environment),
change the `FROM` line in `docker/Dockerfile` and rebuild:

```bash
docker compose build --no-cache
```

CI runs the full matrix (R 4.1 / 4.3 / 4.4 on Ubuntu and macOS) — the
container only needs to track the version you're actively developing
against.

## Vignette knitting and PDFs

`pandoc` is included in the base image, so HTML vignettes knit out of
the box (`devtools::build_vignettes()`, `pkgdown::build_site()`).

LaTeX is **not** baked in. If you need PDF vignettes, install
`tinytex` once inside a `dev` shell:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

The TeX install lands in the user library volume so it persists.

## Rebuilding

The image rebuilds automatically when `docker/Dockerfile` changes. To
force a rebuild:

```bash
docker compose build --no-cache
```

To wipe the R library and start fresh:

```bash
docker compose down -v   # removes named volumes including picsure-r-library
docker compose run --rm dev   # triggers a fresh pak install
```

## Apple Silicon / linux/arm64

The `rocker/rstudio:4.4` base is multi-arch; the build runs natively
on Apple Silicon with no emulation. If you need x86_64 explicitly
(e.g. to match a CI image), add `platform: linux/amd64` to the
service in `docker-compose.override.yml`.
