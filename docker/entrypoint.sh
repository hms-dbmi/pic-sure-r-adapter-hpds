#!/usr/bin/env bash
# Container entrypoint for the `dev` and `notebook` services:
#   1. Install/refresh project R dependencies when DESCRIPTION changes.
#   2. Pre-warm the reticulate-managed Python environment so the first
#      `library(picsure)` in an interactive session is snappy.
#   3. Exec the requested command.
#
# The `rstudio` service overrides ENTRYPOINT to rocker's `/init`, so
# this script does NOT run there — RStudio Server comes up via s6 the
# way the rocker image expects.

set -euo pipefail

R_LIB="${R_LIBS_USER:-/opt/R/library}"
STAMP="${R_LIB}/.deps-stamp"
DESCRIPTION="/workspace/DESCRIPTION"
PY_STAMP="${R_LIB}/.reticulate-stamp"

needs_install() {
    [ ! -f "${STAMP}" ] && return 0
    [ -f "${DESCRIPTION}" ] && [ "${DESCRIPTION}" -nt "${STAMP}" ] && return 0
    return 1
}

if [ -f "${DESCRIPTION}" ] && needs_install; then
    echo "==> Installing R package deps from DESCRIPTION (pak)"
    mkdir -p "${R_LIB}"
    cd /workspace
    # pak handles system libs, binary CRAN packages from PPM, and
    # dependency resolution in one shot. `dependencies = TRUE` picks
    # up Suggests so testthat/knitr/pkgdown land in the library.
    R --quiet --no-save -e "pak::pkg_install('local::.', dependencies = TRUE)"
    touch "${STAMP}"
fi

# Pre-warm reticulate's Python env on first run so users don't pay the
# uv resolve + venv create cost in their first interactive session.
# `library(picsure)` triggers the package's own .onLoad, which calls
# reticulate::py_require() with the correct (git-direct-reference)
# spec; py_config() then forces eager Python initialization so uv
# resolves and builds the venv. Cheap on subsequent runs because the
# venv is cached on a named volume.
if [ -f "${DESCRIPTION}" ] && [ ! -f "${PY_STAMP}" ]; then
    echo "==> Pre-warming reticulate Python environment"
    if R --quiet --no-save -e \
        "tryCatch({ suppressPackageStartupMessages(library(picsure)); reticulate::py_config() }, error = function(e) { message('reticulate pre-warm skipped: ', conditionMessage(e)); quit(status = 0) })"; then
        touch "${PY_STAMP}"
    fi
fi

exec "$@"
