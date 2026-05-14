# syntax=docker/dockerfile:1.7
#
# Dev container for picsure (R).
#
# Base: rocker/rstudio:4.4 — R 4.4 + RStudio Server on Ubuntu noble
# (24.04), with pandoc preinstalled (vignettes knit out of the box).
# LaTeX is not baked in; PDF vignettes are not required for
# `R CMD check` here.
#
# We keep the rocker-provided `rstudio` user (UID 1000) because RStudio
# Server's auth machinery expects it. In the compose file the service
# is named `dev`, but the user inside the container is `rstudio`.

FROM rocker/rstudio:4.4

ENV DEBIAN_FRONTEND=noninteractive

# Build deps for common R packages (curl, openssl, xml2, ragg, ...),
# git for version control inside the container, Python for reticulate,
# tini as PID 1 for the shell/notebook services, plus a couple of dev
# convenience tools.
RUN apt-get update && apt-get install -y --no-install-recommends \
        git \
        openssh-client \
        ca-certificates \
        curl \
        tini \
        less \
        vim-tiny \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libgit2-dev \
        libssh2-1-dev \
        libfontconfig1-dev \
        libfreetype6-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
        libicu-dev \
        libzmq3-dev \
        zlib1g-dev \
        python3 \
        python3-venv \
        python3-pip \
    && rm -rf /var/lib/apt/lists/*

# Pinned uv from the official image (no curl|sh). reticulate uses uv
# under the hood to provision the Python environment on first
# `library(picsure)`.
COPY --from=ghcr.io/astral-sh/uv:0.7.13 /uv /uvx /usr/local/bin/

# Allow callers to remap the rstudio user's UID/GID so bind-mounted
# files land with host ownership on Linux hosts. macOS Docker Desktop
# translates ownership in its VM, so this is mostly a no-op there.
ARG USER_UID=1000
ARG USER_GID=1000
RUN if [ "$(id -u rstudio)" != "${USER_UID}" ]; then usermod  -u ${USER_UID} rstudio; fi \
 && if [ "$(id -g rstudio)" != "${USER_GID}" ]; then groupmod -g ${USER_GID} rstudio; fi \
 && chown -R ${USER_UID}:${USER_GID} /home/rstudio

# Trust the bind-mounted workspace at /workspace for all users (rstudio
# inside the container, and root, in case `R CMD check` is run there).
RUN git config --system --add safe.directory /workspace

# Persistent R package library on a named volume — installed packages
# survive `--rm` runs, and the macOS host's library (incompatible
# binaries) never shadows the container's. uv cache likewise persists.
ENV R_LIBS_USER=/opt/R/library \
    R_LIBS_SITE=/opt/R/library \
    UV_CACHE_DIR=/home/rstudio/.cache/uv \
    UV_LINK_MODE=copy \
    RETICULATE_USE_MANAGED_VENV=yes \
    RETICULATE_UV=/usr/local/bin/uv \
    PATH=/home/rstudio/.local/bin:$PATH

RUN mkdir -p /opt/R/library /home/rstudio/.cache/uv /home/rstudio/.local \
 && chown -R rstudio:rstudio /opt/R/library /home/rstudio

# Site Rprofile: ensure the writable user library is on .libPaths()
# ahead of the read-only system tree so installs land on the named
# volume. Append to the existing Rprofile.site rocker ships.
RUN printf '\n%s\n' \
    'local({' \
    '  user_lib <- Sys.getenv("R_LIBS_USER", "/opt/R/library")' \
    '  if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)' \
    '  .libPaths(c(user_lib, .libPaths()))' \
    '})' \
    >> /usr/local/lib/R/etc/Rprofile.site

# JupyterLab for the `notebook` service. Installed first so that the
# IRkernel `installspec` step below can find `jupyter` on PATH when it
# registers the R kernel. python-lsp-server gives inline as-you-type
# completion for Python cells; R cells use the `languageserver` R
# package installed in the next step.
RUN pip install --break-system-packages --no-cache-dir \
        jupyterlab \
        jupyterlab-lsp \
        'python-lsp-server[all]' \
        ipykernel

# Bootstrap install: pak (fastest dep installer), IRkernel (R notebook
# kernel), languageserver (R LSP for VS Code / jupyterlab-lsp), and
# devtools (load_all / test / check loop). Project Suggests are
# installed at entrypoint time so DESCRIPTION changes don't trigger
# image rebuilds. installspec runs system-wide so all users see the
# R kernel without per-user setup.
RUN R -e "install.packages(c('pak', 'IRkernel', 'languageserver', 'devtools'), repos = 'https://packagemanager.posit.co/cran/__linux__/noble/latest')" \
 && R -e "IRkernel::installspec(user = FALSE)" \
 && chown -R rstudio:rstudio /opt/R/library

USER rstudio
WORKDIR /workspace

COPY --chown=rstudio:rstudio docker/entrypoint.sh /usr/local/bin/entrypoint.sh

# Default entrypoint covers the `dev` and `notebook` services. The
# `rstudio` service overrides ENTRYPOINT back to rocker's `/init` so
# s6 + rstudio-server come up normally.
ENTRYPOINT ["tini", "--", "/usr/local/bin/entrypoint.sh"]
CMD ["bash"]
