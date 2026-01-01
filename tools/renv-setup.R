# run renv-init.R

# ------------------------------------------------------------------------------
# Configure options and env vars
# ------------------------------------------------------------------------------
options(
  repos = getOption("repos"),
  renv.consent = TRUE
)
Sys.setenv(PAK_NO_PROMPT = "true")

# ------------------------------------------------------------------------------
# Configure Bioconductor version
# ------------------------------------------------------------------------------
# bio_version = '3.22'
# message('configuring Bioconductor version: ',  bio_version)
# renv::settings$bioconductor.version(bio_version)

# ------------------------------------------------------------------------------
# Install pak (non-interactively)
# ------------------------------------------------------------------------------
if (!requireNamespace("pak", quietly = TRUE)) {
  message('installing pak')
  install.packages("pak", ask = FALSE)
}

# ------------------------------------------------------------------------------
# Install packages from DESCRIPTION
# ------------------------------------------------------------------------------
if (file.exists("DESCRIPTION")) {
  message('installing dependencies from DESCRIPTION')
  pak::pkg_install(".", ask = FALSE)
}

# ------------------------------------------------------------------------------
# Install development / auxiliary packages
# ------------------------------------------------------------------------------
message('installing development / auxiliary packages')

cran_pkgs <- c(

  # imports
  'dplyr',
  'pillar',
  'purrr',
  'stats',
  'stringr',
  'tibble',
  'tidyr',

  # dev
  'devtools',
  'microbenchmark',
  'roxygen2',
  'usethis'
)

bioc_pkgs <- c(
)

github_pkgs = c(
)

pak::pkg_install(c(cran_pkgs, bioc_pkgs, github_pkgs), ask = FALSE)

# ------------------------------------------------------------------------------
# Check status
# ------------------------------------------------------------------------------
# message('checking for issues')
# renv::status()

# ------------------------------------------------------------------------------
# Snapshot environment
# ------------------------------------------------------------------------------
message('taking snapshot')
renv::snapshot(prompt = FALSE)

