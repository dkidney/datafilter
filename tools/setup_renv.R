# -----------------------------------------------------------
# setup renv
# -----------------------------------------------------------

# install renv
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# initialize renv if it hasn't already been initialized
if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
} else {
  renv::load()
}

# check renv status
renv::status()

# install Imports/Depends/etc. from DESCRIPTION file
renv::install(".")

# install dev packages
dev_pkgs = c(
  'devtools',
  'microbenchmark',
  'roxygen2',
  'usethis'
)
for (pkg in dev_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
      renv::install(pkg)
  }
}

# snapshot the environment
renv::snapshot()


# -----------------------------------------------------------
# setup testing
# -----------------------------------------------------------

usethis::use_testthat()



