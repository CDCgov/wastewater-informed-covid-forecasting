options(
  ## make HTTP requests
  ## identify us correctly
  ## to Posit package manager
  ## so we get appropriate
  ## precompiled binaries
  ## see https://docs.posit.co/rspm/1.0.12/admin/binaries.html#binaries-r-configuration
  HTTPUserAgent = sprintf(
    "R/%s R (%s)",
    getRversion(),
    paste(
      getRversion(),
      R.version["platform"], R.version["arch"],
      R.version["os"]
    )
  ),
  ## use Posit package manager to get
  ## precompiled binaries where possible
  repos = c(
    RSPM = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
  )
)

additional_deps <- c(
  "argparser"
)

install.packages("pak")
pak::pkg_install("github::stan-dev/cmdstanr")
cmdstanr::install_cmdstan()
pak::pkg_install("local::wweval")
pak::pkg_install(additional_deps)
dir.create("stanmodels")
wwinference::compile_model(
  target_dir = "stanmodels"
)
