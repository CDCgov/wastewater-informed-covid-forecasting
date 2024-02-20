options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))

library(cmdstanr)
check_cmdstan_toolchain()
install_cmdstan(cores = 2)

install.packages("remotes")
install.packages("devtools")
remotes::install_local("cfaforecastrenewalww", upgrade = TRUE)
