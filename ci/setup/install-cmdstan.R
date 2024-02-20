options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
