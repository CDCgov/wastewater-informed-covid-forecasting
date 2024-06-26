ar1 <- function(mu, ac, sd, z) {
  n <- length(z)
  x <- rep(0, n)
  tvd <- rep(0, n)

  tvd[1] <- sd * z[1]
  x[1] <- mu[1] + tvd[1]

  for (i in 2:n) {
    tvd[i] <- ac * tvd[i - 1] + sd * z[i]
    x[i] <- mu[i] + tvd[i]
  }
  return(x)
}

p_hosp_mean <- rep(0.01, 26)
p_hosp_logit <- qlogis(p_hosp_mean)
ac <- 0.01
sd <- 0.3
z <- rnorm(26)

p_hosp_t_logit <- ar1(p_hosp_logit, ac, sd, z)

plot(plogis(p_hosp_t_logit))
