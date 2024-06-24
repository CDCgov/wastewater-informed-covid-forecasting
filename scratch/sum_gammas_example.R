# This script is a toy example of a stan model with a single fit to
# iid sums of gammas estimating the cv and the mean of the gammas
library(rstan)
library(ggplot2)
# Simulating some data. Let's say we have n_draws each from distributions that
# are the sum over pop of iid gamma distributions with a shared mu and cv.
pop <- c(5, 100, 20)
mu_set <- 2
cv_set <- 1
n_draws <- 10
pop_vector <- rep(pop, each = n_draws)
y <- rep(0, n_draws * length(pop))
for (i in seq_along(pop_vector)) {
  alpha <- pop_vector[i] / cv_set^2
  beta <- 1 / (mu_set * (cv_set^2))
  y[i] <- rgamma(1, alpha, beta)
}

# Running stan code
model1 <- cmdstanr::cmdstan_model("scratch/natural_scale_gamma.stan")

fit1 <- model1$sample(
  data = list(
    N = length(pop_vector),
    y = y,
    pop_vector = pop_vector
  ),
  iter_warmup = 200,
  iter_sampling = 500,
  chains = 4
)

print(fit1)

draws <- fit1$draws()

params <- draws |>
  tidybayes::spread_draws(mu, cv) |>
  dplyr::mutate(
    mu_true = mu_set,
    cv_true = cv_set
  )

ggplot(params, aes(x = mu)) +
  stat_halfeye() +
  geom_vline(aes(xintercept = mu_true)) +
  theme_bw() +
  xlab("Mean") +
  ylab("")

ggplot(params, aes(x = cv)) +
  stat_halfeye() +
  geom_vline(aes(xintercept = cv_true)) +
  theme_bw() +
  xlab("Coefficient of Variation") +
  ylab("")





# Running stan code
model <- stan_model("scratch/ncp_norm_approx.stan")

fit <- sampling(model,
  list(
    N = length(pop),
    y = y,
    pop = pop
  ),
  warmup = 200,
  iter = 500,
  chains = 4
)

print(fit)

params <- extract(fit)
