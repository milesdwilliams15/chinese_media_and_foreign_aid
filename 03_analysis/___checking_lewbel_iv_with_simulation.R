###################################################################
# A check of the Lewbel (2012) approach to instrumental variables
###################################################################

library(tidyverse)
library(seerrr)


# simulate some endogenous data -------------------------------------------

simulate( # based on example at:
          # https://www.rdocumentation.org/packages/ivlewbel/versions/1.1/topics/lewbel
  R = 10000,
  x1 = rnorm(N, 0, 1),
  x2 = rnorm(N, 0, 1),
  u = rnorm(N, 0, 1),
  s1 = rnorm(N, 0, 1),
  s2 =  rnorm(N, 0, 1),
  ov = rnorm(N, 0, 1),
  z1 = rnorm(N, 0 ,1),
  e1 = u + exp(x1)*s1 + exp(x2)*s1,
  e2 = u + exp(-x1)*s2 + exp(-x2)*s2,
  y1 = 1 + x1 + x2 + ov + e2 + 2*z1,
  y2 = 1 + x1 + x2 + y1 + 2*ov + e1,
) -> sim.data


# use lewbel estimator ----------------------------------------------------

# recover the "instruments"
sim.data %>%
  lapply(
    X = .,
    FUN = function(x) {
      fit <- lm(y1 ~ x1 + x2, data = x)
      eps <- resid(fit)
      z   <- with(x, cbind(x1, x2))
      zbr <- apply(z, 2, function(x) (x - mean(x)) * eps^2)
      colnames(zbr) <- paste0("z", 1 + 1:ncol(zbr))
      zbr <- as_tibble(zbr)
      bind_cols(x, zbr)
    }
  ) -> sim.data

# Estimate models

estimate( # the lewbel instruments
  sim.data,
  y2 ~ y1 + x1 + x2 | z2 + z3 + x1 + x2,
  vars = "y1",
  estimator = iv_robust,
  se_type = "stata"
) -> sim.est1

estimate( # the exogenous instrument
  sim.data,
  y2 ~ y1 + x1 + x2 | z1 + x1 + x2,
  vars = "y1",
  estimator = iv_robust,
  se_type = "stata"
) -> sim.est2

estimate( # no instrument
  sim.data,
  y2 ~ y1 + x1 + x2,
  vars = "y1",
  se_type = "stata"
) -> sim.est3


# evaluate performance ----------------------------------------------------

evaluate(
  sim.est1, 
  what = "bias", 
  truth = 1
) -> sim.eval1
evaluate(
  sim.est2, 
  what = "bias", 
  truth = 1
) -> sim.eval2
evaluate(
  sim.est3, 
  what = "bias", 
  truth = 1
) -> sim.eval3

bind_rows(
  sim.eval1,
  sim.eval2,
  sim.eval3
) %>%
  mutate(
    estimator = c(
      "Lewbel",
      "IV",
      "OLS"
    )
  ) -> sim.smry


# visualize ---------------------------------------------------------------

ggplot(sim.smry) +
  aes(
    abs(bias),
    sqrt(mse)
  ) +
  geom_point() +
  geom_vline(
    xintercept = 0, lty = 2
  ) +
  geom_hline(
    yintercept = 0, lty = 2
  ) +
  geom_label(
    aes(label = estimator)
  ) + 
  labs(
    x = "Absolute Bias",
    y = "RMSE",
    title = "Results from 10,000 simulations"
  ) +
  theme_bw()

