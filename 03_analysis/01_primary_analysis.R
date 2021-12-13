#########################
# Primary Data Analysis
#########################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(censReg)
library(AER)
library(texreg)


# data --------------------------------------------------------------------

dt <- 
  read_csv(
    paste0(getwd(), "/01_data/final_data/final_data.csv")
  )


# summary stats -----------------------------------------------------------

summary(dt) # general summary

# histogram of aid
ggplot(dt) +
  aes(asinh(aid)) +
  geom_histogram() +
  labs(
    x = "ODA-like Contributions\n(inverse hyberbolic sine)",
    y = "Frequency"
  ) +
  theme_classic()



# variable transformations ------------------------------------------------

trans_dt <-
  dt %>%
  mutate(
    income = gdp / pop
  ) %>%
  mutate_at(
    c("aid", "count", "gdp", "income", "pop", "unemp",
      "disaster", "dist", "distw", 
      "distwces", "trade", "china_gdp"),
    asinh # use inverse hyperbolic sine rather than log(1 + x)
  ) %>%
  group_by(recipient_iso3) %>%
  mutate(
    count = lead(count, by = "year")
  ) %>%
  ungroup %>%
  na.omit

# descriptives ------------------------------------------------------------

ggplot(dt) + geom_histogram(aes(log(1+freq)))

trans_dt %>%
  ggplot() +
  aes(aid, count) +
  geom_point() +
  geom_smooth(method = "lm")

# setup panel -------------------------------------------------------------

pdata <- # for estimating random recipient effects with censReg
  plm::pdata.frame(trans_dt, c(id = "recipient_iso3", time = "year"))


# estimate main models ----------------------------------------------------

# Predicting Coverage
spec <- 
  count ~ aid + pmm_polity + gdp + pop + unemp + disaster + civilwar +
  dist + trade + atopally + as.factor(year)


ml_tobit <- function(eq, count = T) { # multilevel tobit with recipient random effects
  if(isTRUE(count)) {
    censReg(eq, data = pdata, method = "BHHH")
  } else {
    eq <- update(eq, ~. - aid)
    censReg(eq, data = pdata, method = "BHHH")
  }
}
lm_rob <- function(eq, count = T) { # level model est. w/ OLS + cluster-robust errors
  if(isTRUE(count)) {
    estimatr::lm_robust(eq, data = pdata, se_type = "stata", cluster = recipient)
  } else {
    eq <- update(eq, ~. - aid)
    estimatr::lm_robust(eq, data = pdata, se_type = "stata", cluster = recipient)
  }
}
# lm_prob <- function(...) { # decision model est. w/ OLS = cluster-robust errors
#   pdata <- mutate(pdata, count = (count>0)+0)
#   estimatr::lm_robust(..., data = pdata, se_type = "stata", cluster = recipient)
# }

output <- list(
  ml_tobit(spec, F),
  ml_tobit(spec),
  lm_rob(spec, F),
  lm_rob(spec)
)

coef_map <- list(
  "aid" = "Aid",
  "pmm_polity" = "Polity",
  "gdp" = "GDP",
  "pop" = "Population",
  "unemp" = "Unemployment",
  "disaster" = "Disaster Deaths",
  "civilwar" = "Civil War",
  "trade" = "Trade",
  "atopally" = "Alliance",
  "dist" = "Distance"
)

model_labs <- 
  c("(1)", "(2)", "(3)", "(4)")

tab1 <- screenreg(
  output,
  custom.model.names = model_labs,
  custom.coef.map = coef_map,
  include.ci = F,
  stars = c(0.001, 0.01, 0.05, 0.1),
  custom.header = list(
    "ML Tobit" = 1:2,
    "OLS" = 3:4
  )
)


# Predicting Aid
pdata <- # for estimating random recipient effects with censReg
  plm::pdata.frame(trans_dt, c(id = "recipient_iso3", time = "year"))

trans_dt <-
  dt %>%
  mutate(
    income = gdp / pop
  ) %>%
  mutate_at(
    c("aid", "count", "gdp", "income", "pop", "unemp",
      "disaster", "dist", "distw", 
      "distwces", "trade"),
    asinh # use inverse hyperbolic sine rather than log(1 + x)
  ) %>%
  group_by(recipient_iso3) %>%
  mutate(
    aid = lead(aid, by = "year")
  ) %>%
  ungroup %>%
  na.omit
spec <- 
  aid ~ count + pmm_polity + gdp + pop + unemp + disaster + civilwar +
  dist + trade + atopally + as.factor(year)


ml_tobit <- function(eq, count = T) { # multilevel tobit with recipient random effects
  if(isTRUE(count)) {
    censReg(eq, data = pdata, method = "BHHH")
  } else {
    eq <- update(eq, ~. - count)
    censReg(eq, data = pdata, method = "BHHH")
  }
}
lm_rob <- function(eq, count = T) { # level model est. w/ OLS + cluster-robust errors
  if(isTRUE(count)) {
    estimatr::lm_robust(eq, data = pdata, se_type = "stata", cluster = recipient)
  } else {
    eq <- update(eq, ~. - count)
    estimatr::lm_robust(eq, data = pdata, se_type = "stata", cluster = recipient)
  }
}
# lm_prob <- function(...) { # decision model est. w/ OLS = cluster-robust errors
#   pdata <- mutate(pdata, count = (count>0)+0)
#   estimatr::lm_robust(..., data = pdata, se_type = "stata", cluster = recipient)
# }

output <- list(
  ml_tobit(spec, F),
  ml_tobit(spec),
  lm_rob(spec, F),
  lm_rob(spec)
)

coef_map <- list(
  "count" = "Coverage",
  "pmm_polity" = "Polity",
  "gdp" = "GDP",
  "pop" = "Population",
  "unemp" = "Unemployment",
  "disaster" = "Disaster Deaths",
  "civilwar" = "Civil War",
  "trade" = "Trade",
  "atopally" = "Alliance",
  "dist" = "Distance"
)

model_labs <- 
  c("(1)", "(2)", "(3)", "(4)")

tab2 <- screenreg(
  output,
  custom.model.names = model_labs,
  custom.coef.map = coef_map,
  include.ci = F,
  stars = c(0.001, 0.01, 0.05, 0.1),
  custom.header = list(
    "ML Tobit" = 1:2,
    "OLS" = 3:4
  )
)


# regression tables -------------------------------------------------------
tab1
tab2

