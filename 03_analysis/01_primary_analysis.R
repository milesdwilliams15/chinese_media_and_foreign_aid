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
    c("aid", "gdp", "income", "pop", "unemp",
      "disaster", "dist", "distw", 
      "distwces", "trade"),
    asinh # use inverse hyperbolic sine rather than log(1 + x)
  )


# setup panel -------------------------------------------------------------

pdata <- # for estimating random recipient effects with censReg
  plm::pdata.frame(trans_dt, c(id = "recipient_iso3", time = "year"))


# estimate main models ----------------------------------------------------

# Main specification
spec <- 
  aid ~ pmm_polity + gdp + pop + unemp + disaster + civilwar +
  dist + trade + atopally + as.factor(year)


cl_tobit <- function(...) { # classic tobit with clustered errors
  nf <- update(..., ~ . + cluster(recipient))
  tobit(nf, data = pdata)
}
ml_tobit <- function(...) { # multilevel tobit with recipient random effects
  censReg(..., data = pdata, method = "BHHH")
}
lm_rob <- function(...) { # level model est. w/ OLS + cluster-robust errors
  pdata <- filter(pdata, aid>0)
  estimatr::lm_robust(..., data = pdata, se_type = "stata", cluster = recipient)
}
lm_prob <- function(...) { # decision model est. w/ OLS = cluster-robust errors
  pdata <- mutate(pdata, aid = (aid>0)+0)
  estimatr::lm_robust(..., data = pdata, se_type = "stata", cluster = recipient)
}

output <- list(
  cl_tobit(spec),
  ml_tobit(spec),
  lm_prob(spec),
  lm_rob(spec)
)

coef_map <- list(
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
  c("Tobit", "ML Tobit", "Decision (OLS)", "Level (OLS)")

screenreg(
  output,
  custom.model.names = model_labs,
  custom.coef.map = coef_map,
  include.ci = F,
  stars = c(0.001, 0.01, 0.05, 0.1)
)
