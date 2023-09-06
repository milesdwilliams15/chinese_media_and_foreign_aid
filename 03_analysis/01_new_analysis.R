#########################
# Updated analysis code #
#########################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse) # syntax
library(glmmTMB)   # mixed-effects neg. binom. models
library(texreg)    # printing regression output
library(here)      # reduce headaches navigating the working directory
library(coolorrr)  # plotting theme and palette
set_palette()
set_theme()

# data --------------------------------------------------------------------

dt <- read_csv(
  here('01_data','final_data', 'final_data_imputed.csv')
)

# descriptives ------------------------------------------------------------

# summary stats
stargazer::stargazer(
  as.data.frame(dt %>% mutate(fdi = fdi/1000000)),
  type = 'latex',
  title = "Summary Statistics"
)

# aid and debt over time
dt %>%
  group_by(year) %>%
  summarize(
    ODA = sum(aid),
    OOF = sum(debt)
  ) %>%
  pivot_longer(
    ODA:OOF
  ) %>%
  mutate(
    value = value / 1000000
  ) %>%
  ggplot() +
  aes(
    x = year,
    y = value,
    color = name
  ) +
  geom_line(
    size = 0.75
  ) +
  scale_x_continuous(
    n.breaks = 10
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  ggpal() +
  labs(
    x = NULL,
    y = 'Yearly Total\n(millions $)',
    color = NULL
  ) +
  theme(
    legend.position = 'top'
  ) 
  ggsave(
    here('06_figures/aid_debt_trends.png'),
    height = 4,
    width = 6
  )

# coverage and visits by aid and debt
dt %>%
  group_by(year, aid > 0) %>%
  summarize(
    Coverage = sum(counts_by_year),
    'Diplomatic Visits' = sum(govt_visits + mil_visits)
  ) %>%
  pivot_longer(
    Coverage:`Diplomatic Visits`
  ) %>%
  ggplot() +
  aes(
    x = year,
    y = value,
    color = ifelse(`aid > 0`, 'Recipient', 'Non-recipient')
  ) +
  geom_line(
    size = 0.75
  ) +
  scale_x_continuous(
    n.breaks = 4
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  ggpal() +
  facet_wrap(
    ~ name, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = 'Yearly Total',
    color = "ODA"
  ) 
  ggsave(
    here('06_figures/total_coverage_visits_by_aid.png'),
    height = 4,
    width = 6
  )

dt %>%
  group_by(year, debt > 0) %>%
  summarize(
    Coverage = sum(counts_by_year),
    'Diplomatic Visits' = sum(govt_visits + mil_visits)
  ) %>%
  pivot_longer(
    Coverage:`Diplomatic Visits`
  ) %>%
  ggplot() +
  aes(
    x = year,
    y = value,
    color = ifelse(`debt > 0`, 'Debtor', 'Non-debtor')
  ) +
  geom_line(
    size = 0.75
  ) +
  scale_y_continuous(
    labels = scales::comma
  ) +
  ggpal() +
  facet_wrap(
    ~ name, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = 'Yearly Total',
    color = 'OOF'
  ) 
  ggsave(
    here('06_figures/total_coverage_visits_by_debt.png'),
    height = 4,
    width = 6 
  )
  
# show by rates rather than totals
dt %>%
  group_by(year, aid > 0) %>%
  summarize(
    Coverage = mean(counts_by_year),
    'Diplomatic Visits' = mean(total_visits)
  ) %>%
  pivot_longer(
    Coverage:`Diplomatic Visits`
  ) %>%
  ggplot() +
  aes(
    x = year,
    y = value,
    color = ifelse(`aid > 0`, 'Recipient', 'Non-recipient')
  ) +
  geom_line(
    size = 0.75
  ) +
  ggpal() +
  facet_wrap(
    ~ name, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = 'Yearly Total per Country',
    color = 'ODA'
  ) 
  ggsave(
    here('06_figures/rate_coverage_visits_by_aid.png'),
    height = 4,
    width = 6
  )

dt %>%
  group_by(year, debt > 0) %>%
  summarize(
    Coverage = mean(counts_by_year),
    'Diplomatic Visits' = mean(total_visits)
  ) %>%
  pivot_longer(
    Coverage:`Diplomatic Visits`
  ) %>%
  ggplot() +
  aes(
    x = year,
    y = value,
    color = ifelse(`debt > 0`, 'Debtor', 'Non-debtor')
  ) +
  geom_line(
    size = 0.75
  ) +
  ggpal() +
  facet_wrap(
    ~ name, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = 'Yearly Total per Country',
    color = 'OOF'
  ) 
  ggsave(
    here('06_figures/rate_coverage_visits_by_debt.png'),
    height = 4,
    width = 6
  )
bin <- function(x) {
  x <- x > 0
  #x <- x - mean(x)
  x
}

dt %>%
  mutate(
    type = socsci::frcode(
      !bin(aid) & !bin(debt) ~ 'None',
      bin(aid) & !bin(debt) ~ 'Aid',
      !bin(aid) & bin(debt) ~ 'Debt',
      TRUE ~ 'Both'
    )
  ) %>%
  group_by(
    type, year
  ) %>%
  count() %>%
  ggplot() +
  aes(
    x = year,
    y = n,
    color = type
  ) +
  geom_line(
    size = 0.75
  ) +
  ggpal() +
  labs(
    x = NULL,
    y = 'N Countries',
    color = NULL
  ) 
  ggsave(
    here('06_figures/dev_countries_over_time_by_type.png'),
    height = 4,
    width = 6
  )

dt %>%
  mutate(
    type = socsci::frcode(
      !bin(aid) & !bin(debt) ~ 'None',
      bin(aid) & !bin(debt) ~ 'Aid',
      !bin(aid) & bin(debt) ~ 'Debt',
      TRUE ~ 'Both'
    )
  ) %>%
  group_by(
    type, year
  ) %>%
  summarize(
    Coverage = mean(counts_by_year),
    'Diplomatic Visits' = mean(total_visits)
  ) %>%
  pivot_longer(
    Coverage:`Diplomatic Visits`
  ) %>%
  ggplot() +
  aes(
    x = year,
    y = value,
    color = type
  ) +
  geom_line(
    size = 0.75
  ) +
  ggpal() +
  facet_wrap(
    ~ name, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = 'Yearly Total per Country',
    color = NULL
  )  
  ggsave(
    here('06_figures/rate_coverage_visits_by_type.png'),
    height = 4,
    width = 6
  )

# analysis ----------------------------------------------------------------

  ## prep the data ----
dt %>%
  mutate(
    type = socsci::frcode(
      !bin(aid) & !bin(debt) ~ 'None',
      bin(aid) & !bin(debt) ~ 'Aid',
      !bin(aid) & bin(debt) ~ 'Debt',
      TRUE ~ 'Both'
    ),
    income = gdp / pop,
    across(
      c(income, pop, disaster, dist,
      imports, exports, fdi),
      asinh
    )
  ) -> new_dt

  ## the model formulas ----
type_form <- ~ type + income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi
cont_form <- update(type_form, ~ . - type + asinh(aid) + asinh(debt))

  ## neg binomial models ----
glmmTMB(
  update(cont_form, counts_by_year ~ . + as.factor(year) + (1 | recipient)),
  data = new_dt,
  ziformula = ~1,
  family = nbinom1
) -> znb_counts_cont
glmmTMB(
  update(cont_form, total_visits ~ . + as.factor(year) + (1 | recipient)),
  data = new_dt,
  ziformula = ~1,
  family = nbinom2
) -> znb_visits_cont

  ## OLS models ----
library(estimatr)
lm_robust(
  update(cont_form, asinh(counts_by_year) ~ .),
  data = new_dt,
  fixed_effects = ~ year + recipient,
  clusters = recipient,
  se_type = "stata"
) -> ols_counts_cont
lm_robust(
  update(cont_form, asinh(total_visits) ~ .),
  data = new_dt,
  fixed_effects = ~ year + recipient,
  clusters = recipient,
  se_type = "stata"
) -> ols_visits_cont

  ## poisson models ----
glm(
  update(cont_form, counts_by_year ~ . + as.factor(year)),
  data = new_dt,
  family = quasipoisson
) -> pml_counts_cont
glm(
  update(cont_form, total_visits ~ . + as.factor(year)),
  data = new_dt,
  family = quasipoisson
) -> pml_visits_cont

fits_to_save <- list(
  znb_counts_cont = znb_counts_cont,
  znb_visits_cont = znb_visits_cont,
  ols_counts_cont = ols_counts_cont,
  ols_visits_cont = ols_visits_cont,
  pml_counts_cont = pml_counts_cont,
  pml_visits_cont = pml_visits_cont
)
save(
  fits_to_save,
  file = here("03_analysis", "model_fits.R")
)

library(lmtest)
library(sandwich)
pstars <- function(x) {
  case_when(
    between(x, 0.1, 0) ~ " ",
    between(x, 0.05, 0.1) ~ "+",
    between(x, 0.01, 0.05) ~ "*",
    between(x, 0.001, 0.01) ~ "**",
    between(x, 0, 0.001) ~ "***"
  )
}
bind_rows(
  broom::tidy(
    pml_counts_cont %>%
      coeftest(vcov. = vcovCL(., cluster = new_dt$recipient,
                              type = "HC0"))
  ),
  broom::tidy(
    pml_visits_cont %>%
      coeftest(vcov. = vcovCL(., cluster = new_dt$recipient,
                              type = "HC0"))
  ),
 tidy(
    ols_counts_cont
  ),
  tidy(
    ols_visits_cont
  ),
  broom.mixed::tidy(
    znb_counts_cont
  ),
  broom.mixed::tidy(
    znb_visits_cont
  )
) %>%
  filter(
    term %in% c('asinh(aid)', 'asinh(debt)')
  ) %>%
  mutate(
    term = c(rep(
      c('ODA (asinh)', 'OOF (asinh)'),
      len = n()
    )),
    Outcome = rep(
      rep(c("Coverage", "Visits"), each = 2),
      len = n()
    ),
    var_type = 
      rep(c(
        "PPML", "OLS", "ZinfNB"
      ), each = 4)
  ) -> fixed_effs
ggplot(fixed_effs) +
  aes(
    x = estimate,
    xmin = estimate - 1.96 * std.error,
    xmax = estimate + 1.96 * std.error,
    y = term,
    color = Outcome,
    label = pstars(p.value)
  ) +
  geom_point(
    position = ggstance::position_dodgev(-.5)
  ) +
  geom_errorbarh(
    position = ggstance::position_dodgev(-.5),
    height = 0
  ) +
  geom_text(
    vjust = -.25,
    position = ggstance::position_dodgev(-.5),
    show.legend = F
  ) +
  labs(
    x = 'Coefficient with 95% CI',
    y = NULL,
    color = 'Outcome'
  ) +
  geom_vline(
    xintercept = 0,
    lty = 2
  ) +
  ggpal() +
  facet_wrap(~ var_type) +
  theme(
    legend.position = 'top',
    plot.caption = element_text(
      hjust = .5
    ),
    axis.text.x = 
      element_text(
        size = 10
      ),
    panel.grid.major.y = 
      element_line(
        linewidth = 20,
        color = "gray95"
      )
  ) +
  guides(
    color = guide_legend(
      title.position = 'top',
      title.hjust = 0.5
    )
  ) 
  ggsave(
    here('06_figures/neg_binom_estimates.png'),
    height = 4,
    width = 12
  )

## generate predictions
aid_vals <- seq(
  asinh(min(new_dt$aid[new_dt$recipient=="Pakistan"])), 
  asinh(max(new_dt$aid[new_dt$recipient=="Pakistan"])), 
  by = 1
) %>% sinh() %>% .[1:10]
debt_vals <- seq(
  asinh(min(new_dt$debt[new_dt$recipient=="Pakistan"])), 
  asinh(max(new_dt$debt[new_dt$recipient=="Pakistan"])), 
  by = 1
) %>% sinh() %>% .[1:10]
my_sample <- function(data, y, r) data %>%
  filter(year == y,
         recipient == r) %>%
  sample_n(10, replace = T)
nd1 <- new_dt %>%
  my_sample(., 2017, "Pakistan") %>%
  mutate(aid = aid_vals) %>%
  bind_cols(
    predict(znb_counts_cont, ., type = "link", se.fit = T)
  ) %>%
  mutate(
    count = exp(fit),
    ll  = exp(fit - 1.96 * se.fit),
    ul  = exp(fit + 1.96 * se.fit)
  )
nd2 <- new_dt %>%
  my_sample(., 2017, "Pakistan") %>%
  mutate(debt = debt_vals) %>%
  bind_cols(
    predict(znb_counts_cont, ., type = "link", se.fit = T)
  ) %>%
  mutate(
    count = exp(fit),
    ll  = exp(fit - 1.96 * se.fit),
    ul  = exp(fit + 1.96 * se.fit)
  )
nd3 <- new_dt %>%
  my_sample(., 2017, "Pakistan") %>%
  mutate(aid = aid_vals) %>%
  bind_cols(
    predict(znb_visits_cont, ., type = "link", se.fit = T)
  ) %>%
  mutate(
    count = exp(fit),
    ll  = exp(fit - 1.96 * se.fit),
    ul  = exp(fit + 1.96 * se.fit)
  )
nd4 <- new_dt %>%
  my_sample(., 2017, "Pakistan") %>%
  mutate(debt = debt_vals) %>%
  bind_cols(
    predict(znb_visits_cont, ., type = "link", se.fit = T)
  ) %>%
  mutate(
    count = exp(fit),
    ll  = exp(fit - 1.96 * se.fit),
    ul  = exp(fit + 1.96 * se.fit)
  )

bind_rows(
  nd1 %>% mutate(outcome = "Mentions", 
                 by = "Aid"),
  nd2 %>% mutate(outcome = "Mentions",
                 by = "Debt"),
  nd3 %>% mutate(outcome = "Visits",
                 by = "Aid"),
  nd4 %>% mutate(outcome = "Visits",
                 by = "Debt")
) %>%
  mutate(
    val = rep(c(aid_vals, debt_vals), len = n())
  ) -> plt_dt
ggplot(plt_dt) +
  aes(x = val + 1,
      y = count,
      ymin = ll,
      ymax = ul) +
  geom_line() +
  geom_ribbon(alpha = 0.4) +
  facet_grid(
    outcome ~ by,
    scales = "free"
  ) +
  scale_x_log10()

plt_dt %>%
  group_by(outcome, by) %>%
  summarize(
    mareff = mean(diff(count))
  ) -> mareff
its <- 2000
map_dfr(
  1:its,
  ~ plt_dt %>%
      group_by(outcome, by) %>%
      summarize(
        mareff = mean(diff(count + 
                             rnorm(n(), 
                                   se.fit))),
        .groups = "drop"
      )
) -> bmareff

bmareff %>%
  group_by(outcome, by) %>%
  summarize(
    se = sd(mareff)
  ) %>%
  full_join(
    mareff
  ) %>%
  ggplot() +
  aes(x = mareff,
      y = by,
      color = outcome) +
  geom_point() +
  geom_errorbarh(
    aes(xmin = mareff - 1.96 * se,
        xmax = mareff + 1.96 * se)
  )

library(ggridges)
ggplot(bmareff) +
  aes(x = mareff,
      y = by) +
  facet_wrap(~ outcome,
             scales = "free_x") +
  geom_density_ridges()

library(kableExtra)
mareff %>%
  kable(
    format = "latex",
    caption = "Marginal Effects",
    booktabs = T,
    linesep = "",
    digits = 2
  ) %>%
  footnote(
    general = "Estimates for Pakistan, 2017."
  )
