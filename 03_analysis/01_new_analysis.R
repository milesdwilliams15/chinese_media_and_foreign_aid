#########################
# Updated analysis code #
#########################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse) # syntax
library(glmmTMB)   # mixed-effects neg. binom. models
library(texreg)    # printing regression output
library(here)      # reduce headaches navigating the working directory
theme_set(theme_light())

# data --------------------------------------------------------------------

dt <- read_csv(
  here('01_data/final_data/final_data_imputed.csv')
)

# descriptives ------------------------------------------------------------

# summary stats
stargazer::stargazer(
  as.data.frame(dt),
  type = 'text'
)

# aid and debt over time
dt %>%
  group_by(year) %>%
  summarize(
    Aid = sum(aid),
    Debt = sum(debt)
  ) %>%
  pivot_longer(
    Aid:Debt
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
  scale_color_manual(
    values = c(
      'Aid' = 'royalblue',
      'Debt' = 'indianred3'
    )
  ) +
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
  scale_color_manual(
    values = c('Recipient' = 'royalblue', 
               'Non-recipient' = 'indianred3')
  ) +
  facet_wrap(
    ~ name, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = 'Yearly Total',
    color = NULL
  ) +
  theme(
    legend.position = 'top'
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
  scale_color_manual(
    values = c('Debtor' = 'royalblue', 
               'Non-debtor' = 'indianred3')
  ) +
  facet_wrap(
    ~ name, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = 'Yearly Total',
    color = NULL
  ) +
  theme(
    legend.position = 'top'
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
  scale_color_manual(
    values = c('Recipient' = 'royalblue', 'Non-recipient' = 'indianred3')
  ) +
  facet_wrap(
    ~ name, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = 'Yearly Total per Country',
    color = NULL
  ) +
  theme(
    legend.position = 'top'
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
  scale_color_manual(
    values = c('Debtor' = 'royalblue', 'Non-debtor' = 'indianred3')
  ) +
  facet_wrap(
    ~ name, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = 'Yearly Total per Country',
    color = NULL
  ) +
  theme(
    legend.position = 'top'
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
  scale_color_manual(
    values = c(
      'None' = 'grey',
      'Aid' = 'royalblue',
      'Debt' = 'indianred3',
      'Both' = 'forestgreen'
    )
  ) +
  labs(
    x = NULL,
    y = 'N Countries',
    color = NULL
  ) +
  theme(
    legend.position = 'top'
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
  scale_color_manual(
    values = c(
      'None' = 'grey',
      'Aid' = 'royalblue',
      'Debt' = 'indianred3',
      'Both' = 'forestgreen'
    )
  ) +
  facet_wrap(
    ~ name, scales = 'free'
  ) +
  labs(
    x = NULL,
    y = 'Yearly Total per Country',
    color = NULL
  ) +
  theme(
    legend.position = 'top'
  ) 
  ggsave(
    here('06_figures/rate_coverage_visits_by_type.png'),
    height = 4,
    width = 6
  )

# analysis ----------------------------------------------------------------

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

type_form <- ~ type + income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi
cont_form <- update(type_form, ~ . - type + asinh(aid) + asinh(debt))

glmmTMB(
  update(type_form, counts_by_year ~ . + as.factor(year) + (1 | recipient)),
  data = new_dt,
  ziformula = ~1,
  family = nbinom1
) -> znb_counts_type
glmmTMB(
  update(type_form, total_visits ~ . + as.factor(year) + (1 | recipient)),
  data = new_dt,
  ziformula = ~1,
  family = nbinom2
) -> znb_visits_type
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
summary(znb_counts_type)
summary(znb_visits_type)
summary(znb_counts_cont)
summary(znb_visits_cont)

bind_rows(
  broom.mixed::tidy(
    znb_counts_type
  ),
  broom.mixed::tidy(
    znb_visits_type
  ),
  broom.mixed::tidy(
    znb_counts_cont
  ),
  broom.mixed::tidy(
    znb_visits_cont
  )
) %>%
  filter(
    term %in% c(paste0(
      'type', c('Aid', 'Debt', 'Both')
    ), 'asinh(aid)', 'asinh(debt)')
  ) %>%
  mutate(
    term = c(rep(
      c('Recipient', 'Debtor', 'Both'),
      len = 6
    ),
    rep(
      c('Aid (asinh)', 'Debt (asinh)'),
      len = 4
    )),
    Outcome = c(rep(
      c('Xinhua Coverage',
        'Diplomatic Visits'),
      each = 3
    ),
    rep(
      c('Xinhua Coverage',
        'Diplomatic Visits'),
      each = 2
    )),
    var_type = c(
      rep('Country Classification',
          len = 6),
      rep('Financing Received',
          len = 4)
    )
  ) -> fixed_effs
ggplot(fixed_effs) +
  aes(
    x = estimate,
    xmin = estimate - 1.96 * std.error,
    xmax = estimate + 1.96 * std.error,
    y = term,
    color = Outcome
  ) +
  facet_wrap(
    ~ var_type,
    scales = 'free'
  ) +
  geom_point(
    position = ggstance::position_dodgev(-.5)
  ) +
  geom_errorbarh(
    position = ggstance::position_dodgev(-.5),
    height = 0
  ) +
  labs(
    x = 'Coefficient with 95% CI',
    y = NULL,
    color = 'Outcome',
    caption = 'Mixed Effects Zero-inflated Negative Binomial Estimates'
  ) +
  geom_vline(
    xintercept = 0,
    lty = 2
  ) +
  scale_color_manual(
    values = c(
      'Diplomatic Visits' = 'royalblue',
      'Xinhua Coverage' = 'indianred3'
    )
  ) +
  theme(
    legend.position = 'top',
    plot.caption = element_text(
      hjust = .5
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
    width = 6
  )

tibble(
  None = predict(znb_counts_type, type = 'response', 
                 newdata = new_dt %>% mutate(type = unique(type[type=='None']))),
  Aid = predict(znb_counts_type, type = 'response', 
                newdata = new_dt %>% mutate(type = unique(type[type=='Aid']))),
  Debt = predict(znb_counts_type, type = 'response', 
                 newdata = new_dt %>% mutate(type = unique(type[type=='Debt']))),
  Both = predict(znb_counts_type, type = 'response', 
                 newdata = new_dt %>% mutate(type = unique(type[type=='Both'])))
) -> count_preds

tibble(
  None = predict(znb_visits_type, type = 'response', 
                 newdata = new_dt %>% mutate(type = unique(type[type=='None']))),
  Aid = predict(znb_visits_type, type = 'response', 
                newdata = new_dt %>% mutate(type = unique(type[type=='Aid']))),
  Debt = predict(znb_visits_type, type = 'response', 
                 newdata = new_dt %>% mutate(type = unique(type[type=='Debt']))),
  Both = predict(znb_visits_type, type = 'response', 
                 newdata = new_dt %>% mutate(type = unique(type[type=='Both'])))
) -> visit_preds

count_preds %>%
  summarize(
    across(
      everything(),
      mean
    )
  ) %>%
  pivot_longer(
    everything()
  ) -> count_out
visit_preds %>%
  summarize(
    across(
      everything(),
      mean
    )
  ) %>%
  pivot_longer(
    everything()
  ) -> visit_out
bind_rows(
  count_out %>%
    mutate(outcome = 'Xinhua Mentions'),
  visit_out %>%
    mutate(outcome = 'Diplomatic Visits')
) %>%
  group_by(outcome) %>%
  mutate(
    new_value = value / max(value)
  ) %>%
  ggplot() +
  aes(
    x = new_value,
    y = name,
    fill = outcome,
    label = round(value, 2)
  ) +
  geom_col(
    color = 'black',
    position = ggstance::position_dodgev(-.9)
  ) +
  geom_text(
    hjust = 1.2,
    color = 'white',
    position = ggstance::position_dodgev(-.9)
  ) +
  scale_fill_manual(
    values = c(
      'Diplomatic Visits' = 'royalblue',
      'Xinhua Mentions' = 'indianred3'
    )
  ) +
  scale_x_continuous(
    breaks = NULL
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = 'Outcome'
  ) +
  theme(
    legend.position = 'top'
  ) +
  guides(
    fill = guide_legend(
      title.position = 'top',
      title.hjust = 0.5
    )
  ) 
  ggsave(
    here('06_figures/predicted_differences.png'),
    height = 4,
    width = 6
  )
