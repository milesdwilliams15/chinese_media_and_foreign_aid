#########################
# Primary Data Analysis
#########################

# data --------------------------------------------------------------------

dt <- 
  read_csv(
    paste0(getwd(), "/01_data/final_data/", data_set)
  )


# variable transformations ------------------------------------------------

trans_dt <-
  dt %>%
  mutate(
    income = gdp / pop
  ) %>%
  mutate_at(
    c("aid", "count", "govt_visits", "gdp", "income", "pop", "unemp",
      "disaster", "dist", "distw", 
      "distwces", "trade", "china_gdp"),
    asinh # use inverse hyperbolic sine rather than log(1 + x)
  ) %>%
  group_by(recipient_iso3) %>%
  mutate(
    aid = lead(aid, by = "year"),
    lead_count = lead(count, by = "year"),
    lead_visits = lead(govt_visits, by = "year")
  ) %>%
  ungroup %>%
  na.omit


# setup panel -------------------------------------------------------------

pdata <- # for estimating random recipient effects with censReg
  plm::pdata.frame(trans_dt, c(id = "recipient_iso3", time = "year"))


# estimate main models ----------------------------------------------------

spec <- # specification without predictors of interest 
  aid ~ pmm_fh + gdp + pop + unemp + disaster + civilwar +
  dist + trade + atopally + as.factor(year)

# models to estimate:
# 1. OLS
# 2. multilevel Tobit with lag of dv
# 3. selection (ml logit) and level (ols) with lag of dv
# 4. lag of dv as instrument for in-year dv
# 5. internal instruments via Lewbel approach

## OLS
ols <- lm_robust(
  update(spec, ~ count + govt_visits + .),
  data = pdata,
  se_type = "stata",
  clusters = recipient_iso3
)

## ML Tobit
if(data_set == "final_data_nonimputed.csv") {
  ml_tobit <- NULL  
} else {
  ml_tobit <- censReg(
    update(spec, ~ count + govt_visits + .),
    data = pdata,
    method = "BHHH"
  )
}


## Selection and Level
glm_select <- gam(
  update(spec, aid>0 ~ count + govt_visits + . +
           s(recipient_iso3, bs = "re")),
  data = pdata %>% mutate(recipient_iso3 = as.numeric(as.factor(recipient_iso3))),
  family = binomial,
  method = "REML"
)
ols_level <- lm_robust(
  update(spec, ~ count + govt_visits + .),
  data = pdata %>% filter(aid>0),
  se_type = "stata",
  clusters = recipient_iso3
)

## With lag of DV as IV
iv_spec <- 
  aid ~ lead_count + lead_visits + pmm_fh + gdp + pop + unemp + disaster + civilwar +
    dist + trade + atopally + as.factor(year) |
    count + govt_visits +  pmm_fh + gdp + pop + unemp + disaster + civilwar +
    dist + trade + atopally + as.factor(year)
iv_lag <- iv_robust(
  iv_spec,
  data = pdata,
  se_type = "stata",
  clusters = recipient_iso3
)

## Lewbel Instruments
# stage1.fit <- lm(
#   update(spec, lead_count ~ .),
#   data = pdata
# )
# eps <- resid(stage1.fit)
# z <- model.frame(
#   update(spec, ~ . - as.factor(year)),
#   data = pdata
# )[, -1]
# zbr <- apply(z, 2, function(x) (x - mean(x)) * eps^2)
# colnames(zbr) <- paste0("z", 1:ncol(zbr))
# zbr <- as_tibble(zbr)
# ndata <- bind_cols(pdata, zbr)
# lewbel_spec <- 
#   aid ~ lead_count + govt_visits + pmm_fh + gdp + pop + unemp + disaster + civilwar +
#     dist + trade + atopally + as.factor(year) |
#     pmm_fh + gdp + pop + unemp + disaster + civilwar +
#     dist + trade + atopally + as.factor(year) + 
#     z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9
# iv_lewbel <- iv_robust(
#   lewbel_spec,
#   data = ndata,
#   se_type = "stata",
#   clusters = recipient_iso3
# )

## Make regression table to summarize results:
coef_map <- list(
  "count" = "Coverage (lag)",
  "lead_count" = "Coverage (in-year)",
  "govt_visits" = "Diplomatic Visits (lag)",
  "lead_visits" = "Diplomatic Visits (in-year)",
  "pmm_fh" = "Polity",
  "gdp" = "GDP",
  "pop" = "Population",
  "unemp" = "Unemployment",
  "disaster" = "Disaster Deaths",
  "civilwar" = "Civil War",
  "trade" = "Trade",
  "atopally" = "Alliance",
  "dist" = "Distance"
)

model_list <- list(
  "OLS" = ols,
  "ML Tobit" = ml_tobit, 
  "Selection" = glm_select, 
  "Level" = ols_level, 
  "IV Lag" = iv_lag
  #"IV Lewbel" = iv_lewbel
)
if(data_set == "final_data_nonimputed.csv") {
  model_list[[2]] <- NULL
}

reg.tab <- texreg(
  model_list,
  custom.coef.map = coef_map,
  include.ci = F,
  stars = c(0.001, 0.01, 0.05, 0.1),
  caption = "Model Estimates",
  caption.above = T
)

## Save the regression table:
save(
  reg.tab,
  file = paste0(getwd(), "/03_analysis/tab",i,".R")
)


# results by region -------------------------------------------------------

spec <- # specification without predictor of interest 
  aid ~ govt_visits + pmm_fh + gdp + pop + unemp + disaster + civilwar +
  dist + trade + atopally + as.factor(year)
pdata <- mutate(pdata, region = str_replace(region, "&", "+"))

# models to estimate by region:
# 1. OLS
# 2. lag of dv as instrument for in-year dv
# 3. internal instruments via Lewbel approach

## OLS
ols_region <- pdata %>% 
  
  # Split data by region
  group_split(
    region
  ) %>% 
  
  # Estimate models for each region
  map(
    ~ lm_robust(
        update(spec, ~ count + .),
        data = .,
        se_type = "stata",
        clusters = recipient_iso3
      )
  ) %>%
  
  # Ensure the list of models is appropriately labeled
  set_names(
    sort(unique(pdata$region))
  )
  

## With lag of DV as IV
iv_spec <- 
  aid ~ lead_count + lead_visits + pmm_fh + gdp + pop + unemp + disaster + civilwar +
  dist + trade + atopally + as.factor(year) |
  count + govt_visits +  pmm_fh + gdp + pop + unemp + disaster + civilwar +
  dist + trade + atopally + as.factor(year)
iv_lag_region <- pdata %>%
  
  # Split data by region
  group_split(
    region
  ) %>%
  
  # Estimate models by region
  map(
    ~ iv_robust(
        iv_spec,
        data = .,
        se_type = "stata",
        clusters = recipient_iso3
      )
  ) %>%
  
  # Label the models
  set_names(
    sort(unique(pdata$region))
  )
  

# ## Lewbel Instruments
# iv_lewbel_region <- pdata %>%
#   
#   # Split by region
#   group_split(
#     region
#   ) %>%
#   
#   # Estiamte models by region
#   map(
#     ~ {
#       stage1.fit <- lm(
#         update(spec, lead_count ~ .),
#         data = .
#       )
#       eps <- resid(stage1.fit)
#       z <- model.frame(
#         update(spec, ~ . - as.factor(year)),
#         data = .
#       )[, -1]
#       zbr <- apply(z, 2, function(x) (x - mean(x)) * eps^2)
#       colnames(zbr) <- paste0("z", 1:ncol(zbr))
#       zbr <- as_tibble(zbr)
#       ndata <- bind_cols(., zbr)
#       lewbel_spec <- 
#         aid ~ lead_count + govt_visits + pmm_fh + gdp + pop + unemp + disaster + civilwar +
#         dist + trade + atopally + as.factor(year) |
#         pmm_fh + gdp + pop + unemp + disaster + civilwar +
#         dist + trade + atopally + as.factor(year) + 
#         z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9
#       iv_robust(
#         lewbel_spec,
#         data = ndata,
#         se_type = "stata",
#         clusters = recipient_iso3
#       )
#     }
#   ) %>%
#   
#   # Label the models
#   set_names(
#     sort(unique(pdata$region))
#   )

## Make regression tables to summarize results:
coef_map <- list(
  "count" = "Coverage (lag)",
  "lead_count" = "Coverage (in-year)",
  "govt_visits" = "Diplomatic Visits (lag)",
  "lead_visits" = "Diplomatic Visits (in-year)",
  "pmm_fh" = "Polity",
  "gdp" = "GDP",
  "pop" = "Population",
  "unemp" = "Unemployment",
  "disaster" = "Disaster Deaths",
  "civilwar" = "Civil War",
  "trade" = "Trade",
  "atopally" = "Alliance",
  "dist" = "Distance"
)

ols.region <- texreg(
  ols_region,
  custom.coef.map = coef_map,
  include.ci = F,
  stars = c(0.001, 0.01, 0.05, 0.1),
  caption = "OLS Estimates by Region"
)
ivlag.region <- texreg(
  iv_lag_region,
  custom.coef.map = coef_map,
  include.ci = F,
  stars = c(0.001, 0.01, 0.05, 0.1),
  caption = "IV Lag Estimates by Region"
)
# ivlew.region <- texreg(
#   iv_lewbel_region,
#   custom.coef.map = coef_map,
#   include.ci = F,
#   stars = c(0.001, 0.01, 0.05, 0.1),
#   caption = "Lewbel IV Estimates by Region"
# )

## Save the regression table:
save(
  ols.region,
  file = paste0(getwd(), "/03_analysis/ols_region_tab",i,".R")
)
save(
  ivlag.region,
  file = paste0(getwd(), "/03_analysis/ivlag_region_tab",i,".R")
)
# save(
#   ivlew.region,
#   file = paste0(getwd(), "/03_analysis/ivlew_region_tab",i,".R")
# )