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
  rename(
    count = counts_by_year
  ) %>%
  mutate_at(
    c("aid", "debt", "count", "govt_visits", "income", "pop",
      "disaster", "dist", "distw", 
      "distwces", "imports", "exports", "fdi"),
    asinh # use inverse hyperbolic sine rather than log(1 + x)
  ) %>%
  group_by(recipient) %>%
  mutate(
    lead_aid = lead(aid, order_by = year),
    lead_debt = lead(debt, order_by = year),
    lead_count = lead(count, order_by = year),
    lead_visits = lead(govt_visits, order_by = year)
  ) %>%
  ungroup %>%
  na.omit %>%
  distinct(recipient, year, .keep_all = T)


# setup panel -------------------------------------------------------------

pdata <- # for estimating random recipient effects with censReg
  plm::pdata.frame(trans_dt, c(id = "recipient", time = "year"))


# estimate main models ----------------------------------------------------

spec_1 <- # specifications without predictors of interest 
  lead_count ~ aid + debt + income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi
spec_2 <-
  lead_visits ~ aid + debt + income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi
  

# models to estimate:
# 1. OLS with TWFE
# 2. multilevel Tobit with lag of dv and year FE
# 3. selection (ml logit) and level (ols) with lag of dv with year FE
# 4. lag of dv as instrument for in-year dv with TWFE
# 5. internal instruments via Lewbel approach with TWFE

## OLS
ols_1 <- lm_robust(
  update(spec_1, ~ .),
  data = pdata,
  fixed_effects = ~ recipient + year,
  se_type = "stata",
  clusters = recipient
)
ols_2 <- lm_robust(
  update(spec_2, ~ .),
  data = pdata,
  fixed_effects = ~ recipient + year,
  se_type = "stata",
  clusters = recipient
)

## ML Tobit
# if(data_set == "final_data_nonimputed.csv") {
#   ml_tobit_1 <- NULL
#   ml_tobit_2 <- NULL
# } else {
#   ml_tobit_1 <- censReg(
#     update(spec_1, ~ . + count + as.factor(year)),
#     data = pdata,
#     method = "BHHH"
#   )
#   library(plm)
#   ml_tobit_1 <- pldv(
#     update(spec_1, ~ . + as.factor(year)),
#     data = trans_dt,
#     index = c("recipient"),
#     model = "fd",
#     objfun = "lad",
#     sample = "cens"
#   )
#   ml_tobit_2 <- censReg(
#     update(spec_2, ~ . + govt_visits + as.factor(year)),
#     data = pdata,
#     method = "BHHH"
#   )
# }


## Selection and Level
glm_select_1 <- gam(
  update(spec_1, lead_count>0 ~ . +
           count +
           as.factor(year) +
           s(recipient, bs = "re")),
  data = pdata %>% mutate(recipient = as.numeric(as.factor(recipient))),
  family = binomial,
  method = "REML"
)

glm_select_2 <- gam(
  update(spec_2, lead_visits>0 ~ . +
           govt_visits +
           as.factor(year) +
           s(recipient, bs = "re")),
  data = pdata %>% mutate(recipient = as.numeric(as.factor(recipient))),
  family = binomial,
  method = "REML"
)

## Level
level_1 <- lm_robust(
  update(spec_1, ~ . + count),
  data = pdata,
  fixed_effects = ~ year,
  se_type = "stata",
  clusters = recipient
)
level_2 <- lm_robust(
  update(spec_2, ~ . + govt_visits),
  data = pdata,
  fixed_effects = ~ year,
  se_type = "stata",
  clusters = recipient
)

## With lag of IVs as instruments
iv_spec_1 <- 
  lead_count ~ lead_aid + lead_debt + income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi + recipient + as.factor(year) |
  aid + debt + income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi + recipient + as.factor(year)
iv_spec_2 <- 
  lead_visits ~ lead_aid + lead_debt + income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi + recipient + as.factor(year) |
  aid + debt + income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi + recipient + as.factor(year)
iv_lag_1 <- iv_robust(
  iv_spec_1,
  data = pdata,
  se_type = "stata",
  clusters = recipient
)
iv_lag_2 <- iv_robust(
  iv_spec_2,
  data = pdata,
  se_type = "stata",
  clusters = recipient
)

# Lewbel Instruments
stage1.fitz <- lm(
  update(spec_1, lead_aid ~ - aid - debt + . + 
           recipient + as.factor(year)),
  data = pdata
)
stage1.fitv <- lm(
  update(spec_2, lead_debt ~ - aid - debt + . +
           recipient + as.factor(year)),
  data = pdata
)
epsz <- resid(stage1.fitz)
epsv <- resid(stage1.fitv)
z <- model.frame(
  update(spec_1, ~ lead_visits + . - as.factor(year) - recipient),
  data = pdata
)[, -1]
v <- model.frame(
  update(spec_1, ~ lead_visits + . - as.factor(year) - recipient),
  data = pdata
)[, -1]
zbr <- apply(z, 2, function(x) (x - mean(x)) * epsz^2)
vbr <- apply(v, 2, function(x) (x - mean(x)) * epsv^2)
colnames(zbr) <- paste0("z", 1:ncol(zbr))
colnames(vbr) <- paste0("v", 1:ncol(vbr))
zbr <- as_tibble(zbr)
vbr <- as_tibble(vbr)
ndata <- bind_cols(pdata, zbr, vbr)
lewbel_spec_1 <-
  lead_count ~ lead_aid + lead_debt + income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi + recipient + as.factor(year) |
  income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi + recipient + as.factor(year) +
    z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9 +
    v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9
lewbel_spec_2 <-
  lead_visits ~ lead_aid + lead_debt + income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi + recipient + as.factor(year) |
  income + pop + disaster + civilwar +
  dist + v2x_api + imports + exports + distance + fdi + recipient + as.factor(year) +
  z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9 +
  v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9
iv_lewbel_1 <- iv_robust(
  lewbel_spec_1,
  data = ndata,
  se_type = "stata",
  clusters = recipient
)
iv_lewbel_2 <- iv_robust(
  lewbel_spec_2,
  data = ndata,
  se_type = "stata",
  clusters = recipient
)

## Make regression table to summarize results:
coef_map <- list(
  "aid" = "ODA (lag)",
  "lead_aid" = "ODA (in-year)",
  "debt" = "OOF (lag)",
  "lead_debt" = "OOF (in-year)",
  "pmm_fh" = "Freedom House",
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
  "OLS (1)" = ols_1,
  "OLS (2)" = ols_1,
  # "Tobit (1)" = ml_tobit_1, 
  # "Tobit (2)" = ml_tobit_2,
  "Selection (1)" = glm_select_1, 
  "Level (1)" = level_1, 
  "Selection (2)" = glm_select_2,
  "Level (2)" = level_2,
  "IV Lag (1)" = iv_lag_1,
  "IV Lag (2)" = iv_lag_2,
  "IV Lewbel (1)" = iv_lewbel_1,
  "IV Lewbel (2)" = iv_lewbel_2
)
# if(data_set == "final_data_nonimputed.csv") {
#   model_list[3:4] <- NULL
# }
save(
  model_list,
  file = paste0(getwd(), "/03_analysis/model_list",i,".R")
)

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

# spec <- # specification without predictor of interest 
#   aid ~ pmm_fh + gdp + pop + unemp + disaster + civilwar +
#   dist + trade + atopally + as.factor(year)
# pdata <- mutate(pdata, region = str_replace(region, "&", "+"))
# 
# # models to estimate by region:
# # 1. OLS
# # 2. lag of dv as instrument for in-year dv
# # 3. internal instruments via Lewbel approach
# 
# ## OLS
# ols_region <- pdata %>% 
#   
#   # Split data by region
#   group_split(
#     region
#   ) %>% 
#   
#   # Estimate models for each region
#   map(
#     ~ lm_robust(
#         update(spec, ~ count + govt_visits + .),
#         data = .,
#         se_type = "stata",
#         clusters = recipient
#       )
#   ) %>%
#   
#   # Ensure the list of models is appropriately labeled
#   set_names(
#     sort(unique(pdata$region))
#   )
#   
# 
# ## With lag of DV as IV
# iv_spec <- 
#   aid ~ lead_count + lead_visits + pmm_fh + gdp + pop + unemp + disaster + civilwar +
#   dist + trade + atopally + as.factor(year) |
#   count + govt_visits +  pmm_fh + gdp + pop + unemp + disaster + civilwar +
#   dist + trade + atopally + as.factor(year)
# iv_lag_region <- pdata %>%
#   
#   # Split data by region
#   group_split(
#     region
#   ) %>%
#   
#   # Estimate models by region
#   map(
#     ~ iv_robust(
#         iv_spec,
#         data = .,
#         se_type = "stata",
#         clusters = recipient
#       )
#   ) %>%
#   
#   # Label the models
#   set_names(
#     sort(unique(pdata$region))
#   )
#   
# 
# # ## Lewbel Instruments
# # iv_lewbel_region <- pdata %>%
# #   
# #   # Split by region
# #   group_split(
# #     region
# #   ) %>%
# #   
# #   # Estiamte models by region
# #   map(
# #     ~ {
# #       stage1.fit <- lm(
# #         update(spec, lead_count ~ .),
# #         data = .
# #       )
# #       eps <- resid(stage1.fit)
# #       z <- model.frame(
# #         update(spec, ~ . - as.factor(year)),
# #         data = .
# #       )[, -1]
# #       zbr <- apply(z, 2, function(x) (x - mean(x)) * eps^2)
# #       colnames(zbr) <- paste0("z", 1:ncol(zbr))
# #       zbr <- as_tibble(zbr)
# #       ndata <- bind_cols(., zbr)
# #       lewbel_spec <- 
# #         aid ~ lead_count + govt_visits + pmm_fh + gdp + pop + unemp + disaster + civilwar +
# #         dist + trade + atopally + as.factor(year) |
# #         pmm_fh + gdp + pop + unemp + disaster + civilwar +
# #         dist + trade + atopally + as.factor(year) + 
# #         z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9
# #       iv_robust(
# #         lewbel_spec,
# #         data = ndata,
# #         se_type = "stata",
# #         clusters = recipient
# #       )
# #     }
# #   ) %>%
# #   
# #   # Label the models
# #   set_names(
# #     sort(unique(pdata$region))
# #   )
# 
# ## Make regression tables to summarize results:
# coef_map <- list(
#   "count" = "Coverage (lag)",
#   "lead_count" = "Coverage (in-year)",
#   "govt_visits" = "Diplomatic Visits (lag)",
#   "lead_visits" = "Diplomatic Visits (in-year)",
#   "pmm_fh" = "Polity",
#   "gdp" = "GDP",
#   "pop" = "Population",
#   "unemp" = "Unemployment",
#   "disaster" = "Disaster Deaths",
#   "civilwar" = "Civil War",
#   "trade" = "Trade",
#   "atopally" = "Alliance",
#   "dist" = "Distance"
# )
# 
# ols.region <- texreg(
#   ols_region,
#   custom.coef.map = coef_map,
#   include.ci = F,
#   stars = c(0.001, 0.01, 0.05, 0.1),
#   caption = "OLS Estimates by Region",
#   caption.above = T,
#   sideways = T
# )
# ivlag.region <- texreg(
#   iv_lag_region,
#   custom.coef.map = coef_map,
#   include.ci = F,
#   stars = c(0.001, 0.01, 0.05, 0.1),
#   caption = "IV Lag Estimates by Region",
#   caption.above = T,
#   sideways = T
# )
# # ivlew.region <- texreg(
# #   iv_lewbel_region,
# #   custom.coef.map = coef_map,
# #   include.ci = F,
# #   stars = c(0.001, 0.01, 0.05, 0.1),
# #   caption = "Lewbel IV Estimates by Region"
# # )
# 
# ## Save the regression table:
# save(
#   ols.region,
#   file = paste0(getwd(), "/03_analysis/ols_region_tab",i,".R")
# )
# save(
#   ivlag.region,
#   file = paste0(getwd(), "/03_analysis/ivlag_region_tab",i,".R")
# )
# # save(
# #   ivlew.region,
# #   file = paste0(getwd(), "/03_analysis/ivlew_region_tab",i,".R")
# # )