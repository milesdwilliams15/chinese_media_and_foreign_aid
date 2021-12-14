##################
# Merge datasets
##################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(countrycode)


# load data ---------------------------------------------------------------

aid_data <- 
  read_csv(
    paste0(getwd(), "/01_data/aid_data/clean_aid_data.csv")
  )
xin_data <-
  read_csv(
    paste0(getwd(), "/01_data/newsarticle/xinhua_cleaned.csv")
  )
cov_data <-
  read_csv(
    paste0(getwd(), "/01_data/covariate_data/clean_covariate_data_nonimputed.csv")
  )
imp_cov_data <-
  read_csv(
    paste0(getwd(), "/01_data/covariate_data/clean_covariate_data_imputed.csv")
  )

# merge -------------------------------------------------------------------

# merge the datasets by year and recipient code:

final_data <-
  left_join(
    aid_data, xin_data,
    by = c("recipient_iso3", "year")
  ) %>%
  left_join(
    cov_data, 
    by = c("recipient_iso3", "year")
  )
imp_final_data <-
  left_join(
    aid_data, xin_data,
    by = c("recipient_iso3", "year")
  ) %>%
  left_join(
    cov_data, 
    by = c("recipient_iso3", "year")
  )

# check that the appropriate data size has been
# maintained:

nrow(aid_data) - nrow(final_data) # no difference!

# set missing counts and freqs to zero

final_data <- final_data %>%
  mutate_at(
    c("count", "freq"),
    ~ replace_na(.x, 0)
  )
imp_final_data <- imp_final_data %>%
  mutate_at(
    c("count", "freq"),
    ~ replace_na(.x, 0)
  )

# save the final dataset --------------------------------------------------

write_csv(
  final_data,
  file = 
    paste0(getwd(), "/01_data/final_data/final_data_nonimputed.csv")
)
write_csv(
  imp_final_data,
  file = 
    paste0(getwd(), "/01_data/final_data/final_data_imputed.csv")
)
