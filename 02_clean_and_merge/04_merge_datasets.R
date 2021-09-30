##################
# Merge datasets
##################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# load data ---------------------------------------------------------------

aid_data <- 
  read_csv(
    paste0(getwd(), "/01_data/aid_data/clean_aid_data.csv")
  )
cov_data <-
  read_csv(
    paste0(getwd(), "/01_data/covariate_data/clean_covariate_data.csv")
  )


# merge -------------------------------------------------------------------

# merge the datasets by year and recipient code:

final_data <-
  left_join(
    aid_data, cov_data,
    by = c("recipient_iso3", "year")
  )

# check that the appropriate data size has been
# maintained:

nrow(aid_data) - nrow(final_data) # no difference!


# save the final dataset --------------------------------------------------

write_csv(
  final_data,
  file = 
    paste0(getwd(), "/01_data/final_data/final_data.csv")
)
