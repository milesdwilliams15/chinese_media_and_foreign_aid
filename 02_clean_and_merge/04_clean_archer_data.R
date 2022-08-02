#####################
# Clean Archer Data
#####################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(here)

# load data ---------------------------------------------------------------

data <- read_csv(
  #"01_data/newsarticle/Xinhua_2001_2014_countrycounts.csv"
  here("01_data/newsarticle/Xinhua_2010_2017_aids_articles_relevant_refined.csv")
)

# clean -------------------------------------------------------------------

data <- data[,-c(1:3)] %>%
  mutate(
    across(frequncies_by_year:counts_by_year, ~replace_na(.x, 0))
  ) %>%
  rename(
    frequencies_by_year = frequncies_by_year,
    recipient_iso3 = recipient_iso
  )

# save --------------------------------------------------------------------

write_csv(
  data,
  here("01_data/newsarticle/xinhua_cleaned.csv")
)
