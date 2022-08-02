###############################
# Clean Chinese Diplomacy Data
###############################


# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(here)
library(countrycode)


# data --------------------------------------------------------------------

dip_data <- read_csv(
  here("01_data/PublicDiplomacy/ChinesePublicDiplomacy.csv")
)


# clean -------------------------------------------------------------------
fix_it <- function(x) str_replace(x,"N/A","0") %>%
  as.numeric()
# keep only necessary variables
dip_data <- dip_data %>%
  transmute(
    recipient_iso3 = countrycode(
      receiving_country, "country.name", "iso3c"
    ),
    year = year,
    govt_visits = fix_it(government_visits),
    mil_visits = fix_it(military_visits),
    total_visits = fix_it(total_elite_visits)
  )

# drop rows with invalid country code:
dip_data <- na.omit(dip_data)


# save --------------------------------------------------------------------

write_csv(
  dip_data,
  file = here("01_data/PublicDiplomacy/clean_diplomacy_data.csv")
)
