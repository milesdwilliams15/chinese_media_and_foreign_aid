###############################
# Clean Chinese Diplomacy Data
###############################


# set up ------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(countrycode)


# data --------------------------------------------------------------------

dip_data <- read_csv(
  paste0(
    getwd(), "/01_data/PublicDiplomacy/ChinesePublicDiplomacy.csv"
  )
)


# clean -------------------------------------------------------------------

# keep only necessary variables
dip_data <- dip_data %>%
  transmute(
    recipient_iso3 = countrycode(
      receiving_country, "country.name", "iso3c"
    ),
    year = year,
    govt_visits = government_visits,
    mil_visits = str_replace(military_visits,"N/A","0") %>%
      as.numeric(),
    total_visits = total_elite_visits
  )

# drop rows with invalid country code:
dip_data <- na.omit(dip_data)


# save --------------------------------------------------------------------

write_csv(
  dip_data,
  file = paste0(
    getwd(),
    "/01_data/PublicDiplomacy/clean_diplomacy_data.csv"
  )
)
