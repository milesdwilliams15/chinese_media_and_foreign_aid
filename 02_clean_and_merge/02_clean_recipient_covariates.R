####################################
# Clean recipient-level covariates
####################################


# setup -------------------------------------------------------------------

rm(list = ls())       # tidy the workspace
library(tidyverse)    # for grammar
library(democracyData)# a bunch of democracy datasets
library(pwt9)


# democracy data ----------------------------------------------------------

# For democracy data, we'll use 
#   * Polity 2
#   * Freedom House
dem_data <-
  generate_democracy_scores_dataset(
    datasets = c(
      "polity_pmm", # polity
      "fh_pmm"      # political + civil liberities
    ),
    "wide"
  )

# limit to years needed (2000-2014)
dem_data <- dem_data %>%
  filter(year %in% 2000:2014)

# use iso3 code for later merging
dem_data <- dem_data %>%
  mutate(
    recipient_iso3 = countrycode::countrycode(
      cown, "cown", "iso3c"
    )
  )


# economic data -----------------------------------------------------------

# Variables to keep:
#   * population
#   * employed
#   * human capital index (years of school and returns to education)
#   * real GDP in 2011 dollars

econ_data <- pwt9.0 %>%
  transmute(
    country = country,
    isocode = isocode,
    year = year,
    pop = pop * 1000000,
    emp = emp * 1000000,
    hc = hc,
    gdp = rgdpna
  )

# Keep values for relevant years
econ_data <- econ_data %>%
  filter(year %in% 2000:2014)

# Add iso3 code for later merging
econ_data <- econ_data %>%
  mutate(
    recipient_iso3 =
      countrycode::countrycode(
        country, "country.name", "iso3c"
      )
  )
