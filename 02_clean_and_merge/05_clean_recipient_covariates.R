####################################
# Clean recipient-level covariates
####################################


# setup -------------------------------------------------------------------

rm(list = ls())       # tidy the workspace
library(tidyverse)    # for grammar
# remotes::install_github("xmarquez/democracyData", force = TRUE)
library(democracyData)# a bunch of democracy datasets
library(countrycode)  # to standardize country codes

# democracy data ----------------------------------------------------------

# From democracy data, we'll use 
#   * Polity 2
#   * V-Dem
dem_data <- generate_democracy_scores_dataset(
  datasets = c('polity_annual', 'fh_full', 'vdem'),
  output_format = 'wide',
  verbose = F
)
dem_data <- dem_data %>%
  transmute(
    recipient_iso3 = countrycode(
      cown, 'cown', 'iso3c'
    ),
    year,
    polity2,
    v2x_api
  )


# economic data -----------------------------------------------------------

# Variables to keep:
#   * population
#   * real GDP in 2011 dollars

econ_data <- read_csv(
  paste0(getwd(), "/01_data/covariate_data/world_bank_gdp_pop_1999_2017.csv")
)
econ_data <- econ_data %>%
  drop_na() %>%
  transmute(
    year = Time,
    recipient_iso3 = countrycode(
      `Country Name`, 'country.name', 'iso3c'
    ),
    gdp = `GDP, PPP (constant 2017 international $) [NY.GDP.MKTP.PP.KD]` %>%
      ifelse(.=='..', NA, .),
    pop = `Population, total [SP.POP.TOTL]` %>%
      ifelse(.=='..', NA, .)
  ) %>%
  mutate(
    across(c(year, gdp:pop), as.numeric)
  )
  

# Pull out China's to add in later
china_gdp <- econ_data %>%
  filter(recipient_iso3 == "CHN") %>%
  select(year, gdp, pop)

econ_data <- econ_data %>%
  
  # NAs are regions that we can drop
  filter(!is.na(recipient_iso3)) %>%
  
  # Merge in China GDP + Pop data
  left_join(
    china_gdp %>% rename(china_gdp = gdp, china_pop = pop),
    by = "year"
  )



# disaster data -----------------------------------------------------------

# Data on deaths from natural disasters:

disaster_data <- 
  read_csv(
    paste0(getwd(),"/01_data/covariate_data/deaths-natural-disasters-ihme.csv")
  )

# We already have iso3 code, so just rename:
disaster_data <- disaster_data %>%
  rename(recipient_iso3 = rcode) %>%
  select(-recipient)


# civil war data ----------------------------------------------------------

cw_data <- read_csv(
  paste0(getwd(),"/01_data/covariate_data/prio_civilwars.csv")
)

# Some cleaning:
cw_data <- cw_data %>%
  mutate(
    
    # replace NAs with 0
    civilwar = replace_na(civilwar, 0),
    
    # get iso3 country codes
    recipient_iso3 = 
      countrycode::countrycode(
        location, "country.name", "iso3c"
      )
  ) %>%
  
  # get rid of conflict IDs (make some trouble for merging)
  select(-conflict_id) %>%
  
  # Drop NAs 
  na.omit %>%
  
  # Reduce to one observation per year per country
  group_by(year, recipient_iso3) %>%
  summarize(
    civilwar = max(civilwar)
  )


# distance data -----------------------------------------------------------

dist_data <- 
  haven::read_dta(
    paste0(getwd(),"/01_data/covariate_data/dist_cepii.dta")
  )

dist_data <- dist_data %>%
  
  # Only keep bilateral distances wrt China ("CHN")
  filter(iso_o == "CHN") %>%
  
  # Rename iso_d 
  rename(recipient_iso3 = iso_d) %>%
  
  # only keep distance measures
  select(recipient_iso3, dist, distw, distwces)



# trade data --------------------------------------------------------------

# Code from this section comes from:
# https://www.r-bloggers.com/2019/05/open-trade-statistics/

# install.packages('tradestatistics')

# Use open-source trade statistics for 2002-2017
library(tradestatistics)
partners <- as_tibble(ots_countries)$country_iso
partners <- partners[partners!='chn']
trade_data <- ots_create_tidy_data(
  years = 2002:2017,
  reporters = "chn",
  partners = partners,
  table = 'yrp'
) %>% as_tibble()
trade_data <- trade_data %>%
  transmute(
    year,
    recipient_iso3 = toupper(partner_iso),
    imports = trade_value_usd_imp,
    exports = trade_value_usd_exp
  )
trade_data <- trade_data %>%
  distinct()


# Alliance data -----------------------------------------------------------

ally_data <- read_csv(
  paste0(getwd(), "/01_data/covariate_data/atop.csv")
)

ally_data <- ally_data %>%
  
  # get iso codes
  mutate(
    dcode = countrycode::countrycode(
      mem1, origin = 'cown',
      destination = 'iso3c'
    ),
    recipient_iso3 = countrycode::countrycode(
      mem2, origin = 'cown',
      destination = 'iso3c'
    )
  ) %>%
  
  # Only keep cases where dcode == CHN
  filter(dcode == "CHN") %>%
  
  # keep the alliance variable + id vars
  select(
    year, recipient_iso3, atopally
  )


# un voting data ----------------------------------------------------------

un_vote <- read_csv(
  here('01_data/covariate_data/UNvotes_filtered.csv')
)
un_vote <- un_vote %>%
  transmute(
    year,
    recipient_iso3 = countrycode(
      ccode2, 'cown', 'iso3c'
    ),
    distance = IdealPointDistance,
    agree
  )

# FDI ---------------------------------------------------------------------

fdi_data <- read_csv(
  here('01_data/covariate_data/WB_fdi_filtered.csv')
)
fdi_data <- fdi_data %>%
  transmute(
    year,
    recipient_iso3 = countrycode(
      .$`﻿Country Name`country.name', 'iso3c'
    ),
    fdi = net
  )

# combine into final covariate dataset ------------------------------------

cov_data <- # recipient + year to match 
  read_csv(
    paste0(getwd(), "/01_data/aid_data/clean_aid_data.csv")
  ) %>%
  select(year, recipient_iso3)

c %>%
  filter(year >= 2002) # no valid trade data before this dateov_list <- list(
  dem_data, econ_data, disaster_data,
  cw_data, dist_data, trade_data, ally_data
)
,
  un_vote, fdi_data
start_n <- nrow(cov_data)
for(i in 1:length(cov_list)) {
  if(i != 5) {
    by_vars <- c("recipient_iso3", "year")
  } else {
    by_vars <- "recipient_iso3"
  }
  cov_data <- cov_data %>%
    left_join(
      cov_list[[i]],
      by = by_vars
    )
  if(start_n != nrow(cov_data)) 
    stop("Something went wrong in merge ", i, "!")
} 



# Clean up missing values -------------------------------------------------

# how many obs have missing data?
start_n - nrow(na.omit(cov_data)) # okay that's a lot

# Some vars with NAs we can impute missing as '0'
cov_data <- cov_data %>%
  
  # getute ally and civil war variables
  mutate_at(
    c("atopally", "civilwar"),
    ~ replace_na(.x, 0)
  )

start_n - nrow(na.omit(cov_data)) # that's better, but not great

# Use random forests to impute remaining missing data
library(missRanger)
imp_cov_data <- cov_data %>%
  missRanger(pmm.k = 3) # combine imputations with predictive mean matching

# for citation of missRanger, see: https://academic.oup.com/bioinformatics/article/28/1/112/219101

# Fix imputed distance vals to be mean of predictions for a given recipient
imp_cov_data <- imp_cov_data %>%
  group_by(recipient_iso3) %>%
  mutate_at(
    c("dist", "distw", "distwces"),
    mean
  )


# Add regional dummies ---------------------------------------------------

cov_data <- 
  cov_data %>%
  mutate(
    region = countrycode(
      recipient_iso3,
      origin = "iso3c",
      destination = "region"
    )
  )
imp_cov_data <- 
  imp_cov_data %>%
  mutate(
    region = countrycode(
      recipient_iso3,
      origin = "iso3c",
      destination = "region"
    )
  )


# save the results --------------------------------------------------------

write_csv(
  imp_cov_data,
  paste0(getwd(), "/01_data/covariate_data/clean_covariate_data_imputed.csv")
)
write_csv(
  cov_data,
  paste0(getwd(), "/01_data/covariate_data/clean_covariate_data_nonimputed.csv")
)
