####################################
# Clean recipient-level covariates
####################################


# setup -------------------------------------------------------------------

rm(list = ls())       # tidy the workspace
library(tidyverse)    # for grammar
library(democracyData)# a bunch of democracy datasets
library(vdem)         # to get the varieties of democracy dataset
library(countrycode)  # to standardize country codes

# democracy data ----------------------------------------------------------

# From democracy data, we'll use 
#   * Polity 2
#   * Freedom House
dem_data <-
  generate_democracy_scores_dataset(
    datasets = c(
      "polity_pmm", # polity
      "fh_pmm"      # political + civil liberties
    ),
    "wide"
  )

# To this, we'll add the five varieties of democracy indices
#   * Electoral Democracy
#   * Liberal Democracy
#   * Participatory Democracy
#   * Deliberative Democracy
#   * Egalitarian Democracy
vdem_data <- extract_vdem(
  section_number = 2
) %>%
  select(vdem_country_name, year, v2x_polyarchy:v2x_egaldem)

# limit to years needed (1999-2017)
dem_data <- dem_data %>%
  filter(year %in% 1999:2017)
vdem_data <- vdem_data %>%
  filter(year %in% 1999:2017)

# use iso3 code for later merging
dem_data <- dem_data %>%
  mutate(
    recipient_iso3 = countrycode::countrycode(
      cown, "cown", "iso3c"
    )
  ) %>%
  
  # Only keep vars of interest
  select(year, recipient_iso3, pmm_fh, pmm_polity)
vdem_data <- vdem_data %>%
  mutate(
    recipient_iso3 = countrycode(
      vdem_country_name, "country.name", "iso3c"
    )
  ) %>%
  select(-vdem_country_name)

# merge the democracy datasets
dem_data <- left_join(
  dem_data, vdem_data, by = c("year", "recipient_iso3")
)


# economic data -----------------------------------------------------------

# Variables to keep:
#   * population
#   * unemployed rate (ILO)
#   * real GDP in 2011 dollars

econ_data <- read_csv(
  paste0(getwd(), "/01_data/covariate_data/world_bank_data.csv")
)

# Pull out China's to add in later
china_gdp <- econ_data %>%
  filter(country == "China") %>%
  select(year, gdp)

econ_data <- econ_data %>%
  
  # ensure codes are iso3 codes
  mutate(
    recipient_iso3 = countrycode::countrycode(
      country, "country.name", "iso3c"
    )
  ) %>%
  
  # NAs are regions that we can drop
  filter(!is.na(recipient_iso3)) %>%
  
  # Drop human capital index
  select(-hc, -country) %>%
  
  # Merge in China GDP data
  left_join(
    china_gdp %>% rename(china_gdp = gdp),
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

trade_data <- read_csv(
  paste0(getwd(), "/01_data/covariate_data/trade_data.csv")
)

trade_data <- trade_data %>%
  
  # Only keep CHN in dcode
  filter(dcode=="CHN") %>%
  
  # Rename rcode 
  rename(recipient_iso3 = rcode) %>%
  
  # Drop dcode
  select(-dcode)


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


# combine into final covariate dataset ------------------------------------

cov_data <- # recipient + year to match 
  read_csv(
    paste0(getwd(), "/01_data/aid_data/clean_aid_data.csv")
  ) %>%
  select(year, recipient_iso3)

cov_list <- list(
  dem_data, econ_data, disaster_data,
  cw_data, dist_data, trade_data, ally_data
)

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
  
  # get rid of some additional junk variables
  select(-code) %>%
  
  # impute ally and civil war variables
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
