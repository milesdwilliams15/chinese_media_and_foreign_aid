###################################
# Clean aid data prior to merging
###################################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(here)


# attach raw aid data -----------------------------------------------------

aid_data <- readxl::read_excel(
  path = here("01_data/aid_data/AidDatasGlobalChineseDevelopmentFinanceDataset_v2.0.xlsx"),
  sheet = 'Global_CDF2.0'
)
dim(aid_data) # check size



# clean -------------------------------------------------------------------

# The readme pdf that came with the data suggests only keeping observations
# listed as 'Recommended For Aggregates' == 'Yes'
aid_data <-
  aid_data %>%
  filter(`Recommended For Aggregates`=='Yes')
dim(aid_data) # new size
sectors <- paste(
  aid_data$`Sector Code`,
  aid_data$`Sector Name`
) %>% unique
# Now, clean up recipient names and aid values by year:
clean_data <- aid_data %>%
  
  # only keep year, recipient code (use iso3), and aid amount
  transmute(
    year = `Commitment Year`,
    recipient_iso3 = countrycode::countrycode(
      Recipient, 'country.name', 'iso3c'
    ),
    value =  `Amount (Constant USD2017)`,# amount in 2017 US dollars,
    flow_class = case_when(
      `Flow Class` == 'ODA-like' ~ 'aid',
      `Flow Class` == 'OOF-like' ~ 'debt',
      TRUE ~ 'drop'
    ),
    sector_code = `Sector Code`,
    sector_name = `Sector Name`
  ) %>%
  
  # aggregate the data to be at the recipient-year level:
  group_by(year, recipient_iso3) %>%
  summarize(
    aid = sum(value[flow_class=='aid'], na.rm=T),
    debt = sum(value[flow_class=='debt'], na.rm=T),
    aid_socgov = sum(value[flow_class=='aid' &
                             sector_code %in% 100:199], na.rm=T),
    aid_econ = sum(value[flow_class=='aid' &
                             sector_code %in% 200:399], na.rm=T),
    aid_human = sum(value[flow_class=='aid' &
                             sector_code %in% 700:799], na.rm=T)
  ) %>%
  ungroup %>%
  
  # get country names from iso3 codes:
  mutate(
    recipient = countrycode::countrycode(
      recipient_iso3, "iso3c", "country.name"
    ) # note that invalid codes for regions will be NA
  ) %>% 
  na.omit # drop NAs (NAs are regional values)

# Check that we have observations for each recipient in a year:
clean_data %>%
  count(recipient) %>%
  ggplot(aes(n)) + geom_histogram()
   # we do NOT

# make a schedule of recipients by year:
full_schedule <- expand.grid(
  year = 2000:2017,
  recipient_iso3 = unique(clean_data$recipient_iso3)
)
dim(full_schedule) # should have 2,556 obs.
dim(clean_data)    # we only have 1,937

full_data <- full_schedule %>%
  left_join(clean_data, by = c("year", "recipient_iso3")) %>%
  mutate(
    recipient = countrycode::countrycode(
      recipient_iso3, "iso3c", "country.name"
    ), # write over recipient name to fill in NAs
    across(
      aid:aid_human,
      ~ replace_na(.x, 0)
    )
  )
dim(full_data) # correct size

# place variables in more sensible order:
full_data <- full_data %>%
  select(year, recipient, recipient_iso3, everything())


# save to .csv file -------------------------------------------------------

write_csv(
  full_data,
  file = here("01_data/aid_data/clean_aid_data.csv")
)
