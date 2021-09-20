###################################
# Clean aid data prior to merging
###################################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# attach raw aid data -----------------------------------------------------

aid_data <- readxl::read_excel(
  paste0(getwd(), "/01_data/aid_data/GlobalChineseOfficialFinanceDataset_v1.0.xlsx")
)
dim(aid_data) # check size



# clean -------------------------------------------------------------------

# The readme pdf that came with the data suggests only keeping observations
# listed as recommended_for_research==TRUE:
aid_data <-
  aid_data %>%
  filter(recommended_for_research==T)
dim(aid_data) # new size

# Now, clean up recipient names and aid values by year:
clean_data <- aid_data %>%
  
  # only keep year, recipient code (use iso3), and aid amount
  transmute(
    year = year,
    recipient_iso3 = recipient_iso3,
    aid = usd_defl_2014 # amount in 2014 US dollars
  ) %>%
  
  # there are multiple lines per recipient per year. aggregate
  # to the sum of aid to a recipient in a given year:
  group_by(year, recipient_iso3) %>%
  summarize(
    aid = sum(aid, na.rm=T)
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
  year = 2000:2014,
  recipient_iso3 = unique(clean_data$recipient_iso3)
)
dim(full_schedule) # should have 2,085 obs.
dim(clean_data)    # we only have 1,295

full_data <- full_schedule %>%
  left_join(clean_data, by = c("year", "recipient_iso3")) %>%
  mutate(
    recipient = countrycode::countrycode(
      recipient_iso3, "iso3c", "country.name"
    ), # write over recipient name to fill in NAs
    aid = replace_na(
      aid, 0
    )  # replace NAs with 0 
  )
dim(full_data) # correct size

# place variables in more sensible order:
full_data <- full_data %>%
  select(year, recipient, recipient_iso3, aid)


# save to .csv file -------------------------------------------------------

write_csv(
  full_data,
  file = paste0(
    getwd(), "/01_data/aid_data/clean_aid_data.csv"
  )
)
