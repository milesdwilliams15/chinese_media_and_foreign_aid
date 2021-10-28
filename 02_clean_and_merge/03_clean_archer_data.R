#####################
# Clean Archer Data
#####################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# load data ---------------------------------------------------------------

counts <- read_csv(
  "01_data/newsarticle/Xinhua_2001_2014_countrycounts.csv"
)
frequs <- read_csv(
  "01_data/newsarticle/Xinhua_2001_2014_countryfrequencies.csv"
)


# clean -------------------------------------------------------------------

counts <- counts %>%
  select(-X1) %>%
  gather(
    year, "count", -country_mentioned
  ) %>%
  mutate(
    count = replace_na(count, 0),
    country = str_remove(country_mentioned, ".*, "),
    recipient_iso3 = countrycode::countrycode(
      country, "country.name", "iso3c"
    )
  ) %>%
  select(recipient_iso3, year, count) %>%
  na.omit

frequs <- frequs %>%
  select(-X1) %>%
  gather(
    year, "freq", -country_mentioned
  ) %>%
  mutate(
    count = replace_na(freq, 0),
    country = str_remove(country_mentioned, ".*, "),
    recipient_iso3 = countrycode::countrycode(
      country, "country.name", "iso3c"
    )
  ) %>%
  select(recipient_iso3, year, freq) %>%
  na.omit


# merge country counts and freq -------------------------------------------

archer <- full_join(
  counts, frequs, by = c("recipient_iso3", "year")
)

# deal with duplicates and additional missing values
archer <- archer %>%
  mutate(
    freq = replace_na(freq, 0)
  ) %>%
  group_by(recipient_iso3, year) %>%
  summarize(
    count = max(count),
    freq  = max(freq)
  )


# save --------------------------------------------------------------------

write_csv(
  archer,
  "01_data/newsarticle/xinhua_cleaned.csv"
)
