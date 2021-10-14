#####################
# Clean Archer Data
#####################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)


# load data ---------------------------------------------------------------

year <- as.list(2000:2014)
dts <-
  lapply(
    year,
    function(x) {
      read_csv(
        paste0(
          str_remove(getwd(), "/git_version"),
          "/Xinhua news articles_2000-2014/",
          "Xinhua_", x, ".csv"
        )
      )
    }
  ) # useful for now to leave as a list



# clean -------------------------------------------------------------------

dts %>%
  map(
    ~ .x %>% pull(country) %>%
      str_split("[|]") %>%
      str_replace(".*,", "")
  )
