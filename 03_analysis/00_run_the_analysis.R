########################################
# Run the script for the main analysis
########################################


# setup -------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(censReg)
library(AER)
library(texreg)
library(estimatr)


# run analysis ------------------------------------------------------------

data_sets <- c(
  "final_data_imputed.csv",
  "final_data_nonimputed.csv"
)
for(i in 1:2) {
  cat("\nRUNNING ANALYSIS WITH:", data_sets[i], "......\n")
  data_set <- data_sets[i]
  source(
    paste0(getwd(), "/03_analysis/01_primary_analysis.R")
  )
  if(i == 2) cat("\n.....DONE!")
}
