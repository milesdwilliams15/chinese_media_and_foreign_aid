## Code for cleaning and merging data

  - The file `01_clean_aid_data.R` contains script that cleans and collapses the Global Chinese Official Finance Dataset (version 1) produced by AidData to the aid recipient-year level of analysis.
  - The file `02_clean_diplomacy_data.R` contains script that cleans the Public Diplomacy dataset from AidData.
  - `03_clean_newsarticles_counts_frequencies.R` counts how many news articles are reported by Xinhua as well as their frequencies. Missing data existed (around 8% on average was not identified with any extracted_locations/country.) 
  - `04_clean_archer_data.R` cleans the article counts and frequencies data for merging with the final analysis dataset.
  - `05_clean_recipient_covariates.R` merges multiple datasets on recipient characteristics and bilateral characteristics with China. Two alternative sets of covariate data are produced. One that does not impute missing data, and a second that does. Multiple imputation of missing data was performed with the `missRanger` package.
  - `06_merge_datasets.R` performs the final merges (both with and without imputation) and creates two finalized datasets with imputed and the non-imputed data.
  
