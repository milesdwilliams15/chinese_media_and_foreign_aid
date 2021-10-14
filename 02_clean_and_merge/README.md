## Code for cleaning and merging data

  - The file `01_clean_aid_data.R` contains script that cleans and collapses the Global Chinese Official Finance Dataset (version 1) produced by AidData to the aid recipient-year level of analysis.
  - `02_clean_recipient_covariates.R` merges multiple datasets on recipient characteristics and bilateral characteristics with China. Multiple imputation of missing data was performed with the `missRanger` package.
  - `05_clean_newsarticles_counts_frequencies.R` counts how many news articels are reported by Xinhua as well as their frequencies. Missing data existed (around 8% on average was not identified with any extracted_locations/country.) 
