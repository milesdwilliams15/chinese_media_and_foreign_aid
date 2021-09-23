## Code for cleaning and merging data

  - The file `01_clean_aid_data.R` contains script that cleans and collapses the Global Chinese Official Finance Dataset (version 1) produced by AidData to the aid recipient-year level of analysis.
  - `02_clean_recipient_covariates.R` merges multiple datasets on recipient and characteristics and bilateral characteristics with China. Multiple imputation of missing data was performed with the `missRanger` package.