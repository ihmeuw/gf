# Global Fund Prospective Country Evaluation
## Prepped data for DRC National Malaria Program (PNLP) Data

Prepped March to June 2018 by Audrey Batzel abatzel@uw.edu

*Note: Imputation still in progress.*
  
#### This data comes from the DRC National Malaria Program data shared with the PCE by the DRC team.  The data covers the entire country for the years 2010-2017 at the health-zone level, for each month.

#### **Unique identifiers**
The unique identifiers for the observations in this data are health zone, date (month and year), indicator, and subpopulation.
To view the other variables included in the dataset, please see the codebook on Basecamp, [here]()

#### **Where to find the code and data**
The code can be found on github, [here](https://github.com/ihmeuw/gf/tree/develop/outcome_measurement/malaria/cod).
The prepped data can be found on Basecamp in DRC PCE / Docs & Files / Outcome Measurement data / National Malaria Program / Prepped Data, [here](https://3.basecamp.com/3769859/buckets/4025874/vaults/1131529751).

#### **What the code files do and how to run them** 

prep_COD_Malaria_data_function.R is the "master script" that runs the files to prep the data.
PNLP_files is a csv file of the names of the excel files that contain the data to be read into RStudio.
prep_data.R is the function that cleans that data.  It has a lot of specifics to correct errors in the data.
reshape_data.R
standardize_dps.R
remove_outliers.R

For multiple imputation of missing data:
prep_for_MI