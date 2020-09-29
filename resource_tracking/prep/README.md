# RT PREP CODE  
## *USER GUIDE*

Last updated by Francisco Rios Casas (frc2@uw.edu)
September 2020

Unless otherwise stated, all files should be run in numerical/alphabetical order. Files are called in order in the master file list (step 1).

### FOLDERS
- "\_common":
	- "global_variables.r": A set of global variables that are called when preparing the Global Fund files. Any variable that should be declared in global scope (i.e., it would be confusing to hard-code everywhere, and would be clearer to declare at the start of the script) should be declared here.
	- "load_master_list.r" This is a series of verification checks that reads in the current master file list (on Box) and makes sure it's ready to be used to prep data (no conflicting files, complete information entered, etc.)
	- "set_up_r.r" This file is sourced at the beginning of file prep to load libraries and set up filepaths.
	- "shared_functions.r" This file is a set of shared functions for resource tracking prep. There are in-line comments that explain what each function does.
	  - "gf_files_prep_functions", "ghe_sicoin_prep_functions", "odah_prep_functions": These folders have functions that interact with the raw data files in steps 2, 3, and 4 respectively. There are headers and in-line comments that explain what each file does.

### MASTER FILE (1)
- "1_master_file.r": This file is the master script from which all the other files run. There are a set of boolean logic switches in this file which can turn on different sections of the database to process.  

### GLOBAL FUND PREP  (2)
- "2a_gf_files_verify_mapping.r": This file prepares the modular framework map, and makes sure that it's not breaking any logic constraints.
- "2b_gf_files_prep_data.r" : This file extracts the raw data from Global Fund budgets and PUDRs and saves it.
- "2b_gos_prep_data.r" : This file extracts the GOS data and formats it to be similar to current budgets/PUDRs.
- "2c_gf_files_gos_map_data.r": Once budgets, PUDRs, and GOS data have been extracted, this file combines it, maps it to the modular framework,
	converts currencies, divides up re-mapped modules according to their modular framework split, divides data into usable datasets, and saves only the variables that are listed in the RT codebook.
- "2d_gf_split_datasets.r" This file is called in step 2c. It splits the aggregate resource tracking data into four files: final budgets,
		absorption, expenditures, and budget revisions.
- "2e_gf_aggregate_files.r" This script aggregates the four data files from step 2d across countries, to make a consortia level final budgets file,
		absorption file, expenditures file, and revisions file.
- "2f_gf_verify_outputs.r" Runs a series of unit tests (file total verification) on final budget, expenditure, and absorption files.
- "2g_gf_visualize_data.rmd" Creates a R-markdown with some general descriptive statistics about the data, as a reality check on the numbers.

### GOVERNMENT HEALTH EXPENDITURE (3)
- "3a_ghe_fgh_actuals_prep_data.r": This prepares "actuals" (government expenditure data extracted from National Health Accounts (NHAs), reported without modeling or adjustment) from the Financing Global Health study.
- "3b_ghe_fgh_estimates_prep_data.r" This prepares "estimates" (model estimates of government health expenditure) from the Financing Global Health study.
- "3c_ghe_sicoin_prep_data.r" Prepares SICOIN government health expenditure for Guatemala only. SICOIN is a national-level database of government health expenditure.
- "3d_ghe_who_prep_data.r" Prepares World Health Organization data on Government Health Expenditure. As of January 2020, this data is not being used.
- "3e_ghe_map_sicoin.r" This data maps SICOIN extracted data to Global Fund modules and interventions, and converts currencies from quetzales to USD.
- "3f_ghe_validate_data.r" As of January 2020, this file is not running. However, the goal was to get a unit test script running for the GHE data, similar to step 2f in the Global Fund files process.
- "3x_prep_sicoin_filelist.r" This is a recursive search script that created the SICOIN file list. It will never need to be run again, and is just left here for documentation.

### OTHER DEVELOPMENT ASSISTANCE FOR HEALTH (4)
- "4a_other_dah_prep_data.r" This script preps the raw "Other development assistance for health" file from the Financing Global Health team.

### AGGREGATE DATA SOURCES (5)
- "5_aggregate_all_data_sources.r" This script aggregates prepped files from between steps 2, 3, and 4 (for example, a joint GHE-ODAH file). However, as of January 2020, it's not updated.

### VALIDATE AND UPLOAD (6)
- "6_validate_and_upload.r" As of January 2020, this script is not uploaded. The vision was to have a script that automatically uploaded prepped data to Basecamp.

### PREP PUDR COST CATEGORIES (7)
- "7a_cost_categories_prep_data.r" This script extracts the raw cost category data from the PUDRs. It's similar in structure to "2b_gf_files_prep_data.r".
- "7b_cost_categories_clean_data.r" Cleans the raw extracted data from step 7a. Corrects cost category strings, adds metadata, converts currencies, and validates columns.

### PREP PUDR COMMITMENTS/OBLIGATIONS (8)
- "8a_commitments_obligations_prep.r" Extracts raw commitments/obligations from the PUDRs. It's similar in structure to "2b_gf_files_prep_data.r".
- "8b_commitments_obligations_clean.r" Cleans the raw extracted data from step 8a. Maps modules/interventions, converts currencies, and validates columns.

### PREP NFM3 FILES
- Keyword searches
  - "create_keyword_search_dataset.r"
  - "focus_topic_search.r"
  - "prep_final_focus_topic_mapping.r"
	- "merge_keyword_search_results_with_budget_data.r"
- Extracting NFM3 data
  - "run_prep_fr_budgets.r": extracts data from NFM3 and NFM2 funding request budgets; merges data together. Should be modified to also extract final approved NFM3 budgets.  
	- "nfm3_verify_mapping.r": prepares new modular framework in order to map budgetary data. This file is soured in "run_prep_fr_budgets.r"
	- "get_target_population_data.r": this script is not running. It was written to pull the target population tab from the NFM3 budgets.
- Grant Cycle analyses
  - "prep_qualitative_data.r"
  - "get_data_for_2s_analysis.r"

### OTHER FILES
- "match_box_to_j.r" As of January 2020, this script is not running. The vision was to have an automatic script that backs up the Box file system to the J drive to make sure they're always the same.
- "reporting_completeness_gf.rmd" This script checks the reporting completeness for Global Fund budgets and PUDRs, and outputs an automated R-markdown report.
- "unclassified_files.rmd" This script recursively searches the Box account, and flags files that are not classified in the Master File List. (As of January 2020, this may still be set up to search the J:drive, but this is an easy fix.)
