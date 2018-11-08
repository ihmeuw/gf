quietly{
//Author: Emily Linebarger, elineb@uw.edu 
//Purpose: To check the module mapping framework for the resource tracking database for errors. 
//Last updated: November 2018

cap log close 
set more off 
clear all 

set linesize 255

log using "C:\Users\elineb\Documents\gf\resource_tracking\prep\module_mapping_test_log.smcl", replace 
import excel using "J:\Project\Evaluation\GF\mapping\multi_country\intervention_categories\intervention_and_indicator_list.xlsx", sheet(module_mapping) firstrow clear 

************************************
*** MODULE MAPPING FRAMEWORK TESTS***
************************************

//Generate some variables that will display more nicely in a PDF
gen module_short = substr(module, 1, 10) 
replace module_short = module_short + "..." if (length(module) > 10)

gen intervention_short = substr(intervention, 1, 10)  
replace intervention_short = intervention_short + "..." if (length(intervention) > 10) 

//Check to make sure we're not growing or shrinking budget amounts due to coefficients. 
n di in red _newline "Test 1: Check that there are no duplicates in module, disease, and intervention with a coefficient of 1" 
duplicates tag module intervention disease, gen(dup) 
count if dup != 0 & coefficient == 1 
if `r(N)' != 0 {
	n di in red _newline "ERROR: `r(N)' Duplicates in key variables with a coefficient of 1. This will duplicate budget line items in database." 
	n tab coefficient 
	n list module intervention disease code coefficient if dup != 0 & coefficient == 1 
} 
else { 
	n di in red _newline "...test passed." 
} 


n di in red _newline "Test 2: Check that if there are coefficients < 1, they they sum to 1 by module, intervention, and disease" 
preserve 
collapse (sum) coefficient, by(module intervention disease module_short intervention_short) 
if `r(N)' != 0 {
	n di in red _newline "ERROR: `r(N)' Duplicates in key variables with a coefficient that sums to less than 1. This will shrink budget line items in database." 
	n tab coefficient 
	n list module intervention disease if coefficient != 1 
} 
else { 
	n di in red _newline "...test passed." 
} 
restore 

//Check that we're cataloging budget items correctly. 
gen na_all = 0 
foreach var in module intervention { 
	replace na_all = 1 if `var' == "na" | `var' == "all"
} 

n di in red _newline "Test 3: Check all cases where module or intervention is 'na' or 'all'"
count if na_all == 1 & length(code) > 2 
if `r(N)' != 0 {
	n di in red _newline "ERROR: `r(N)' Modules labeled 'na' or 'all' that are mapped to a specific variable."
	n di in red _newline "This may be okay if coefficient is less than 1 and coefficient is correctly divided among all sub-categories, but we also may want to reconsider this setup." 
	n list module intervention disease code coefficient if na_all == 1 & length(code) > 2
} 
else { 
	n di in red _newline "...test passed." 
} 

//Check that coefficients are generated correctly to sum to 1 across all sub-types of a intervention classification. 
preserve
ren code intervention_code 
duplicates drop intervention_code, force 
gen group_code = substr(intervention_code, 1, 2) 
gen count = 1 
collapse (sum) count, by (group_code) 
gen correct_coefficient = 1.0/count

tempfile group_codes
save `group_codes', replace 
restore 

preserve 
keep if coefficient < 1 & na_all == 1 
gen group_code = substr(code, 1, 2) 
merge m:1 group_code using `group_codes', keep(1 3) //We don't care about mappings that don't exist in our database. 
count if coefficient != correct_coefficient 
if `r(N)' != 0 {
	n di in red _newline "Test 4: ERROR: `r(N)' budget line items labeled 'all' or 'na' with a coefficient not split equally across all categories" 
	n list module intervention disease coefficient correct_coefficient code group_code if coefficient != correct_coefficient 
} 
else { 
	n di in red _newline "...test passed." 
}

restore 

************************************
***   CLEANED TOTAL DATA CHECKS 	***
************************************

//Check that there are no duplicates in the variable "fpm" or "pudr" within a given grant name and time period in the total cleaned data 
insheet using "J:\Project\Evaluation\GF\resource_tracking\multi_country\mapping\cleaned_total_data.csv", clear 

duplicates drop filename, force //Don't need such a granular level of detail for these checks. 

preserve 
duplicates tag grant_number grant_period data_source, gen(dup) 
count if dup != 0 
if `r(N)' != 0 {
	n di in red _newline "ERROR: `r(N)' Cases of multiple final budgets/pudrs within grant and time period."
	n list grant_number grant_period data_source filename if dup != 1 & (data_source == "fpm" | data_source == "fpm_final" | data_source == "pudr")
} 
else { 
	n di in red _newline "...test passed." 
} 
restore 

//Make sure that there are no time periods with less than 2 years. 
tab grant_period
count if length(grant_period) < 5 
if `r(N)' != 0 {
	n di in red _newline "ERROR: `r(N)' Cases where grant period doesn't match global fund format."
	n list grant_number grant_period filename if length(grant_period) < 5 
} 
else { 
	n di in red _newline "...test passed." 
} 

di in red _newline "Tests complete." 

//translate "C:\Users\elineb\Documents\gf\resource_tracking\prep\module_mapping_test_log.smcl" "C:\Users\elineb\Documents\gf\resource_tracking\prep\module_mapping_test_log.pdf", translator(smcl2pdf)

log close 

}
} 