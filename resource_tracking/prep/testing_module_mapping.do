quietly{
//Author: Emily Linebarger, elineb@uw.edu 
//Purpose: To check the module mapping framework for the resource tracking database for errors. 
//Last updated: November 2018

cap log close 
set more off 
clear all 

log using "C:\Users\elineb\Documents\gf\resource_tracking\prep\module_mapping_test_log.smcl", replace 
import excel using "J:\Project\Evaluation\GF\mapping\multi_country\intervention_categories\intervention_and_indicator_list.xlsx", sheet(module_mapping) firstrow clear 

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
collapse (sum) coefficient, by(module intervention disease) 
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



log close 
}
} 