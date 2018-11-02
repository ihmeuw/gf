//Author: Emily Linebarger, elineb@uw.edu 
//Purpose: To randomly select 10% of each country's grant list to unit test (files selected on November 2, 2018). 
//			Will also select with certainty one of each type of function (pudr, fpm, etc.) to verify that each function is working for each country. 
//Last updated: November 2018

set more off 
clear all 
global save_loc = "C:\Users\elineb\Desktop"
cap mkdir "$save_loc/unit_tests"

foreach iso in "gtm" "uga" "cod" {
	global iso `iso'
	di in red _newline "$iso" 
	
	global dir = "J:/Project/Evaluation/GF/resource_tracking/${iso}/gf"
	insheet using "${dir}/${iso}_budget_filelist.csv", clear 
	
	//First, find total count of tests you need to run. 
	count 
	local num_tests = round(`r(N)' * .10) //Test 10% of each country's filelist, plus one of each type. 
	
	//Next, select with certainty one of each function type (pudr, fpm, etc.) 
	preserve 
	set seed 20 //Make sure that your random results are replicable 
	gen rand=runiform()
	sort rand
	duplicates drop data_source, force
	
	count 
	drop rand 
	tempfile unique_types 
	save `unique_types', replace 
	restore 
	
	//Select additional tests until you've selected 25% of database
	set seed 20 //Make sure that your random results are replicable 
	gen rand=runiform()
	sort rand
	keep in 1/`num_tests' 
	append using `unique_types' 
	count 
	drop rand 
	
	outsheet using "${save_loc}/${iso}_tests.csv", comma replace 
	
} 
}