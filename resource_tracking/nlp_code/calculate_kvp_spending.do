//PURPOSE: Using lastest iteration of NLP Data, calculate how much was spent on KVP in 2015-2017 grants cycle versus 2018-2020 grants cycle.
//AUTHOR: Emily Linebarger 
//LAST UPDATED: November 2018  

insheet using "J:\Project\Evaluation\GF\resource_tracking\multi_country\mapping\nlp_data\model_outputs\iteration5\iteration5.csv", comma clear

// 1. Generate a binary variable KVP to flag which observations relate to key and vulnerable populations. Generate one version for original module/intervention, and one for reassigned module/intervention to compare. 
gen kvp_orig = 0 
replace kvp_orig = 1 if gf_module == "Comprehensive prevention programs for men who have sex with men" | gf_module == "Comprehensive prevention programs for people who inject drugs and their partners" | gf_module == "Comprehensive prevention programs for sex workers and their clients" | gf_module == "Comprehensive prevention programs for transgender people" | gf_module == "Comprehensive programs for people in prisons and other closed settings" | gf_module == "Prevention of mother-to-child transmission" | gf_module == "Prevention programs for adolescents and youth, in and out of school"

gen short_intervention_orig = substr(gf_intervention, 1, 15) 

replace kvp_orig = 1 if gf_intervention == "Key populations (MDR-TB) - Others" | gf_intervention == "Key populations (TB care and prevention) - Others" | gf_intervention == "Key populations (TB care and prevention) - Prisoners" | gf_intervention == "Key populations (TB/HIV) - Prisoners" | short_intervention_orig == "Key populations" | gf_intervention == "Reducing HIV-related gender discrimination, harmful gender norms and violence against women and girls in all their diversity" | gf_intervention == "Intermittent preventive treatment - In pregnancy"

gen kvp_model = 0 
replace kvp_model = 1 if predicted_module_translated == "Comprehensive prevention programs for men who have sex with men" | predicted_module_translated == "Comprehensive prevention programs for people who inject drugs and their partners" | predicted_module_translated == "Comprehensive prevention programs for sex workers and their clients" | predicted_module_translated == "Comprehensive prevention programs for transgender people" | predicted_module_translated == "Comprehensive programs for people in prisons and other closed settings" | predicted_module_translated == "Prevention of mother-to-child transmission" | predicted_module_translated == "Prevention programs for adolescents and youth, in and out of school"

//Doing this to deal with cases that have special characters 
gen short_intervention_model = substr(predicted_intervention_translate, 1, 15) 

replace kvp_model = 1 if predicted_intervention_translate == "Key populations (MDR-TB) - Others" | predicted_intervention_translate == "Key populations (TB care and prevention) - Others" | predicted_intervention_translate == "Key populations (TB care and prevention) - Prisoners" | predicted_intervention_translate == "Key populations (TB/HIV) - Prisoners" | short_intervention_model == "Key populations" | predicted_intervention_translate == "Reducing HIV-related gender discrimination, harmful gender norms and violence against women and girls in all their diversity" | predicted_intervention_translate == "Intermittent preventive treatment - In pregnancy"


// 2. Make sure that the "kvp_model" variable is correct for analysis. 
//Two cases that need to be labeled as KVP that were changed by the model. 
replace kvp_model = 1 if module == "mdrtb" & intervention == "keyaffectedpopulations" 
replace kvp_model = 1 if module == "tbcareandprevention" & intervention == "keypopulationstbcareandpreventionothers"

count if kvp_orig != kvp_model 
br gf_module predicted_module_translated gf_intervention predicted_intervention_translate module intervention kvp_orig kvp_model if kvp_orig != kvp_model //The KVP variable for this observation is correct. 

//Make sure you haven't missed any module/intervention pairs that should be included 
br predicted_module_translated predicted_intervention_translate if kvp_model == 0 

// 3. Keep only observations that correspond to key and vulnerable populations, and create a tempfile to merge on to resource tracking data. Also, flag which of these observations were corrected by the model. 
outsheet gf_module gf_intervention predicted_module_translate predicted_intervention_translate module intervention sda_activity_translated if kvp_model == 0 using "H:\Temp Projects\KVP Analysis with NLP Data\To Verify.csv", comma replace 
keep if kvp_model == 1 
gen model_correction = 0 
replace model_correction = 1 if predicted_module_translate != gf_module | predicted_intervention_translate != gf_intervention 
tab model_correction
br gf_module gf_intervention predicted_module_translate predicted_intervention_translate module intervention sda_activity_translated model_correction if model_correction == 1 //These cases will all still fall under KVP- so right now, this is fine if KVP is always a binary variable, but if you want to do more granular analysis we'll have to address these cases (i.e. calculating spending given to programs for men who have sex with men vs. trangender people). 
//outsheet gf_module gf_intervention predicted_module_translate predicted_intervention_translate module intervention sda_activity_translated model_correction using "H:\Temp Projects\KVP Analysis with NLP Data\To Verify.csv", comma replace 

//Create an output table of all KVP cases to review 
outsheet predicted_module_translate predicted_intervention_translate model intervention using "H:\Temp Projects\KVP Analysis with NLP Data\KVP_modules_interventions.csv", comma replace

keep module intervention 
duplicates drop module intervention, force 
tempfile kvp_framework 
save `kvp_framework', replace 

//Merge on the cleaned resource tracking data and tally up the budget totals for 2015-2017 grants and 2018-2020 grants. 
insheet using "J:\Project\Evaluation\GF\resource_tracking\multi_country\mapping\cleaned_total_data.csv", clear 

merge m:1 module intervention using `kvp_framework', keep(3)

tab grant_period
tab grant_number 

outsheet using "H:\Temp Projects\KVP Analysis with NLP Data\KVP_resource_tracking.csv", comma replace 

/*
br gf_module gf_intervention sda_activity_translated if gf_intervention == "Prong 1: Primary prevention of HIV infection among women of childbearing age" | gf_intervention == "Other intervention(s) to reduce human rights- related barriers to HIV services" | gf_intervention ==  "Reducing HIV-related gender discrimination, harmful gender norms and violence against women and girls in all their diversity"
*/

