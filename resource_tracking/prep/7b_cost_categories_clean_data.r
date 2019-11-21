# ------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Clean raw cost category extraction
# DATE: Last updated November 2019
# ------------------------------------------------------------------

#----------------------------------------------------------------------------
# Correct some cost categories
#----------------------------------------------------------------------------
all_cost_categories[, cleaned_cost_category:=cost_category]

all_cost_categories[, cleaned_cost_category:=gsub("\\. ", "\\.0 ", cleaned_cost_category)]
all_cost_categories[cleaned_cost_category=="11.0 Programme Administration costs (PA)", cleaned_cost_category:="11.0 Indirect and Overhead Costs"]

# Validate these changes
all_cost_categories[, category_code:=tstrsplit(cleaned_cost_category, " ", keep=1)]

#Make sure that you've just got one code per cost category string. 
check1 = unique(all_cost_categories[, .(category_code, cleaned_cost_category)])
check1[, dup:=.N, by='category_code']
check1 = check1[dup>1]
if (nrow(check1)>0){
  stop("There are multiple cost categories for one code.")
}

#sort(unique(all_cost_categories$cleaned_cost_category))

#-----------------------------------------------------------
# Add in variable for current grant, and location variable
# ----------------------------------------------------------
all_cost_categories$current_grant = FALSE 
for (i in 1:length(current_cod_grants)){
  all_cost_categories[grant==current_cod_grants[i] & grant_period==current_cod_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_gtm_grants)){
  all_cost_categories[grant==current_gtm_grants[i] & grant_period==current_gtm_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_uga_grants)){
  all_cost_categories[grant==current_uga_grants[i] & grant_period==current_uga_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_sen_grants)){
  all_cost_categories[grant==current_sen_grants[i] & grant_period==current_sen_grant_period[i], 
              current_grant:=TRUE]
}

if (prep_files==TRUE){
  all_cost_categories$loc_name = country
  for (i in 1:nrow(code_lookup_tables)){
    all_cost_categories[loc_name==code_lookup_tables$iso_code[i], country:=code_lookup_tables$country[i]]
  }
}

#--------------------------------------------------------------------------------
#Add in a variable for the disease of the grant #Yuck - Emily try to rewrite this code. 
#--------------------------------------------------------------------------------
all_cost_categories[, disease_split:=strsplit(grant, "-")]
potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')

for (i in 1:nrow(all_cost_categories)){
  if (all_cost_categories$disease_split[[i]][2]%in%potential_diseases){
    all_cost_categories[i, grant_disease:=sapply(disease_split, "[", 2 )]
  } else if (all_cost_categories$disease_split[[i]][3]%in%potential_diseases){
    all_cost_categories[i, grant_disease:=sapply(disease_split, "[", 3 )]
  } else if (all_cost_categories$disease_split[[i]][4]%in%potential_diseases){
    all_cost_categories[i, grant_disease:=sapply(disease_split, "[", 4 )]
  }
}

all_cost_categories[, disease_split:=NULL]

unique(all_cost_categories[!grant_disease%in%potential_diseases, .(grant, grant_disease)]) #Visual check that these all make sense. 

all_cost_categories[grant_disease=='C', grant_disease:='hiv/tb']
all_cost_categories[grant_disease=='H', grant_disease:='hiv']
all_cost_categories[grant_disease=='T', grant_disease:='tb']
all_cost_categories[grant_disease=='S' | grant_disease=='R', grant_disease:='rssh']
all_cost_categories[grant_disease=='M', grant_disease:='malaria']
all_cost_categories[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 

stopifnot(unique(all_cost_categories$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))

# --------------------------------------------------------
# Convert currencies to USD 
# --------------------------------------------------------
stopifnot(all_cost_categories$file_currency%in%c("LOC","EUR","USD")) #After visual review, even local currencies (LOC) are actually Euros or USD. EL 11/19/2019. 

needs_conversion = all_cost_categories[file_currency!='USD']
if (nrow(needs_conversion)!=0){
  in_USD = all_cost_categories[file_currency=="USD"]
  converted_to_USD = convert_currency(needs_conversion, 'year', convertFrom="EUR", convertTo="USD", 
                                      finVars=c('budget', 'expenditure', 'disbursement', 'lfa_exp_adjustment'))
  all_cost_categories = rbind(in_USD, converted_to_USD, use.names=TRUE)
}

# --------------------------------------------------------
#Validate the columns in final data and the storage types  
# --------------------------------------------------------

#Note that I'm dropping 'module' and 'intervention' - which were corrected from the original text, but are just used for mapping. EKL 1/29/19
# Only keep the variable names that are in the codebook for consistency. This should constantly be reviewed. 
dropped_vars = names(all_cost_categories)[!names(all_cost_categories)%in%codebook$Variable]
if (length(dropped_vars)!=0){
  print("Some variables are being dropped because they aren't in the codebook - Review to make sure these shouldn't be in the final data.")
  print(dropped_vars)
}
all_cost_categories = all_cost_categories[, names(all_cost_categories)%in%codebook$Variable, with=FALSE]

#After variables are removed, collapse dataset to simplify
byVars <- colnames(all_cost_categories)
if ('disbursement'%in%names(all_cost_categories)){
  byVars = byVars[byVars != 'budget' & byVars != 'expenditure' & byVars !='disbursement' & byVars !='lfa_exp_adjustment']
  all_cost_categories = all_cost_categories[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment', 'disbursement'), by=byVars]
} else {
  byVars = byVars[byVars != 'budget' & byVars != 'expenditure' & byVars != 'lfa_exp_adjustment']
  all_cost_categories = all_cost_categories[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=c('budget', 'expenditure', 'lfa_exp_adjustment'), by=byVars]
}

# ----------------------------------------------
# Write the prepped data
# ---------------------------------------------
saveRDS(all_cost_categories, paste0(box, "tableau_data/all_cost_categories.rds"))
write.csv(all_cost_categories, paste0(box, "tableau_data/all_cost_categories.csv"), row.names=FALSE)
saveRDS(all_cost_categories, paste0(box, "tableau_data/archive/all_cost_categories_", Sys.Date(), ".rds"))

print("Step C: Map data completed. Outputs saved in prepped_data folder.")