# ------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Clean commitments/obligations extraction
# DATE: Last updated November 2019
# ------------------------------------------------------------------

#-------------------------------------------------------
# Map modules/interventions
#-------------------------------------------------------
#Remove whitespaces, punctuation, and unwanted characters from module and intervention. 
all_commitments = strip_chars(all_commitments)

#Correct common acronyms in the resource database and the module map. 
all_commitments[, module:=replace_acronyms(module)]
all_commitments[, intervention:=replace_acronyms(intervention)]

module_map[, module:=replace_acronyms(module)]
module_map[, intervention:=replace_acronyms(intervention)]

# Check for unmapped modules/interventions before mapping
gf_concat <- paste0(module_map$module, module_map$intervention)
rt_concat <- paste0(all_commitments$module, all_commitments$intervention)
unmapped_mods <- all_commitments[!rt_concat%in%gf_concat]

if(nrow(unmapped_mods)>0){
  print(unique(unmapped_mods[, c("module", "intervention"), with= FALSE]))
  print(unique(unmapped_mods$fileName)) #For documentation in the comments above. 
  stop("You have unmapped original modules/interventions!")
}


# Merge with module map on module, intervention, and disease to pull in code
mergeVars = c('disease', 'module', 'intervention')
#module_map = unique(module_map)
module_map = module_map[!is.na(code)]

mapped_data <- merge(all_commitments, module_map, by=mergeVars, all.x = TRUE, allow.cartesian = TRUE)
dropped_mods <- mapped_data[is.na(mapped_data$gf_module), ]

if(nrow(dropped_mods) >0){
  # Check if anything is dropped in the merge -> if you get an error. Check the mapping spreadsheet
  print(unique(dropped_mods[, c("module", "intervention", "disease"), with= FALSE]))
  stop("Modules/interventions were dropped!")
}
#-----------------------------------------------------------
# Add in variable for current grant, and location variable
# ----------------------------------------------------------
all_commitments$current_grant = FALSE 
for (i in 1:length(current_cod_grants)){
  all_commitments[grant==current_cod_grants[i] & grant_period==current_cod_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_gtm_grants)){
  all_commitments[grant==current_gtm_grants[i] & grant_period==current_gtm_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_uga_grants)){
  all_commitments[grant==current_uga_grants[i] & grant_period==current_uga_grant_period[i], 
              current_grant:=TRUE]
}
for (i in 1:length(current_sen_grants)){
  all_commitments[grant==current_sen_grants[i] & grant_period==current_sen_grant_period[i], 
              current_grant:=TRUE]
}

#--------------------------------------------------------------------------------
#Add in a variable for the disease of the grant 
#--------------------------------------------------------------------------------
all_commitments[, disease_split:=strsplit(grant, "-")]
potential_diseases = c('C', 'H', 'T', 'M', 'S', 'R', 'Z')

for (i in 1:nrow(all_commitments)){
  if (all_commitments$disease_split[[i]][2]%in%potential_diseases){
    all_commitments[i, grant_disease:=sapply(disease_split, "[", 2 )]
  } else if (all_commitments$disease_split[[i]][3]%in%potential_diseases){
    all_commitments[i, grant_disease:=sapply(disease_split, "[", 3 )]
  } else if (all_commitments$disease_split[[i]][4]%in%potential_diseases){
    all_commitments[i, grant_disease:=sapply(disease_split, "[", 4 )]
  }
}

all_commitments[, disease_split:=NULL]

unique(all_commitments[!grant_disease%in%potential_diseases, .(grant, grant_disease)]) #Visual check that these all make sense. 

all_commitments[grant_disease=='C', grant_disease:='hiv/tb']
all_commitments[grant_disease=='H', grant_disease:='hiv']
all_commitments[grant_disease=='T', grant_disease:='tb']
all_commitments[grant_disease=='S' | grant_disease=='R', grant_disease:='rssh']
all_commitments[grant_disease=='M', grant_disease:='malaria']
all_commitments[grant_disease=='Z' & grant=='SEN-Z-MOH', grant_disease:='tb'] #oNLY ONE CASE OF THIS. 

stopifnot(unique(all_commitments$grant_disease)%in%c('hiv', 'tb', 'hiv/tb', 'rssh', 'malaria'))

# --------------------------------------------------------
# Convert currencies to USD 
# --------------------------------------------------------
# First, convert all financial variables to numeric
fin_vars = c('budget', 'lfa_adjustment', 'lfa_budget', 'ct_adjustment', 'gf_budget')
for (v in fin_vars) all_commitments[, (v):=as.numeric(get(v))]

# Now, convert currencies 
stopifnot(all_commitments$file_currency%in%c("LOC","EUR","USD")) #After visual review, even local currencies (LOC) are actually Euros or USD. EL 11/19/2019. 

needs_conversion = all_commitments[file_currency!='USD']
if (nrow(needs_conversion)!=0){
  
  # Need to extract start date and divide files up by quarter EL 12/27/2019 
  # But until that happens, just assume the year is 2018 for OECD conversion rate. 
  stopifnot(unique(needs_conversion$grant_period)=="2018-2020")
  stopifnot(unique(needs_conversion$pudr_semester_financial)%in%c('2-A', '1-AB', '1-A'))
  needs_conversion$year <- 2018
  
  in_USD = all_commitments[file_currency=="USD"]
  converted_to_USD = convert_currency(needs_conversion, 'year', convertFrom="EUR", convertTo="USD", 
                                      finVars=c('budget', 'lfa_adjustment', 'lfa_budget', 'ct_adjustment', 'gf_budget'))
  converted_to_USD$year <- NULL
  all_commitments = rbind(in_USD, converted_to_USD, use.names=TRUE)
}

# ----------------------------------------------
# Write the prepped data
# ---------------------------------------------
saveRDS(all_commitments, paste0(box, "tableau_data/all_commitments.rds"))
write.csv(all_commitments, paste0(box, "tableau_data/all_commitments.csv"), row.names=FALSE)
saveRDS(all_commitments, paste0(box, "tableau_data/archive/all_commitments_", Sys.Date(), ".rds"))

print("Step B: Clean data completed.")