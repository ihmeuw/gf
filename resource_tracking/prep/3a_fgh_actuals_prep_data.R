# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chen
# PURPOSE: Read in FGH data and clean into a presentable format.  
# DATE: Last updated March 2019. 
# ----------------------------------------------

# ----------------------------------------------------------------------
# To do list for this code: 
# Change the subsetting of columns for other DAH actuals to be based off of the codebook @ the end
# Convert currencies for GHE malaria actuals. 

# ---------------------------------------------------------------------

#Archiving here how the most recent raw FGH data was saved - new FGH data through 2018 was received on March 13, 2019. EKL
# fgh_data = read.dta13(paste0(j, "/Project/IRH/DAH/RESEARCH/INTEGRATED DATABASES/DATA/FGH_2018/FGH_EZ_2018.dta"))
# setDT(fgh_data)
# fgh_data = fgh_data[tolower(iso3_rc)%in%code_lookup_tables$iso_code] #Keeping GTM, UGA, COD, and SEN
# saveRDS(fgh_data, paste0(j, "/Project/Evaluation/GF/resource_tracking/_fgh/raw_data/FGH_EZ_2018.rds"))

#This is the previous version of the FGH data, which only had information through 2016. 
# fgh_data <- fread(paste0(fgh_raw, "archive/ihme_dah_cod_uga_gtm_1990_2016.csv"))

# ----------------------------------------------
# OTHER DAH ACTUALS 
# ----------------------------------------------
{
  fgh_data = readRDS(paste0(fgh_raw, "FGH_EZ_2018.rds"))
  
  #Read in the mapping documents 
  fgh_mapping <- fread(paste0(mapping_dir, "fgh_mapping.csv"), stringsAsFactors = FALSE)
  fgh_mapping = fgh_mapping[disease != "" & code != "" & !is.na(coefficient)] #Only keep the rows we've classified fully. 
  fgh_mapping = unique(fgh_mapping[, .(activity_description, code, coefficient)])
  
  final_mapping <- fread(paste0(mapping_dir, "all_interventions.csv"))
  final_mapping = final_mapping[, -c("disease", 'module_fr', 'intervention_fr', 'module_esp', 'intervention_esp', 'abbrev_mod_eng')] #Remove the disease column because we don't need it, and remove unfinished spanish and french columns. 
  final_mapping = unique(final_mapping)
  
  # ----------------------------------------------
  # Prep the DAH data
  # ----------------------------------------------
  
  setnames(fgh_data, c("source", "iso3_rc"), c("dah_origin","loc_name"))
  
  fgh_data$financing_source <- mapply(get_dah_source_channel, fgh_data$channel)
  fgh_data = fgh_data[, -c('channel')] #Remove unnecessary categorical variables 
  disease_vars = grep("hiv|mal|tb|swap", names(fgh_data)) #Grab HIV, TB, Malaria, and RSSH variables 
  id_vars = grep("year|financing_source|loc_name", names(fgh_data))
  keep_cols =c(id_vars, disease_vars)
  
  fgh_data = fgh_data[, .SD, .SDcols=keep_cols]
  
  #Drop 'total' columns 
  fgh_data = fgh_data[, -c('hiv_dah_18', 'mal_dah_18', 'tb_dah_18', 'swap_hss_total_dah_18')]
  
  ## "melt" the data: 
  fghData <-  melt(fgh_data, id=c("year", "financing_source", "loc_name"), variable.name = "activity_description", value.name="disbursement")
  fghData$disbursement <- as.numeric(fghData$disbursement)
  
  ##get the disease column: 
  fghData[grepl("hiv", activity_description), disease:='hiv']
  fghData[grepl("mal", activity_description), disease:='malaria']
  fghData[grepl("tb", activity_description), disease:='tb']
  fghData[grepl("swap|hss", activity_description), disease:='rssh']
  if (nrow(fghData[is.na(disease)])!=0){
    print(fghData[is.na(disease), .(activity_description)])
    stop("Not all rows were correctly labeled with disease!")
  }
  
  ##sum the disbursement by the other variables just to remove any duplicates: 
  byVars = c('loc_name', 'year', 'financing_source', 'disease', 'activity_description')
  fghData = fghData[, .(disbursement=sum(disbursement, na.rm=T)), 
                    by=byVars]
  
  fghData$fin_data_type <- "actual"
  
  #Make sure all data are uniquely identified before merges below. 
  fghData[duplicated(fghData, by=c('loc_name', 'year', 'financing_source', 'disease', 'activity_description')), dup:=TRUE]
  stopifnot(nrow(fghData[dup==TRUE])==0)
  fghData$dup<-NULL
  
  fgh_mapping[duplicated(fgh_mapping), dup:=TRUE]
  stopifnot(nrow(fgh_mapping[dup==TRUE])==0)
  fgh_mapping$dup<-NULL
  
  final_mapping[duplicated(final_mapping), dup:=TRUE]
  stopifnot(nrow(final_mapping[dup==TRUE])==0)
  final_mapping$dup<-NULL
  
  #Check that disbursement will still be correct after coefficient redistribution. 
  pre_check = fghData[, .(pre_check = sum(disbursement, na.rm=T)), by='activity_description']
  
  #Map this data to global fund modules and interventions. 
  fgh_to_codes <- merge(fghData, fgh_mapping, by='activity_description', all.x = TRUE, allow.cartesian=T)
  if (nrow(fgh_to_codes[is.na(coefficient) | is.na(code)])!=0){
    print(fgh_to_codes[is.na(coefficient) | is.na(code), .(activity_description)])
    stop("FGH codes did not merge correctly!")
  }
  fgh_mapped <- merge(fgh_to_codes, final_mapping, by='code', all.x = TRUE, allow.cartesian=T)
  if (nrow(fgh_mapped[is.na(module_eng)])!=0){
    print(fgh_mapped[is.na(module_eng), .(activity_description)])
    print("FGH modules/interventions did not merge correctly!")
  }
  
  #Apply redistributive coefficients 
  fgh_mapped[, disbursement:=disbursement*coefficient]
  
  post_check = fgh_mapped[, .(post_check = sum(disbursement, na.rm=T)), by='activity_description']
  post_check = merge(pre_check, post_check, by='activity_description', all=T)
  if (nrow(post_check[pre_check!=post_check])!=0){
    print(post_check[pre_check!=post_check])
    stop("Coefficient redistribution error!")
  }
  
  #Check merge 
  #the number of rows of fgh_to_codes and fgh_mapped at this point should be the same. 
  stopifnot(nrow(fgh_mapped) == nrow(fgh_to_codes))
  
  #-----------------------------------------------
  # Relabel "unspecified" columns
  #-----------------------------------------------
  fgh_mapped[module_eng=="Unspecified" & disease=="rssh", module_eng:="Unspecified RSSH"]
  fgh_mapped[intervention_eng=="Unspecified" & disease=="rssh", intervention_eng:="Unspecified RSSH"]
  
  fgh_mapped[module_eng=="Unspecified" & disease=="hiv", module_eng:="Unspecified HIV"]
  fgh_mapped[intervention_eng=="Unspecified" & disease=="hiv", intervention_eng:="Unspecified HIV"]
  
  fgh_mapped[module_eng=="Unspecified" & disease=="malaria", module_eng:="Unspecified Malaria"]
  fgh_mapped[intervention_eng=="Unspecified" & disease=="malaria", intervention_eng:="Unspecified Malaria"]
  
  fgh_mapped[module_eng=="Unspecified" & disease=="tb", module_eng:="Unspecified TB"]
  fgh_mapped[intervention_eng=="Unspecified" & disease=="tb", intervention_eng:="Unspecified TB"]
  
  # ----------------------------------------------
  # Sort the data nicely 
  # ----------------------------------------------
  fgh_mapped = fgh_mapped[, .(loc_name, year, financing_source, disease, activity_description, disbursement, code, module_eng, intervention_eng)] #Would be good to do this off of the codebook!! Emily 6/19/19
  setnames(fgh_mapped, c('module_eng', 'intervention_eng'), c('gf_module', 'gf_intervention'))
  fgh_mapped = fgh_mapped[order(loc_name, year, financing_source, disease, activity_description)]
  
  # ----------------------------------------------
  # export the FGH data 
  # ----------------------------------------------
  
  #Save the full dataset 
  write.csv(fgh_mapped, paste0(fgh_prepped, "other_dah_actuals_all.csv"), row.names=FALSE)
  saveRDS(fgh_mapped, paste0(fgh_prepped, "other_dah_actuals_all.rds"))
  
  #Save a version for each country
  write.csv(fgh_mapped[loc_name=="COD"], paste0(fgh_prepped, "other_dah_actuals_all_cod.csv"), row.names=FALSE)
  saveRDS(fgh_mapped[loc_name=="COD"], paste0(fgh_prepped, "other_dah_actuals_all_cod.rds"))
  
  write.csv(fgh_mapped[loc_name=="GTM"], paste0(fgh_prepped, "other_dah_actuals_all_gtm.csv"), row.names=FALSE)
  saveRDS(fgh_mapped[loc_name=="GTM"], paste0(fgh_prepped, "other_dah_actuals_all_gtm.rds"))
  
  write.csv(fgh_mapped[loc_name=="SEN"], paste0(fgh_prepped, "other_dah_actuals_all_sen.csv"), row.names=FALSE)
  saveRDS(fgh_mapped[loc_name=="SEN"], paste0(fgh_prepped, "other_dah_actuals_all_sen.rds"))
  
  write.csv(fgh_mapped[loc_name=="UGA"], paste0(fgh_prepped, "other_dah_actuals_all_uga.csv"), row.names=FALSE)
  saveRDS(fgh_mapped[loc_name=="UGA"], paste0(fgh_prepped, "other_dah_actuals_all_uga.rds"))

}

# ----------------------------------------------
# GHE ACTUALS (Malaria and HIV, coming from separate files)
# ----------------------------------------------

#GHE MALARIA 
# {
#   ghe_malaria = fread(paste0(j, "/Project/IRH/Malaria/processed_data/all_data_vetted.csv"), stringsAsFactors = FALSE)
#   ghe_malaria = ghe_malaria[tolower(ihme_loc_id)%in%code_lookup_tables$iso_code]
#   ghe_malaria[, year_diff:=year_end-year_start]
#   
#   ghe_malaria[, value:=gsub(",", "", value)]
#   ghe_malaria[, value:=as.numeric(value)]
#   
#   #Validate data 
#   stopifnot(unique(ghe_malaria$units)=="USD") #NOOOOOO
#   
#   
#   stopifnot(nrow(ghe_malaria[is.na(value)])==0)
#   ghe_malaria[, .(expenditure=sum(value, na.rm=T)), by=c('ihme_loc_id', 'year_id', 'value_code', 'source_type', 'report_year', 'month_start', 'year_start', 'month_end', 'year_end')]
#   
#   saveRDS(ghe_malaria, paste0(fgh_prepped, "ghe_actuals_malaria.rds"))
#   write.csv(ghe_malaria, paste0(fgh_prepped, "ghe_actuals_malaria.csv"), row.names=FALSE)
# } 

print("Completed step 3A: Prep FGH Actuals.")
