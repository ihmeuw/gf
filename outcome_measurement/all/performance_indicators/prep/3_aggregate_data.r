# ----------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Cleaning and merging all data together. 
# DATE: Last updated August 2019.  
# INSTRUCTIONS: Working directory should be the root of your personal repository. 
# ----------------------------------------------

for (country in c('cod', 'sen', 'uga', 'gtm')){
  assign(paste0(country, "_1A"), readRDS(paste0(prepped_dir, country, "_1A.rds")))
  assign(paste0(country, "_1A_disagg"), readRDS(paste0(prepped_dir, country, "_1A_disagg.rds")))
  assign(paste0(country, "_1B"), readRDS(paste0(prepped_dir, country, "_1B.rds")))
  assign(paste0(country, "_1B_disagg"), readRDS(paste0(prepped_dir, country, "_1B_disagg.rds")))
  
  if (!'all_data'%in%ls()){
    all_data = copy(get(paste0(country, "_1A")))
    all_data = rbindlist(list(all_data, get(paste0(country, "_1A_disagg")), 
                         get(paste0(country, "_1B")), get(paste0(country, "_1B_disagg"))), 
                         use.names=T, fill=T)
  } else {
    all_data = rbindlist(list(all_data, get(paste0(country, "_1A")), get(paste0(country, "_1A_disagg")), 
                              get(paste0(country, "_1B")), get(paste0(country, "_1B_disagg"))), 
                         use.names=T, fill=T)
  }

}

#Fix duplicate names, and make sure you've resolved all issues. 
dup_names = duplicated(names(all_data))
dup_cols = names(all_data)[dup_names==T]
stopifnot(names(all_data)[dup_names==T]%in%dup_cols)

#Is there any difference between these duplicate rows? Or were they just in PUDRs twice? Remove if so. 
for (c in dup_cols){
  duplicates = all_data[, .(get(c), get(c))]
  names(duplicates) = c('col1', 'col2')
  duplicates[col1==col2, equal:=T]
  stopifnot(nrow(duplicates[equal!=T])==0)
  
  col_index = grep(c, names(all_data))
  all_data = all_data[, !c(col_index[1]), with=F]
}

#Make sure you got them all. 
dup_names = duplicated(names(all_data))
if (any(dup_names)){
  print(names(all_data)[dup_names==T])
  warning("There are duplicate names in the combined database!")
}


#---------------------------------------------
# SAVE FINAL DATA 
#---------------------------------------------
saveRDS(all_data, paste0(prepped_dir, "all_prepped_data.rds"))
write.csv(all_data, paste0(prepped_dir, "all_prepped_data.csv"), row.names=F)

#Archive a copy. 
saveRDS(all_data, paste0(prepped_dir, "archive/all_prepped_data_", Sys.Date(), ".rds"))

print("Step 2: Aggregate and clean data completed. Outputs saved in prepped data folder.")
