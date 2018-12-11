# facilities composition
# add to the facilities list so org units are classified by tyoe
# 8/29/18
# Caitlin O'Brien-Carelli
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(reshape)
library(epibasix)
# --------------------
# merge on the cluster
# files take a long time to load - merge in a cluster IDE

# sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p 1247 -s 10 -P snis_download  

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#-------------------------
# import the meta data for the merge
facilities <- data.table(readRDS(paste0(dir, 'meta_data/master_facilities.rds')))

# delete the unecessary variables
facilities[ ,c("country", "country_id", "dps_id", "health_area_id"):=NULL]

#---------------------------
# create a type of organisational unit variable

facilities[is.na(dps), type:='dps']
facilities[is.na(health_zone) & is.na(type), type:='health zone']
facilities[is.na(health_area) & is.na(type), type:='health area']
facilities[is.na(type), type:='facility']

#-----------------------------------------------
# type of facility

# create organisational unit name in lower case and replace the diacritical marks
facilities[ , org_unit1:=tolower(org_unit)]

# function to remove diacritical marks
fix_diacritics <- function(x) {
  replacement_chars = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')
  
  replace_me <- paste(names(replacement_chars), collapse='')
  replace_with <- paste(replacement_chars, collapse = '')    
  return(chartr(replace_me, replace_with, x))
  
}

facilities$org_unit1 <- fix_diacritics(facilities$org_unit1)

#----------------------
# classify the facilities
# run the clinic code first - since a number of facilities have clinic and another class

# geographic areas
facilities[grep(pattern="aire de sante", x=org_unit1), level:='health_area']
facilities[grep(pattern="zone de sante", x=org_unit1), level:='health_zone']
facilities[grep(pattern="province", x=org_unit1), level:='dps']

# one health area is misclassified in type 
facilities[level=='health area' & type!='health area', level:='health_area']

# facilities by level 
facilities[grep(pattern="\\sclinique", x=org_unit1), level:='clinic']
facilities[grep(pattern="centre de sante", x=org_unit1), level:='health_center']
facilities[grep(pattern="centre de sante de reference", x=org_unit1), level:='reference_health_center']
facilities[grep(pattern="poste de sante", x=org_unit1), level:='health_post']
facilities[grep(pattern="centre medical", x=org_unit1), level:='medical_center']
facilities[grep(pattern="\\shopital", x=org_unit1), level:='hospital']
facilities[grep(pattern="\\shopital secondaire", x=org_unit1), level:='secondary_hospital']
facilities[grep(pattern="hopital general de reference", x=org_unit1), level:='general_reference_hospital']
facilities[grep(pattern="hgr", x=org_unit1), level:='general_reference_hospital']
facilities[grep(pattern="centre hospitalier", x=org_unit1), level:='hospital_center']
facilities[grep(pattern="dispensaire", x=org_unit1), level:='dispensary']
facilities[grep(pattern="polyclinique", x=org_unit1), level:='polyclinic']
facilities[grep(pattern="centre medico-chirurgical", x=org_unit1), level:='medical_surgical_center']

# typos 
facilities[grep(pattern="centre sante", x=org_unit1), level:='health_center']
facilities[grep(pattern="poste sante", x=org_unit1), level:='health_post']
facilities[grep(pattern="general de reference", x=org_unit1), level:='general_reference_hospital']

#--------------------------------------------
# save the new data table

saveRDS(facilities, paste0(dir, 'all_units/master_facilities.rds' ))

#--------------------------------------------
# create data tables to use for the matching analysis 

# delete the 80 facilities with typos in the names 
facilities <- facilities[!is.na(level)]

# save a list of every health zone and all of its associated facilities
facilities[type=='facility', unique(level)]
hz <- facilities[type=='facility']
saveRDS(hz, paste0(dir,'all_units/health_zone_facilities.rds' ))

# health zone and the count of each facility type - shaped long
hz2 <- facilities[type=='facility', .(value=length(unique(org_unit))), by=.(health_zone, dps, variable=level)]

saveRDS(hz2, paste0(dir,'all_units/health_zone_counts_long.rds' ))

# health zone and the count of each facility type - shaped wide
hz3 <- cast(hz2)

replace_na = function(x) {
  for (j in names(x))
    set(x,which(is.na(x[[j]])),j,0)
}

replace_na(hz3)

hz3 <- data.table(hz3)
saveRDS(hz3, paste0(dir,'all_units/health_zone_counts_wide.rds' ))

#------------------------------------------


new <- melt(hz3, id.vars=c('health_zone', 'dps'))



for (f in unique(new$variable)) {
  print(f)
  print(summary(new[variable==f]$value))
}

new[ ,unique(variable)]
new[variable=='general_reference_hospital' , variable:='grh']
new[variable=='medical_surgical_center' , variable:='med_surg']
new[variable=='reference_health_center' , variable:='ref_hc']
new[variable=='secondary_hospital' , variable:='sec_hosp']
new[variable=='hospital_center' , variable:='hosp_center']

new <- new[order(variable),]

list_of_plots = NULL
i=1

for(f in unique(new$health_zone)) {
  
  # look up district name
  name <- unique(new[health_zone==f]$health_zone)
  
  # graph for each facility
  list_of_plots[[i]] <- ggplot(new[health_zone==f], aes(x=variable, y=value)) + 
    geom_bar(stat='identity') + theme_bw() + labs(title=name)
  
  i=i+1
  
}

pdf('J:/Project/Evaluation/GF/outcome_measurement/cod/dhis/facilities_composition.pdf', height=6, width=12)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
}

dev.off()




list_of_plots = NULL
i=1

for(f in unique(hz$health_zone)) {
  
  # look up district name
  name <- unique(hz[health_zone==f]$health_zone)
  
  # graph for each facility
  list_of_plots[[i]] <- ggplot(hz[health_zone==f], aes(x=variable, y=value)) + 
    geom_bar(stat='identity') + theme_bw() + labs(title=name)
  
  i=i+1
  
}

pdf('J:/Project/Evaluation/GF/outcome_measurement/cod/dhis/facilities_composition.pdf', height=6, width=12)
for(i in seq(length(list_of_plots))) { 
  print(list_of_plots[[i]])
}

dev.off()



