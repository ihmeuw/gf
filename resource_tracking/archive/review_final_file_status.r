#----------------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Compare final files in resource tracking 
# with archived database 
# DATE: July 3, 2019
#----------------------------------------------------

library(data.table) 


#--------------------------------------
# PREP DATA 
#--------------------------------------
current_db = readRDS("J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/combined_prepped_data/budget_pudr_iterations.rds")
archive_db = fread("J:/Project/Evaluation/GF/resource_tracking/archive/cleaned_total_data_bc_archive.csv")


current_files = unique(current_db[, .(grant, grant_period, file_name, file_iteration, data_source)])
setnames(current_files, 'file_iteration', 'current_version')
archive_files = unique(archive_db[!data_source%in%c('sicoin', 'gos', 'fgh'), .(grant_number, grant_period, fileName, data_source)])
setnames(archive_files, c('grant_number', 'fileName', 'data_source'), c('grant', 'file_name', 'archive_version'))

#Standardize "version" and "data source" variables for archived DB
unique(archive_files$archive_version)

archive_files[grepl('fpm', archive_version), data_source:='fpm']
archive_files[grepl('pudr', archive_version), data_source:='pudr']
stopifnot(unique(archive_files$data_source)%in%c('fpm', 'pudr'))

archive_files[archive_version%in%c('fpm', 'pudr'), archive_version:='final']
archive_files[archive_version%in%c('fpm_iter', 'init_fpm_dec', 'init_fpm', 'iterated_fpm', 'fpm_init', 'init_fpm_dfeb'), archive_version:='initial']
stopifnot(unique(archive_files$archive_version)%in%c('final', 'initial'))

#Fix some of the grant names 
archive_files[grant=="GTM-M-UPCOMING", grant:="GTM-M-MSPAS"]
archive_files[grant=="GTM-T-UPCOMING", grant:="GTM-T-MSPAS"]


# #Make sure current DB has the same structure before merge 
stopifnot(unique(current_files$data_source)%in%c('fpm', 'pudr'))
stopifnot(unique(current_files$current_version)%in%c('final', 'initial'))


#Merge the two files together on data source, grant, grant period, and file name, and see if you've got the same version. 
merge = merge(current_files, archive_files, by=c('grant', 'grant_period', 'data_source', 'file_name'), all=T)

#--------------------------------------
# ANALYSIS
#--------------------------------------
#View final budgets
View(merge[current_version=="final" & data_source == "fpm"])
#View final PUDRs
View(merge[current_version=="final" & data_source=="pudr"])

#Are there any duplicated file names in the merge? 
merge[duplicated(file_name), dup:=TRUE]
merge[dup==TRUE]


#Where doeS the current version NOT match the archived verion? 
merge[current_version=="initial" & archive_version=="final"]
# Do we have any cases by grant, grant_period, and data_source where we have two files covering the same period? 

check1 = unique(merge[data_source=="fpm" & (current_version=="final" | archive_version=="final"), .(grant, grant_period, data_source, file_name)])
check1[duplicated(check1, by=c('grant', 'grant_period', 'data_source')), ERROR:=TRUE]
#Just view the whole dataset
View(merge[order(current_version)])

View(check1[ERROR==TRUE])
View(merge[data_source=="fpm" & grant%in%c('UGA-C-TASO', 'UGA-H-MoFPED', 'GTM-T-MSPAS')])
