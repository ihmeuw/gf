# prep qualitative comments from PUDR Indicator tabs

# SET UP
# clear memory
rm(list=ls())

##########
# Switches
# What countries do you want to run? 
countries = c('cod', 'uga', 'sen', 'gtm') #Add country codes to this list to prep them. Possible choices are 'cod', 'gtm', 'sen', and 'uga'. 
prep_quali_data = TRUE # Set to true if you want to re-run all data again

verbose = FALSE #Set to true if you want to print more detailed error messages. 

# run setup code (load file paths and packages)
user=as.character(Sys.info()[7])
if (Sys.info()[1]=='Windows'){
  repo_root = paste0("C:/Users/", user, "/Documents/gf/") #Change to the root of your repository
  setwd(repo_root)
} else {
  setwd(paste0("/ihme/homes/",user,"/gf/"))
}

source('./outcome_measurement/all/performance_indicators/prep/set_up_r.r', encoding="UTF-8")
source('./resource_tracking/prep/_common/shared_functions.r', encoding="UTF-8")
source('./resource_tracking/prep/_common/load_master_list.r', encoding="UTF-8")

# LOAD ORIGINAL FILE LIST
master_file_list = load_master_list(purpose="performance indicators")

#Only want to keep approved_gm files (EL 9/12/2019 - what about PUDR revisions?)
master_file_list = master_file_list[file_iteration=="approved_gm"]

#Flag files where sheets are NA - if they are duplicated with non-NA files, drop these. 
na_sheets = master_file_list[is.na(sheet_impact_outcome_1a) | sheet_impact_outcome_1a=="NA" |
                               is.na(sheet_impact_outcome_1a_disagg) | sheet_impact_outcome_1a_disagg=="NA" |
                               is.na(sheet_coverage_1b) | sheet_coverage_1b=="NA" | 
                               is.na(sheet_coverage_1b_disagg) | sheet_coverage_1b_disagg=="NA", file_name]

#Make sure you don't have the same start date for the same grant (quick check; it would be better )
master_file_list[, date_dup:=sequence(.N), by=c('grant', 'grant_period', 'start_date_programmatic', 'pudr_semester_programmatic')] #EMILY NEED TO RETHINK THIS. 
master_file_list[, date_dup:=date_dup-1]#This indexes at one, so you need to decrement it

#Drop duplicates that are NA 
dup_grants = master_file_list[date_dup==1, .(grant, grant_period, pudr_semester_programmatic)]
dup_grants[, concat:=paste0(grant, grant_period, pudr_semester_programmatic)]
dup_files = master_file_list[paste0(grant, grant_period, pudr_semester_programmatic)%in%dup_grants$concat, file_name]

master_file_list = master_file_list[!(file_name%in%na_sheets & file_name%in%dup_files)]

#Re-do duplicates check. 
master_file_list$date_dup = NULL
master_file_list[, date_dup:=sequence(.N), by=c('grant', 'grant_period', 'start_date_programmatic', 'pudr_semester_programmatic')] #EMILY NEED TO RETHINK THIS. 
master_file_list[, date_dup:=date_dup-1]#This indexes at one, so you need to decrement it

if ( nrow(master_file_list[date_dup>0])!=0){
  print(master_file_list[date_dup > 0, .(file_name, file_iteration, grant, grant_period, start_date_financial)][order(grant, grant_period, start_date_financial)])
  print("There are duplicates in approved_gm files - review file list.")
}

#Only extract indicators for current grant periods! EL 9/20/2019 
current_periods = c('2018-2020', '2016-2019', '2019-2021', '2019-2022')
master_file_list = master_file_list[grant_period%in%current_periods]

# PREP DATA using newly modified function

#-------------------------------------------
#Prep Coverage 1B sheets 
#-------------------------------------------
if (prep_quali_data){
  print("Prepping Coverage 1B qualitative comments...")
  for (country in countries){
    print(paste0("Processing country: ", country))
    file_list = master_file_list[loc_name==country & file_iteration=="approved_gm" & !is.na(sheet_coverage_1b)] 
    
    #Just force-change all of the sheet names for now - will want to make sure this is right EKL 6/25/19 
    #country_dir = ? 
    master_file_dir = paste0(box, toupper(country), "/raw_data/")
    
    # Validate file list 
    if (nrow(file_list)>0){ #Do you have files for this country to process? 
      for (i in 1:nrow(file_list)){
        # Set up file path 
        folder = "budgets"
        folder = ifelse (file_list$data_source[i] == "pudr", "pudrs", folder)
        if (file_list$file_iteration[i]=="initial"){
          version = "iterations"
        } else if (file_list$file_iteration[i]=="revision"){
          version= "revisions"
        } else {
          version = ""
        }
        grant_period = file_list$grant_period[i]
        
        file_dir = paste0(master_file_dir, file_list$grant_status[i], "/", file_list$grant[i], "/", grant_period, "/", folder, "/")
        if (version != ""){
          file_dir = paste0(file_dir, version, "/")
        }
        
        args = list(file_dir, file_list$file_name[i], file_list$sheet_coverage_1b[i], file_list$language_1b[i])
        tmpData = do.call(prep_coverage_1B_comments, args) 
        
        #Append important columns from the file list 
        append_cols = file_list[i, .(loc_name, grant, grant_period, grant_status, file_name, disease, primary_recipient, start_date_programmatic, end_date_programmatic, pudr_semester_programmatic)]
        for (col in names(append_cols)){
          tmpData[, (col):=append_cols[, get(col)]]
        }  
        tmpData$pudr_sheet = "coverage_indicators_main"
        
        #Bind the files together 
        if (i == 1){
          coverage1B = tmpData
        } else {
          coverage1B = rbind(tmpData, coverage1B, fill=T, use.names=T) #You won't always have the same column names, and that's ok. 
        }
        print(paste0(i, " ", file_list$grant[i], " ", file_list$grant_period[i])) ## if the code breaks, you know which file it broke on
        
      }
      # subset the columns of interest
      df <- coverage1B[,.(loc_name, grant, grant_period, start_date_programmatic, end_date_programmatic, pudr_semester_programmatic,
                                      module, indicator, 
                                      pr_result_achievement_ratio, lfa_result_achievement_ratio, gf_result_achievement_ratio,
                                      pr_comments, lfa_comments, gf_comments)]
      
      write.xlsx(df, paste0(prepped_dir,"qualitative_data/", country, "_1B_comments.xlsx"))
      saveRDS(df, paste0(prepped_dir, "qualitative_data/", country, "_1B_comments.RDS"))

    }
    else {
      print(paste0("No applicable Coverage 1B files for ", country))
  }
  }
}
      
# # Aggregate all country data
# for (country in c('cod', 'sen', 'uga', 'gtm')){
#   assign(paste0(country, "_1B_qc"), as.data.table(read.xlsx(paste0(prepped_dir,"qualitative_data/", country, "_1B_comments.xlsx"))))
#   
#   if (!'all_data'%in%ls()){
#     all_data = copy(get(paste0(country, "_1B_qc")))
#     # all_data = rbindlist(list(all_data, get(paste0(country, "_1A_disagg")), 
#     #                           get(paste0(country, "_1B")), get(paste0(country, "_1B_disagg"))), 
#     #                      use.names=T, fill=T)
#   } else {
#     all_data = rbindlist(list(all_data, get(paste0(country, "_1B_qc")), 
#                          use.names=T, fill=T))
#   }
  
# }
  
#### Finalize Data for each country and save in correct folder on J Drive

# for (country in c('cod', 'uga', 'sen', 'gtm')){
#     df <- as.data.table(read.xlsx(paste0(prepped_dir,"qualitative_data/", country, "_1B_comments.xlsx"), sheetIndex = 1))
#     
# 
#   }

# # # add information on pudr semester
for (country in c('uga', 'gtm')){
  
  df <- readRDS(paste0(prepped_dir, "qualitative_data/", country, "_1B_comments.RDS"))
  setnames(df, 'pudr_semester_programmatic', 'pudr_code')
  pudr_labels_abbrev = read.xlsx(paste0("J:/Project/Evaluation/GF/resource_tracking/documentation/PUDR Semester Labeling Qual.xlsx"), sheetIndex = 1)
  df = merge(df, pudr_labels_abbrev, by=c('pudr_code'), all.x=T)
    if (nrow(df[is.na(sem_abbrev)])>0){
      print(unique(df[is.na(sem_abbrev), .(pudr_code)]))
      stop("Values of pudr_code did not merge correctly.")
      }


# there are duplicate files in the cod data so I will just append an extra number on the duplicates with same indicator
if (sum(duplicated(df, by=c("grant", "loc_name", "grant_period", "sem_abbrev", "module", "indicator")))>0){
  # create vector of which columns are duplicates
  duplicate_rows <- which(duplicated(df, by=c("grant", "loc_name", "grant_period", "sem_abbrev", "module", "indicator")))
  i <- 1
  for (i in 1:length(duplicate_rows)){
    df[duplicate_rows[i]]$indicator <- paste0(df[duplicate_rows[i]]$indicator, " (", i, ")")
  }
}

# cast wide
# subset once more then cast wide
coverage1Bqual <- df[,.(loc_name, grant, grant_period, sem_abbrev,
                        module, indicator, 
                        pr_result_achievement_ratio, lfa_result_achievement_ratio, gf_result_achievement_ratio,
                        pr_comments, lfa_comments, gf_comments)]

coverage1Bqual_wide <- dcast(coverage1Bqual, loc_name + grant + grant_period + module + indicator ~ sem_abbrev, 
                             value.var = c("pr_result_achievement_ratio", "lfa_result_achievement_ratio", "gf_result_achievement_ratio", "pr_comments", "lfa_comments", "gf_comments"))

# set col order
if (country%in%c('uga','cod')){
  setcolorder(coverage1Bqual_wide, 
              c("loc_name", "grant", "grant_period", "module", "indicator", 
                "pr_result_achievement_ratio_sem_1", "lfa_result_achievement_ratio_sem_1", "gf_result_achievement_ratio_sem_1", 
                "pr_comments_sem_1", "lfa_comments_sem_1", "gf_comments_sem_1",
                "pr_result_achievement_ratio_sem_2", "lfa_result_achievement_ratio_sem_2", "gf_result_achievement_ratio_sem_2", 
                "pr_comments_sem_2", "lfa_comments_sem_2", "gf_comments_sem_2",
                "pr_result_achievement_ratio_sem_3", "lfa_result_achievement_ratio_sem_3", "gf_result_achievement_ratio_sem_3", 
                "pr_comments_sem_3", "lfa_comments_sem_3", "gf_comments_sem_3",
                "pr_result_achievement_ratio_sem_4", "lfa_result_achievement_ratio_sem_4", "gf_result_achievement_ratio_sem_4", 
                "pr_comments_sem_4", "lfa_comments_sem_4", "gf_comments_sem_4"))
}else if (country=='sen'){
  setcolorder(coverage1Bqual_wide, 
              c("loc_name", "grant", "grant_period", "module", "indicator", 
                "pr_result_achievement_ratio_sem_1", "lfa_result_achievement_ratio_sem_1", "gf_result_achievement_ratio_sem_1", 
                "pr_comments_sem_1", "lfa_comments_sem_1", "gf_comments_sem_1",
                "pr_result_achievement_ratio_sem_2", "lfa_result_achievement_ratio_sem_2", "gf_result_achievement_ratio_sem_2", 
                "pr_comments_sem_2", "lfa_comments_sem_2", "gf_comments_sem_2",
                "pr_result_achievement_ratio_sem_3", "lfa_result_achievement_ratio_sem_3", "gf_result_achievement_ratio_sem_3", 
                "pr_comments_sem_3", "lfa_comments_sem_3", "gf_comments_sem_3",
                "pr_result_achievement_ratio_sem_3-4", "lfa_result_achievement_ratio_sem_3-4", "gf_result_achievement_ratio_sem_3-4", 
                "pr_comments_sem_3-4", "lfa_comments_sem_3-4", "gf_comments_sem_3-4"))
}else if (country=='gtm'){
  setcolorder(coverage1Bqual_wide, 
              c("loc_name", "grant", "grant_period", "module", "indicator", 
                "pr_result_achievement_ratio_sem_1", "lfa_result_achievement_ratio_sem_1", "gf_result_achievement_ratio_sem_1", 
                "pr_comments_sem_1", "lfa_comments_sem_1", "gf_comments_sem_1",
                "pr_result_achievement_ratio_sem_2", "lfa_result_achievement_ratio_sem_2", "gf_result_achievement_ratio_sem_2", 
                "pr_comments_sem_2", "lfa_comments_sem_2", "gf_comments_sem_2",
                "pr_result_achievement_ratio_sem_1-2", "lfa_result_achievement_ratio_sem_1-2", "gf_result_achievement_ratio_sem_1-2", 
                "pr_comments_sem_1-2", "lfa_comments_sem_1-2", "gf_comments_sem_1-2",
                "pr_result_achievement_ratio_sem_3", "lfa_result_achievement_ratio_sem_3", "gf_result_achievement_ratio_sem_3", 
                "pr_comments_sem_3", "lfa_comments_sem_3", "gf_comments_sem_3",
                "pr_result_achievement_ratio_sem_4", "lfa_result_achievement_ratio_sem_4", "gf_result_achievement_ratio_sem_4", 
                "pr_comments_sem_4", "lfa_comments_sem_4", "gf_comments_sem_4",
                "pr_result_achievement_ratio_sem_5", "lfa_result_achievement_ratio_sem_5", "gf_result_achievement_ratio_sem_5", 
                "pr_comments_sem_5", "lfa_comments_sem_5", "gf_comments_sem_5",
                "pr_result_achievement_ratio_sem_6", "lfa_result_achievement_ratio_sem_6", "gf_result_achievement_ratio_sem_6", 
                "pr_comments_sem_6", "lfa_comments_sem_6", "gf_comments_sem_6"))
}
# save file along with other PUDR qualitative data

grants_processed <- unique(coverage1Bqual_wide[loc_name==country]$grant)

# for g in grants_processed{
#   path <- paste0("J:/Project/Evaluation/GF/resource_tracking/qualitative_data/",country,"/", g,"_pudr_comments.xlsx")
#   sheet_name = "indicators"
#   if(sheet_name %in% names(getSheets(loadWorkbook(path)))){
#     wb <- loadWorkbook(path)
#     removeSheet(wb, sheetName = sheet_name)
#     saveWorkbook(wb, path)
#   }
# }

for (g in grants_processed){
  require(xlsx)
  write.xlsx(coverage1Bqual_wide[loc_name==country & grant==g], file = paste0("J:/Project/Evaluation/GF/resource_tracking/qualitative_data/",country,"/", g,"_pudr_comments.xlsx"), sheetName="indicators", append=TRUE, row.names=FALSE)
  print(paste0(g, " pudr indicator comments saved in output directory")) ## if the code breaks, you know which file it broke on
}
}
