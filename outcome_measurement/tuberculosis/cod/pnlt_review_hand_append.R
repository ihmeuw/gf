#---------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Review hand-appended DRC TB data 
# DATE: June 25, 2019 
#----------------------------------------------

rm(list=ls())

library(data.table)
library(openxlsx)
library(readxl)

dt18 = data.table(read.xlsx("J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/aggregated_data/TB DATA 2018.xlsx"))
dt16_17 = data.table(read.xlsx("J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/aggregated_data/TB DATA 2016_2017.xlsx"))

#---------------------------------
# 2018 DATA 
#---------------------------------
#Pull the important name columns and number of columns for later. 
names1 = names(dt18)
names2 = dt18[1, ]
names3 = dt18[2, ]
names4 = dt18[3, ]
names5 = dt18[4, ]

append_cols = ncol(dt18)

#Fix names - just focus on quarter and DPS for now. 
setnames(dt18, c('X1', 'INFORMATION.GENERALE'), c('quarter', 'dps'))

#Review unique values for quarter and DPS 
dt18[, quarter_new:=substr(quarter, nchar(quarter)-7, nchar(quarter))]
dt18[quarter_new%in%c('RIMESTRE', 'L  CPLT '), quarter_new:=NA]
dt18[, quarter_new:=trimws(quarter_new)]

unique(dt18$quarter_new)
unique(dt18$dps)

#Drop out "total" rows in DPS column 
total_rows = grep("total", tolower(dt18$dps))
dt18 = dt18[!total_rows, ]

#--------------------------------
#ERROR FLAGS

#Error 1 - remaining totals in quarters
dt18[grepl("total", tolower(quarter)), error1:=TRUE]
if (nrow(dt18[error1==TRUE])!=0){
  print("ERROR: There are some total labels in the quarter column.")
  print(paste0("This impacts ", round((nrow(dt18[error1==TRUE])/nrow(dt18))*100, 2), "% of the data."))
}

#Error 2 - 0's in dps
dt18[grepl("0", tolower(dps)), error2:=TRUE]
if (nrow(dt18[error2==TRUE])!=0){
  print("ERROR: There are 0's in the DPS column.")
  print(paste0("This impacts ", round((nrow(dt18[error2==TRUE])/nrow(dt18))*100, 2), "% of the data."))
}

#Error 3 - if there is a 0 in DPS, are there any real values in other variables? 
errors = dt18[error2==TRUE]
cols = c(names(dt18)[!names(dt18)%in%c('dps', 'quarter', 'quarter_new', 'error1', 'error2')])
for (col in cols){
  errors[, (col):=as.numeric(get(col))]
}

error3_check = errors[dps=="0", lapply(.SD, sum), by=c('dps', 'quarter'), .SDcols = cols]
#---------------------------------

#---------------------------------
# 2016-2017 DATA 
#---------------------------------

#Fix names - just focus on quarter and DPS for now. 
setnames(dt16_17, c('X1', 'X2'), c('quarter', 'dps'))

#Review unique values for quarter and DPS 
dt16_17[, quarter_new:=substr(quarter, nchar(quarter)-7, nchar(quarter))]
dt16_17[quarter_new%in%c('RIMESTRE', 'L  CPLT '), quarter_new:=NA]
dt16_17[, quarter_new:=trimws(quarter_new)]

#Drop out "total" rows in DPS column 
total_rows = grep("total", tolower(dt16_17$dps))
dt16_17 = dt16_17[!total_rows, ]

unique(dt16_17$quarter_new)
unique(dt16_17$dps)

#--------------------------------
#ERROR FLAGS

#Error 1 
dt16_17[grepl("total", tolower(quarter)), error1:=TRUE]
if (nrow(dt16_17[error1==TRUE])!=0){
  print("ERROR: There are some total labels in the quarter column.")
  print(paste0("This impacts ", round((nrow(dt16_17[error1==TRUE])/nrow(dt16_17))*100, 2), "% of the data."))
}

#Error 2 - 0's in dps
dt16_17[grepl("0", tolower(dps)), error2:=TRUE]
if (nrow(dt16_17[error2==TRUE])!=0){
  print("ERROR: There are 0's in the DPS column.")
  print(paste0("This impacts ", round((nrow(dt16_17[error2==TRUE])/nrow(dt16_17))*100, 2), "% of the data."))
}

#Error 3 - if there is a 0 in DPS, are there any real values in other variables? 
errors = dt16_17[error2==TRUE]
cols = c(names(dt16_17)[!names(dt16_17)%in%c('dps', 'quarter', 'quarter_new', 'error1', 'error2')])
for (col in cols){
  errors[, (col):=as.numeric(get(col))]
}

error3_check = errors[dps=="0", lapply(.SD, sum), by=c('dps', 'quarter'), .SDcols = cols]
#---------------------------------


#-------------------------------------------------------------
# CHECK THAT ALL COLUMNS MATCH WITH RAW DATA FOR 2018 
#-------------------------------------------------------------
files_2018 = list.files("J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/2018/Data TB 2018", full.names=TRUE)
t4_2018 = files_2018[grep("T4", files_2018)]
length(files_2018) #109
length(t4_2018) #27 - sounds about right. There were a few duplicate uploads. 

#Compare the column names and rows 1-4 (sub-names) for each file with 2018 data. 
for (file in t4_2018){
  print(paste0("Checking file ", file))
  sheets = excel_sheets(file)
  
  sheet = sheets[grep("dep", tolower(sheets))] #Grab "Depistage"
  sheet = sheet[grep("t4", tolower(sheet))] #Grab T4
  if (file == "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/2018/Data TB 2018/NORD KIVU T4 2018.xlsx"){
    sheet = "DEPIST TB VIH T4 2018"
  } if (file == "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/2018/Data TB 2018/TSHOPO T4 2018.xlsx"){
    
  }
  if (length(sheet)!=1){
    print("WARNING: did not isolate testing sheet.")
  }
  # print(sheets[1:10])
  # warning_flag = FALSE
  # dt = tryCatch({
  #   read.xlsx(file, sheet="DEPIST T4 2018")
  # }, warning=function(war){
  #   message("The sheet does not exist.")
  #   return(NA)
  # }, error = function(err){
  #   message("The sheet does not exist.")
  #   return(NA)
  # })
  # 
  # if(!is.na(dt)){
  #   if (ncol(dt)!=append_cols){
  #     print("ERROR: Inconsistent number of columns with append file.")
  #   }
  #   
  #   # dt_names1 = names(dt)
  #   # dt_names2 = dt[1, ]
  #   # dt_names3 = dt[2, ]
  #   # dt_names4 = dt[3, ]
  #   # dt_names5 = dt[4, ]
  #   # 
  #   # if (dt_names1!=names1){
  #   #   print("ERROR: Row 1 names different.")
  #   # } 
  #   # if (dt_names2!=names2){
  #   #   print("ERROR: Row 2 names different.")
  #   # }  
  #   # if (dt_names3!=names3){
  #   #   print("ERROR: Row 3 names different.")
  #   # }  
  #   # if (dt_names4!=names4){
  #   #   print("ERROR: Row 4 names different.")
  #   # } 
  #   # if (dt_names5!=names5){
  #   #   print("ERROR: Row 5 names different.")
  #   # }
  # }
  # 
  print("...")
}