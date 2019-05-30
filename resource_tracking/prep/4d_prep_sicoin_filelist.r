# ----------------------------------------------
# AUTHOR: Emily Linebarger 
# DATE: May 2019 
# PURPOSE: Recursively build up a file list of SICOIN 
# files, and verify them before reading into database. 
# ----------------------------------------------

#Main paths: 
base_dirs = c("SICOIN GT/MALARIA/MONTHLY", 
              "SICOIN GT/TUBERCULOSIS/MONTHLY", 
              "SICOIN GT/VIH/MONTHLY")

#Build up a file list by recursively scanning directories of raw data. 
#This code doesn't need to be re-run unless more base directories are added, or if new files 
# are added within these base directories. 
# Emily Linebarger 5.16.19
terminal_dirs = character()
file_list = character() #What columns do you want to have here?
for (path in base_dirs){
  path = paste0(sicoin_raw, path)
  new_dirs = get_dirs(path, character())
  terminal_dirs = append(terminal_dirs, new_dirs)
  for (dir in terminal_dirs){
    file_list = append(file_list, get_files(dir))
  }
}

#--------------------------------------------------------
#Take the raw data you've extracted, and pull out some information from it. 
#--------------------------------------------------------
fileList = data.table(file_list)
names(fileList) = 'file_name'

#Split the URL into pieces, because each piece gives information.
#The final element gives the disease, month, year, and type. 
fileList[, split:=tstrsplit(file_name, "/", keep=14)]
fileList[, disease:=tstrsplit(file_name, "/", keep=10)]

#Split this final element into more pieces. 
fileList[, month:=tstrsplit(split, " ", keep=2)]
fileList[, year:=tstrsplit(split, " ", keep=3)]
fileList[, year:=gsub(".xls|.xlsx", "", year)]
fileList[, function_type:=tstrsplit(split, " ", keep=4)]
fileList[, is_program_file:=tstrsplit(split, " ", keep=5)]
fileList[, program:=tstrsplit(split, " ", keep=6)]

#--------------------------------------------------------
#Validate and format data 
#--------------------------------------------------------
#Disease 
fileList[, disease:=tolower(disease)]
ok_diseases = c('malaria', 'tuberculosis', 'vih')
stopifnot(nrow(fileList[!disease%in%ok_diseases])==0)
fileList[disease=="tuberculosis", disease:='tb']
fileList[disease=="vih", disease:="hiv"]

#Month 
fileList[, month:=tolower(month)]
unique(fileList$month)

#Convert months to numeric - there are some typos in the file names. 
fileList[month=="jan" | month=="january" | month=="janaury", month:='1']
fileList[month=="feb" | month=="february", month:='2']
fileList[month=="march", month:='3']
fileList[month=="april" | month=="apribl" | month=="ap" | month=="apr", month:='4']
fileList[month=="may", month:='5']
fileList[month=="jun" | month=="june", month:='6']
fileList[month=="july", month:='7']
fileList[month=="aug" | month=="august", month:='8']
fileList[month=="sep" | month=="september" | month=="sept", month:='9']
fileList[month=="oct" | month=="octo" | month=="october", month:='10']
fileList[month=="nov" | month=="november", month:='11']
fileList[month=="december" | month=="dec" | month=="dic", month:='12']

ok_months = as.character(seq(1, 12, by=1))
#unique(fileList[!month%in%ok_months, .(month)])

#If the month wasn't grabbed correctly, then the year and the function type weren't either. Try again with these. 
type1_files = c('donations', 'gf', 'origin')
type2_files = as.character(seq(2004, 2018, by=1))

#Type 1 files 
fileList[month%in%type2_files, function_type:=tstrsplit(split, " ", keep=2)]
fileList[month%in%type1_files, month:=NA]

#Type 2 files 
fileList[month%in%type2_files, year:=tstrsplit(split, " ", keep=2)]
fileList[month%in%type2_files, function_type:=tstrsplit(split, " ", keep=3)]
fileList[month%in%type2_files, month:=NA]

#Fix program file labeling for program files 
fileList[, is_program_file:=gsub(".xls", "", is_program_file)]
fileList[, program:=gsub(".xls", "", program)]

fileList[is_program_file=="12", program:="12"]
fileList[is_program_file=="14", program:="14"]

fileList[is_program_file=="12" | is_program_file=="14" | is_program_file=="PROG", is_program_file:="TRUE"]
fileList[is_program_file!="TRUE", is_program_file:="FALSE"]

#---------------------------------------------------------
#Manual edits 
#---------------------------------------------------------
#Month
fileList[month=="2011.xls", month:=NA]

#Manual edits for year 
fileList[year=="2O17" | year == "217", year:="2017"]
fileList[year=="211", year:="2011"]

#Edits to function type 
fileList[, function_type:=tolower(function_type)]
fileList[, function_type:=gsub(".xls|.xlsx", "", function_type)]

fileList[function_type=="donatios", function_type:="donations"]
fileList[function_type=='origin', function_type:='detailed']
fileList[function_type=='gf' | function_type=="global" | function_type=="prog", function_type:='summary']

#---------------------------------------------------------
#Validate data
#---------------------------------------------------------
#Convert months and years to numeric
ok_months = as.character(seq(1, 12, by=1))
ok_months = c(ok_months, NA)
ok_years = as.character(seq(2004, 2018, by=1))
ok_years = c(ok_years, NA)
ok_functions = c('donations', 'summary', 'detailed', '')
ok_functions = c(ok_functions, NA)

stopifnot(nrow(fileList[!(month%in%ok_months)])==0)
stopifnot(nrow(fileList[!year%in%ok_years])==0)
stopifnot(nrow(fileList[!function_type%in%ok_functions])==0)

#Make month and year numeric, and make start dates. 
fileList[, start_date:=as.Date(paste0("01-", month, "-", year), format="%d-%m-%Y")]
fileList[, month:=as.numeric(month)]
fileList[, year:=as.numeric(year)]

unique(fileList$month)
unique(fileList$year)
unique(fileList$function_type)

#----------------------------------------------------------------
# Drop rows that aren't needed, and then continue verification. 
# Any rows that have NA for month at this point are just overview files for the whole year, 
# and we just want to keep monthly data. Emily Linebarger 5/16/19
#----------------------------------------------------------------
View(fileList[is.na(month)])
fileList = fileList[!is.na(month)]

#Is there any overlap in disease and start date? 
fileList[, count:=1]
fileList[, dup:=sum(count), by=c('start_date', 'disease', 'function_type', 'program')] #Can we just assume that all function types are unique inputs here? 
View(fileList[dup>1][order(disease, start_date)]) #What's going on with the Prog 12, Prog 14 files? And there are still 4 duplicates you need to resolve here. 

#Are you missing any months in the detailed function column for any disease? 
expected_dates = expand.grid(year = seq(min(fileList$year), max(fileList$year), by=1), month=seq(1, 12, by=1))
hiv = unique(fileList[function_type=="detailed" & disease=="hiv", .(month, year, disease)])
hiv = merge(hiv, expected_dates, all.y=T)
tb = unique(fileList[function_type=="detailed" & disease=="tb", .(month, year, disease)])
tb = merge(tb, expected_dates, all.y=T)
malaria = unique(fileList[function_type=="detailed" & disease=="malaria", .(month, year, disease)])
malaria = merge(malaria, expected_dates, all.y=T)

missing_hiv = hiv[is.na(disease)]
missing_hiv[, disease:='hiv']
missing_tb = tb[is.na(disease)]
missing_tb[, disease:='tb']
missing_mal = malaria[is.na(disease)]
missing_mal[, disease:='malaria']

missing_months = rbind(missing_hiv, missing_tb, missing_mal) #REVIEW THIS FILE IF YOU NEED TO SEE WHAT MONTHS YOU'RE MISSING. 
missing_months = missing_months[order(disease, year, month)]
#Drop unneeded rows 
fileList = fileList[, -c('split', 'count', 'dup')]

#--------------------------------------------------------
#Save this file
#--------------------------------------------------------
write.csv(fileList, paste0(sicoin_raw, "sicoin_filelist.csv"), row.names=FALSE)
