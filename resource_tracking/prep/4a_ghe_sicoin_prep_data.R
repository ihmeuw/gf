#------------------------------------------------
# AUTHOR: Irena Chen, modified by Emily Linebarger
# DATE: Written in Nov. 2017, and modified May 2019
# PURPOSE: Preps SICOIN files. 
# ----------------------------------------------

#------------------------------------------------
# TO DO: Can we check if the files in our file list include everything in the raw data folder? 

# ----------------------------------------------
# load the list of sicoin files 
# ----------------------------------------------
file_list = fread(paste0(dir, "_ghe/sicoin_gtm/sicoin_file_list.csv"))
file_list[, start_date:=as.Date(start_date, format="%m/%d/%Y")]

#Subset to one function at a time so we can see what they're doing. 
file_list = file_list[format=='detailed']


## loop over all of the files 
for(i in 1:length(file_list$file_name)){
  
  sicoinDir = paste0(dir, "_ghe/sicoin_gtm/")  
  inFile = paste0(sicoinDir,file_list$file_path[i],file_list$file_name[i])
  
  if(file_list$format[i]=="detailed"){
    tmpData = prep_detailed_sicoin(inFile)
  } else {
    print("Function hasn't been verified yet.")
  }
  
  #Still need to modify these functions! 
  #   else if (file_list$format[i]=="summary"){
  #   tmpData = prep_summary_sicoin(as.character(paste0(sicoinDir,file_list$file_path[i],file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  # } else if (file_list$format[i]=="blank"){
  #   tmpData = prep_blank_sicoin(country, adm1, ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  # } else if(file_list$format[i]=="donacions"){
  #   tmpData = prep_donacions_sicoin(as.character(paste0(sicoinDir,file_list$file_path[i],file_list$file_name[i])),
  #                                    ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i],
  #                                    country,  adm1)
  # } else if (file_list$format[i]=="report"){
  #   tmpData = prep_report_sicoin(as.character(paste0(sicoinDir,file_list$file_path[i],file_list$file_name[i])), ymd(file_list$start_date[i]), file_list$disease[i], file_list$period[i], file_list$source[i])
  # }
  
  #Add extra columns 
  append_cols = file_list[i, .(file_name, start_date, disease, period)]
  for (col in names(append_cols)){
    tmpData[, (col):=append_cols[, get(col)]]
  }  
  tmpData[, end_date:=start_date+file_list$period[i]]
  
  # Bind together with previously run files. 
  if(i==1){
    resource_database = tmpData
  } else {
    resource_database = rbind(resource_database, tmpData, use.names=TRUE)
  }

  print(paste0(i, " ", file_list$format[i], " ", file_list$file_name[i])) ## if the code breaks, you know which file it broke on
}

resource_database$data_source = "sicoin"
resource_database$lang = "esp"
saveRDS(resource_database, paste0(sicoin_prepped, "raw_bound_files.rds"))

# ---------------------------------------------
# Clean data and run quality checks 
# ---------------------------------------------

#Are there cases of overlapping or missing dates? 
date_dups = unique(resource_database[, .(department, municipality, file_name, start_date, disease)])
date_dups[, count:=1]
date_dups[, dup:=sum(count), by=c('department', 'municipality', 'start_date', 'disease')]
if (length(unique(date_dups$dup)>1)){
  date_dups = date_dups[order(department, municipality, start_date)]
  View(date_dups[dup>1])
  print("Duplicate data - the same month is being covered for the same geographic area in two different files. Review.")
}
date_dups = date_dups[order(department, municipality, start_date)]
date_dups = duplicate(date_dups)

##check for duplicates:
dups=resource_database[duplicated(resource_database) | duplicated(resource_database, fromLast=TRUE)]

#EMILY THIS CAN'T HAPPEN. 
# Convert from Quitzal to USD
conversion_table = data.table("year" = c("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"), 
                              "conversion" = c(7.74022,	7.5532,	7.3301,	7.45875,	7.42153,	8.01039,	7.92282,	7.64965,	7.68406,	7.71407,	7.59794,	7.49704,	7.43533,	7.18309,	7.31697))

resource_database$year = substring(resource_database$start_date, 1, 4)
resource_database = merge(resource_database, conversion_table, by = "year", allow.cartesian = TRUE)
resource_database$budget = resource_database$budget / resource_database$conversion
resource_database$disbursement = resource_database$disbursement / resource_database$conversion

resource_database$year = NULL
resource_database$conversion = NULL









