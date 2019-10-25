# --------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: script that cleans PUDR Indicators Performance Framework Data for analyses
# DATE: Last updated October 2019 
# NOTE: this version assumes that all country data is stored in same file
# --------------------------------------

#----------------------------------------------
# TO-DO list for this code: 
# - Need to reshape wide based off of the most "final" indicator (PR, LFA, GF)
# - systematically test for strange data formats. 
#----------------------------------------------

#  Read in data, and correct names 
dt = readRDS(paste0(prepped_dir, "all_prepped_data.rds")) 
#Make sure that nothing is a factor. 
names(dt) = gsub("%", "pct", names(dt)) #EMILY THIS SHOULD BE CORRECTED IN PREP CODE

#--------------------------------------------------
# Remove unnecessary rows - #EMILY this may be a data extraction issue. 
#--------------------------------------------------
dt <- dt[indicator!="[Impact Indicator Name]"]
dt <- dt[indicator!="[Outcome Indicator Name]"]
dt <- dt[indicator!="0"]

#---------------------------------------------
# clean special characters, blanks, and NAs # EMILY- how are you systematically testing for this? 
#---------------------------------------------
numVars = names(dt)[grepl("_value|_year|_n|_d|_pct|_achievement_ratio", names(dt))]
numVars = numVars[!numVars%in%c('start_date_programmatic', 'end_date_programmatic', 'report_due_date')]

for (var in numVars) { 
  if (verbose){print(var)}
  #Do an overall check - how many rows have at least one digit for this value (numeric information?) Is it the same at the end? 
  start_numeric = nrow(dt[grepl("[[:digit:]]", get(var))])
  # REMOVE NAs
  dt[get(var)=="N/A" | get(var)=="ND" | get(var)==" ", (var):=NA]
  start_nas = nrow(dt[is.na(get(var))]) #Make sure that this numeric conversion doesn't accidentally introduce any NAs after this point.
  
  # LOOK FOR "E-" AND CONVERT SCIENTIFIC NOTATION
  dt[grepl("E\\-", get(var)), neg_sci_notation:=TRUE]
  dt[grepl("E\\+", get(var)), pos_sci_notation:=TRUE]
  if (nrow(dt[neg_sci_notation==TRUE | pos_sci_notation==TRUE])>0){
    if (verbose){
      print("These are the values that will be converted from scientific notation.")
      print(dt[pos_sci_notation==TRUE | neg_sci_notation, unique(get(var))])
    }
    dt[neg_sci_notation==TRUE, c('value', 'rounding'):=tstrsplit(get(var), "E\\-", fixed=TRUE)]
    dt[pos_sci_notation==TRUE, c('value', 'rounding'):=tstrsplit(get(var), "E\\+", fixed=TRUE)]
    dt[, value:=as.numeric(value)]
    dt[, rounding:=as.numeric(rounding)]
    if (verbose){
      print("These are the values that will be converted from scientific notation.")
      print(unique(dt[pos_sci_notation==TRUE | neg_sci_notation, .(variable=get(var), value, rounding)]))
    }
    
    #Convert values 
    dt[neg_sci_notation==TRUE, (var):=value*((.1)^rounding)]
    dt[pos_sci_notation==TRUE, (var):=value*(10)^rounding]
  } 
  
  # REMOVE ALPHANUMERIC CHARACTERS 
  dt[, (var):=gsub("[[:alpha:]]", "", get(var))] #Remove alphabetic characters. 
 
  # DIVIDE VALUES THAT HAVE A FORWARD SLASH.
  if (nrow(dt[grepl("/", get(var))])>0){
    dt[grepl("/", get(var)), num:=tstrsplit(get(var), "/", keep=1)]
    dt[grepl("/", get(var)), denom:=tstrsplit(get(var), "/", keep=2)]
    dt[!is.na(num), clean_num:=gsub(",", "\\.", num)][, clean_num:=as.numeric(clean_num)] #Replace commas with periods for these variables (denominator should be 100)
    dt[!is.na(denom), clean_denom:=gsub(",", "\\.", denom)][, clean_denom:=as.numeric(clean_denom)]
      if (verbose){
        print("These are the values that will be divided using new numerators and denominators.")
        print(unique(dt[grepl("/", get(var)), .(variable=get(var), num, clean_num, denom, clean_denom)]))
      }
    dt[grepl("/", get(var)), (var):=clean_num/clean_denom]
  } 
  
  #REPLACE COMMAS WITH PERIODS. 
  if (verbose & nrow(dt[grepl(",", get(var))])>0){
    print("These are the values that will have commas replaced with periods.") 
    print(dt[grepl(",", get(var)), unique(get(var))])
  }
  dt[, (var):=gsub(",", "\\.", get(var))] #replace commas with periods.
  
  #If values have multiple periods after this step, remove them.
  if (verbose & nrow(dt[str_count(get(var), "\\.")>1])>0) {
    print("These are the variables that have more than one period - these periods will be removed because they were likely commas that got converted in the last step.")
    print(dt[str_count(get(var), "\\.")>1, unique(get(var))])
  }
  dt[str_count(get(var), "\\.")>1, (var):=gsub("\\.", "", get(var))] 
  
  #REMOVE PUNCTUATION
  dt[, (var):=gsub("?|%|‰", "", get(var))] # There's probably some fancy way to do this with excluding characters from [[:punct:]] ? 
  
  #Turn all of these variables numeric. 
  dt[, (var):=as.numeric(get(var))]
  
  end_nas = nrow(dt[is.na(get(var))]) #Make sure you didn't accidentally introduce any NAs. 
  if (start_nas!=end_nas){
    print(var) 
    stop("NAs introduced by cleaning code!")
  }
  
  end_numeric = nrow(dt[grepl("[[:digit:]]", get(var))])
  if (start_numeric!=end_numeric){
    print(var)
    stop("Numeric information was lost during cleaning process!")
  }
}

# Calculate an internal verified achievement ratio.
dt[, ihme_achievement_ratio:=gf_result_achievement_ratio]
dt[is.na(ihme_achievement_ratio), ihme_achievement_ratio:=lfa_result_achievement_ratio]
dt[is.na(ihme_achievement_ratio), ihme_achievement_ratio:=pr_result_achievement_ratio]

# Are there any cases where your calculation of the indicator ratio would be different than the actual? 

# create variable with indicator code (makes cross--country comparisons easier) can merge short description using this code later
dt = dt[, indicator_code:=tstrsplit(indicator, ":", keep=1)]

# read and merge codebook to standardize names
codebook <- fread(paste0(code_dir, "indicators_codebook.csv"))

# EMILY use a path built up from global variables. 
merged_dt <- merge(dt, codebook, by="indicator_code", all.x = TRUE)


#-----------------------------------------------------
# Map indicator codes 
#-----------------------------------------------------

#Clean indicators before merging with codebook.  
dt[, indicator:=gsub("&amp;", "&", indicator)]

# load code book and PUDR PFI Database
codebook = fread(paste0(code_dir, "special_assessments/synthesis/2019_multicountry_analyses/data_source_codebook.csv"), header = TRUE)

# Merge and replace the Baselice source code 
data <- merge(dt, codebook, by.x="baseline_source", by.y = "source_original", all.x=TRUE)
data1 <- data[,baseline_source_code:=source_code]
data1 <- data1[,c("source_code"):=NULL]

# Merge and replace the pr result source code
data2 <- merge(data1, codebook, by.x="pr_result_source", by.y = "source_original", all.x = TRUE)
data3 <- data2[,pr_result_source_code:=source_code]
data3 <- data3[,source_code:=NULL]

# merge on new codebook with all indicator names
# read and merge codebook to standardize names
codebook_names <- fread("C:/Users/frc2/Documents/gf/special_assessments/synthesis/2019_multicountry_analyses/indicators_codebook_full.csv") 
data4 <- merge(data3, codebook_names, by="indicator_code", all.x = TRUE)

#------------------------------------------------------
# SAVE FINAL DATA
# -----------------------------------------------------
saveRDS(merged_dt, paste0(prepped_dir, "cleaned_pfi.rds"))
saveRDS(merged_dt, paste0(prepped_dir, "archive/cleaned_pfi_", Sys.Date(), ".rds"))



