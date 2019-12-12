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
# - address "NAs introduced by cleaning code error" warning
#----------------------------------------------

#  Read in data, and correct names 
dt = readRDS(paste0(prepped_dir, "all_prepped_data.rds"))
#Make sure that nothing is a factor. 
names(dt) = gsub("%", "pct", names(dt)) #EMILY THIS SHOULD BE CORRECTED IN PREP CODE

#--------------------------------------------------
# Remove unnecessary rows - #EMILY this may be a data extraction issue. Mostly they are hidden rows in GTM PU/DRs
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
    dt[grepl("/", get(var)), (var):=clean_num/clean_denom] # this piece of code seems to lead to a warning message
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


# Cleaning up uganda target_pct column in which some values are incorrectly typed into the PUDRs

dt$target_pct[which(dt$loc_name=="uga" & dt$target_pct==1)] <- 100
dt$target_pct[which(dt$loc_name=="uga" & dt$target_pct==0.5)] <- 50
dt$target_pct[which(dt$loc_name=="uga" & dt$target_pct==0.85)] <- 85
dt$target_pct[which(dt$loc_name=="uga" & dt$baseline_year==2014 & dt$baseline_value==19 & dt$indicator_type=="Impact")] <- 6.7


# Calculate an internal verified achievement ratio.
dt[, any_achievement_ratio:=gf_result_achievement_ratio]
dt[is.na(any_achievement_ratio), any_achievement_ratio:=lfa_result_achievement_ratio]
dt[is.na(any_achievement_ratio), any_achievement_ratio:=pr_result_achievement_ratio]

# Are there any cases where your calculation of the indicator ratio would be different than the actual? 
#yes, in that case we calculate our own
# achievement ratio using the best numerator and denominator

# calculate '_value' variables which are either the percent reported or the numerator (if indicator is not a proportion or percent)
# this is done for the one target value reported and the three sources of result values (PR, LFA, and GF)
dt$target_value <- ifelse(is.na(dt$target_pct),dt$target_n, dt$target_pct)
dt$pr_result_value <- ifelse(is.na(dt$pr_result_pct), dt$target_n, dt$pr_result_pct)
dt$lfa_result_value <- ifelse(is.na(dt$lfa_result_pct), dt$lfa_result_n, dt$lfa_result_pct)
dt$gf_result_value <- ifelse(is.na(dt$gf_result_pct), dt$gf_result_n, dt$gf_result_pct)

# create the any_result_value which is will gather any available result value reported by any of the three sources
dt[, any_result_value:=gf_result_value]
dt[is.na(any_result_value), any_result_value:=lfa_result_value]
dt[is.na(any_result_value), any_result_value:=pr_result_value]

# create completeness rating for target and result value
dt$completeness_rating <- NA

dt$completeness_rating[which(   is.na(dt$target_value)  &  is.na(dt$any_result_value))] <- 1
dt$completeness_rating[which(   is.na(dt$target_value)  & !is.na(dt$any_result_value))] <- 2
dt$completeness_rating[which(  !is.na(dt$target_value)  &  is.na(dt$any_result_value))] <- 3
dt$completeness_rating[which(  !is.na(dt$target_value)  & !is.na(dt$any_result_value))] <- 4

# create factor variable and assign names
dt$completeness_rating <- factor(dt$completeness_rating)

levels(dt$completeness_rating) <-  c("No data", "Only Result", "Only Target", "Both available")

# calculate if the sources differ between the baseline value and the pr reported value
dt$sources_different <- NA
dt$sources_different[which(dt$baseline_source_code!=dt$pr_result_source_code)] <- 1
dt$sources_different[which(dt$baseline_source_code==dt$pr_result_source_code)] <- 0

#-----------------------------------------------------
# Map indicator codes 
#-----------------------------------------------------

#Clean indicators before merging with codebook.  
dt[, indicator:=gsub("&amp;", "&", indicator)]

# create variable with indicator code (makes cross--country comparisons easier) can merge short description using this code later
dt = dt[, indicator_code:=tstrsplit(indicator, ":", keep=1)]

# load code book on data sources
codebook_sources = fread(paste0(book_dir, "data_source_codebook.csv"), header = TRUE)

# Merge and replace the Baseline source code 
dt1 <- merge(dt, codebook_sources, by.x="baseline_source", by.y = "source_original", all.x=TRUE)
dt2 <- dt1[,baseline_source_code:=source_code]
dt2 <- dt2[,c("source_code"):=NULL]

# Merge and replace the pr result source code
dt2 <- merge(dt1, codebook_sources, by.x="pr_result_source", by.y = "source_original", all.x = TRUE)
dt3 <- dt2[,pr_result_source_code:=source_code]
dt3 <- dt3[,source_code:=NULL]

# read and merge codebook to standardize names
codebook_stdnames <- fread(paste0(book_dir, "indicators_codebook.csv"))

# read and merge codebook to map indicator code names
merged_dt <- merge(dt3, codebook_stdnames, by="indicator_code", all.x = TRUE)

#------------------------------------------------------
# SAVE FINAL DATA
# -----------------------------------------------------
saveRDS(merged_dt, paste0(prepped_dir, "cleaned_pfi.rds"))
saveRDS(merged_dt, paste0(prepped_dir, "archive/cleaned_pfi_", Sys.Date(), ".rds"))
