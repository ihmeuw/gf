
# ----------------------------------------------
# Irena Chen
#
# 2/22/2018

### This code cleans the FGH csv file and turns it into something that is like our resource tracking database

# ----------------------------------------------
# call libraries 
# ----------------------------------------------
rm(list=ls())
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(readxl)
library(reshape)
library(scales)

#----------------------------------------------
# Source functions
# ---------------------------------------------
repo = 'C:/Users/elineb/Documents/gf/'
source(paste0(repo, 'resource_tracking/prep/shared_mapping_functions.R'))
source(paste0(repo, 'resource_tracking/prep/fgh_prep/fgh_prep_functions.R'))

# ----------------------------------------------
# Load the DAH data and other raw files 
# ----------------------------------------------

fgh_data <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/ihme_dah_cod_uga_gtm_1990_2016.csv")
fgh_mapping <- fread("J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/fgh_mapping.csv")
fgh_mapping = fgh_mapping[disease != "" & code != "" & !is.na(coefficient)] #Only keep the rows we've classified fully. 
fgh_mapping = fgh_mapping[, !"disease"] #Remove the disease column because we don't need it. 
final_map <- "J:/Project/Evaluation/GF/mapping/multi_country/intervention_categories/intervention_and_indicator_list.xlsx"
final_mapping <- load_mapping_list(final_map, include_rssh_by_disease = FALSE)
final_mapping = final_mapping[, !"disease"] #Remove the disease column because we don't need it.

# ----------------------------------------------
# Prep the DAH data
# ----------------------------------------------

setnames(fgh_data, c("source", "iso3_rc"), c("dah_origin","loc_name"))

fgh_data$oid_zika_dah_17 = as.numeric(fgh_data$oid_zika_dah_17)
fgh_data$financing_source <- mapply(get_dah_source_channel, fgh_data$channel)
fgh_data$channel = NULL
fghData<- fgh_data[, -c("dah_origin", "dah_17", "total_mal_17", "total_hiv_17", "total_tb_17"), with=FALSE] #Remove 'total' columns and origin variable - it looks like 
#this is giving the closest sum to "dah_17" from the raw file, but it's not 100% accurate. Need the codebook to make sure we're including the right columns here. 

## "melt" the data: 
fghData <-  melt(fghData, id=c("year", "financing_source", "loc_name"), variable.name = "sda_activity", value.name="disbursement") #Do we want these different funding streams to be "activities"?? EKL
fghData$disbursement <- as.numeric(fghData$disbursement)

#EKL would really like to get a codebook if this data if at all possible- what does dah_17 represent? 
##get the disease column: 
fghData$disease <- mapply(get_disease, fghData$sda_activity)

## add in 
fghData[loc_name=='COD', adm1:=171] 
fghData[loc_name=='GTM', adm1:=128] 
fghData[loc_name=='UGA',  adm1:=190] 

##sum the disbursement by the other variables just to remove any duplicates: 
byVars = c('year', 'disease', 'financing_source','sda_activity', 'loc_name', 'adm1')
fghData = fghData[, disbursement:=sum(na.omit(disbursement)), 
                  by=byVars]
fghData = unique(fghData)

fghData$fin_data_type <- "actual"

#Map this data to global fund modules and interventions. 
fgh_to_codes <- merge(fghData, fgh_mapping, by='sda_activity', all.x = TRUE, allow.cartesian = TRUE)
fgh_to_codes[is.na(coefficient), coefficient:=1] #For things that didn't map, make sure disbursement isn't growing or shrinking 
fgh_mapped <- merge(fgh_to_codes, final_mapping, by='code', all.x = TRUE)

#Apply redistributive coefficients 
fgh_mapped[, disbursement:=disbursement*coefficient]

#Check merge 
#the number of rows of fgh_to_codes and fgh_mapped at this point should be the same. 
stopifnot(nrow(fgh_mapped) == nrow(fgh_to_codes))
# The sum of disbursement from fghData and fgh_mapped should be the same.
stopifnot(fgh_mapped[, sum(disbursement, na.rm=TRUE)] == fghData[, sum(disbursement, na.rm=TRUE)])

#Create variables. 
fgh_mapped$fileName <- "ihme_dah_cod_uga_gtm_1990_2016.csv"
fgh_mapped$fin_data_type <- "actual"

# ----------------------------------------------
# prep the HIV THE data from the FGH team  
# ----------------------------------------------

ghe_data <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/gpr_corrected_final_gbd4.csv")

##country codes for GTM, UGA, and DRC
country_codes <- c(128, 190,171)

ghe_data <- ghe_data[location_id %in% country_codes]

ghe_data$model <- NULL
ghe_data$hiv_pop <- NULL
setnames(ghe_data, c("location_id", "year_id"), c("adm1", "year")) ##POSSIBLY HERE
ghe_wide <- reshape(ghe_data,direction='wide',
                    idvar=c("adm1", "year"),
                   timevar="value_code")

## (THE - OOP+PPP+GHE = DAH (in theory))

oop_vars <- names(ghe_wide)[grepl(c("oop"), names(ghe_wide))]
ppp_vars <- names(ghe_wide)[grepl(c("ppp"), names(ghe_wide))]
ghe_vars <- names(ghe_wide)[grepl(c("public"), names(ghe_wide))]


##only take the variables that we want: 
ghe_cleaned <- ghe_wide[, c("adm1", "year", 
                            "ensemble_mean.fs_hiv_domestic_private_oop","ensemble_lower.fs_hiv_domestic_private_oop", "ensemble_upper.fs_hiv_domestic_private_oop",
                            "ensemble_mean.fs_hiv_domestic_private_ppp",  "ensemble_lower.fs_hiv_domestic_private_ppp", "ensemble_upper.fs_hiv_domestic_private_ppp",
                            "ensemble_mean.fs_hiv_domestic_public",  "ensemble_lower.fs_hiv_domestic_public", "ensemble_upper.fs_hiv_domestic_public",
                            "ensemble_upper.the_hiv", "ensemble_lower.the_hiv", "ensemble_mean.the_hiv"), with=FALSE]

#reshape "long" so that all of the dah/the/other estimates are in 1 column: 
ghe_cleaned <- melt(ghe_cleaned, id.vars = c("adm1", "year"),
                    variable.name = "fin_data_type", value="disbursement")

#add the disease variable, and module/intervention (all will be an 'Unspecified') for HIV
ghe_cleaned$disease <- 'hiv'
ghe_cleaned$sda_activity <- 'Unspecified'
ghe_cleaned$code <- 'H99'
ghe_cleaned$coefficient <- 1

#Map to final mapping 
ghe_mapped <- merge(ghe_cleaned, final_mapping, by='code', all.x = TRUE)

##assign loc_name id based on their ISO codes: 
ghe_mapped[adm1==128, loc_name:="GTM"]
ghe_mapped[adm1==190, loc_name:="UGA"]
ghe_mapped[adm1==171, loc_name:="COD"]

ghe_mapped$financing_source <- mapply(get_the_source_channel, as.character(ghe_mapped$fin_data_type))
ghe_mapped = ghe_mapped[financing_source != "the"]
ghe_mapped$fileName <- "gpr_corrected_final_gbd4.csv"

ghe_mapped$fin_data_type <- mapply(transform_fin_data_type, as.character(ghe_mapped$fin_data_type)) #CHECK FOR ESTIMATES VS MODELS
# ----------------------------------------------
# ## rbind the DAH and the forecasted HIV THE: 
# ----------------------------------------------
totalFgh <- rbind(fgh_mapped, ghe_mapped, fill = TRUE) #EKL how are we sure these data don't overlap? 


# ----------------------------------------------
# Add descriptive variables 
# ----------------------------------------------

totalFgh$data_source <- "fgh"
totalFgh$period <- 365
totalFgh$start_date <- paste0(totalFgh$year, "-01-01")
totalFgh$start_date  <- as.Date(totalFgh$start_date,"%Y-%m-%d")
totalFgh$end_date <- paste0(totalFgh$year, "-12-31") 
totalFgh$adm2 <- totalFgh$adm1
totalFgh$lang <- "eng"

totalFgh$country = mapply(get_country_name, totalFgh$loc_name)
totalFgh$loc_name = tolower(totalFgh$loc_name)

#Remove all total rows from data. 
totalFgh = totalFgh[!grep("total_", sda_activity)] #This should not happen here!!! EKL Needs to happen above. 

# ----------------------------------------------
# export the FGH data 
# ----------------------------------------------

write.csv(totalFgh,"J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_current_fgh.csv", row.names=FALSE)


