
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

# ----------------------------------------------
##declare functions to help prep the data:  #EKL pull these functions out. Or are all of these necessary??? 
# ----------------------------------------------
get_dah_source_channel <- function(channel){ #EKL do we like these classifications? 
  x <- "Other bilateral"
  if(channel=="GFATM"){
    x <- "The Global Fund"
  } else if(channel=="BIL_USA"){
    x <- "U.S. bilateral"
  } else if(channel == "NGO" | channel == "INTLNGO" | channel == "US_FOUND" | channel == "BMGF"){
    x <- "NGOs and foundations"
  } else if(channel == "AfDB" | channel == "WB_IDA" | channel == "WB_IBRD" | channel == "AsDB" | channel == "IDB" | channel == "UNITAID"){
    x <- "UN Agencies, The World Bank, and regional development banks"
  } else if (channel == "GAVI"){
    x <- "GAVI"
  }
  return(x)
}

get_the_source_channel <- function(channel){
  x <-channel
  if(grepl("dah", channel)){
    x <- "dah"
  } else if(grepl( "the",channel)){
    x <- "the"
  } else if(grepl( "oop",channel)){
    x <- "oop"
  } else if(grepl( "ppp",channel)){
    x <- "ppp"
  } else if(grepl( "public",channel)){
    x <- "ghe"
  } else {
    x <- x
  }
  return(x)
}

get_disease <- function(sda_orig){
  x <- "other"
  if(grepl("hiv", sda_orig)){
    x <- "hiv"
  } else if(grepl("mal", sda_orig)){
    x <- "malaria"
  } else if (grepl("tb", sda_orig)){
    x <- "tb"
  } else if (grepl("hss", sda_orig)){
    x <- "hss"
  } else {
    x <- x
  }
  return(x)
}


get_loc_id <- function(country){
  x <- "cod"
  if(grepl("Guatemala", country)){
    x <- "gtm"
  } else if(grepl("Uganda", country)){
    x <- "uga"
  } else {
    x <- x
  }
  return(x)
}

get_country_name <- function(loc_name){
  x <- "Congo (Democratic Republic)" 
  if(grepl("GTM", loc_name)){
    x <- "Guatemala"
  } else if(grepl("UGA", loc_name)){
    x <- "Uganda"
  } else {
    x <- x
  }
  return(x)
}

transform_fin_data_type <- function(fin_data_type){
  x <- "actuals"
  if(grepl("lower", fin_data_type)){
    x <- "model_estimates_lower_ci"
  } else if(grepl("upper", fin_data_type)){
    x <- "model_estimates_upper_ci"
  } else if(grepl("mean", fin_data_type)){
    x <- "model_estimates"
  } else {
    x <- x
  }
  return(x)
}

# ----------------------------------------------
## load the fgh DAH data
# ----------------------------------------------

fgh_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/ihme_dah_cod_uga_gtm_1990_2016.csv",fileEncoding="latin1"))

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
fghData$code <- "s98"
fghData$fileName <- "ihme_dah_cod_uga_gtm_1990_2016.csv"


#THERE IS ALREADY HIV DATA IN THIS DATASET EKL

# ----------------------------------------------
# prep the HIV THE data from the FGH team  
# ----------------------------------------------

ghe_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/gpr_corrected_final_gbd4.csv",fileEncoding="latin1"))

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

# 
# # ----------------------------------------------
# #  get the mean and lower/upper estimates of the sum of OOP (out of pocket), PPP (pre-paid private insurance) and GHE (gov.health expenditures)
# ### IF YOU CAN FIND AN EASIER WAY TO DO THIS, PLEASE DO #### 
# # ----------------------------------------------
# 
# ## sum of the means of OOP+PPP+GHE
# ghe_wide$mean_oop_ppp_ghe_agg <- (ghe_wide$ensemble_mean.fs_hiv_domestic_private_oop+
#                                     ghe_wide$ensemble_mean.fs_hiv_domestic_private_ppp + 
#                                     ghe_wide$ensemble_mean.fs_hiv_domestic_public)
#   
# #lower estimates
# ghe_wide$lower_oop_ppp_ghe_agg <- (ghe_wide$ensemble_lower.fs_hiv_domestic_private_oop+
#                                      ghe_wide$ensemble_lower.fs_hiv_domestic_private_ppp + 
#                                      ghe_wide$ensemble_lower.fs_hiv_domestic_public)
# 
# ##upper estimates
# ghe_wide$upper_oop_ppp_ghe_agg <- (ghe_wide$ensemble_upper.fs_hiv_domestic_private_oop+
#                                      ghe_wide$ensemble_upper.fs_hiv_domestic_private_ppp + 
#                                      ghe_wide$ensemble_upper.fs_hiv_domestic_public)
# 
# ##subtract OOP+PPP+GHE from THE to get DAH: 
# ghe_wide$mean_dah <- (ghe_wide$ensemble_mean.the_hiv - ghe_wide$mean_oop_ppp_ghe_agg)
# ghe_wide$lower_dah <- (ghe_wide$ensemble_lower.func_hiv_prev - ghe_wide$lower_oop_ppp_ghe_agg)
# ghe_wide$upper_dah <- (ghe_wide$ensemble_upper.the_hiv - ghe_wide$upper_oop_ppp_ghe_agg)
# 

##only take the variables that we want: 
ghe_cleaned <- ghe_wide[, c("adm1", "year", 
                            "ensemble_mean.fs_hiv_domestic_private_oop","ensemble_lower.fs_hiv_domestic_private_oop", "ensemble_upper.fs_hiv_domestic_private_oop",
                            "ensemble_mean.fs_hiv_domestic_private_ppp",  "ensemble_lower.fs_hiv_domestic_private_ppp", "ensemble_upper.fs_hiv_domestic_private_ppp",
                            "ensemble_mean.fs_hiv_domestic_public",  "ensemble_lower.fs_hiv_domestic_public", "ensemble_upper.fs_hiv_domestic_public",
                            "ensemble_upper.the_hiv", "ensemble_lower.the_hiv", "ensemble_mean.the_hiv"), with=FALSE]

#reshape "long" so that all of the dah/the/other estimates are in 1 column: 
ghe_cleaned <- melt(ghe_cleaned, id.vars = c("adm1", "year"),
                    variable.name = "fin_data_type", value="disbursement")

##add the disease variable
ghe_cleaned$disease <- "hiv"

##assign loc_name id based on their ISO codes: 
ghe_cleaned[adm1==128, loc_name:="GTM"]
ghe_cleaned[adm1==190, loc_name:="UGA"]
ghe_cleaned[adm1==171, loc_name:="COD"]
ghe_cleaned$sda_activity <- "all"
ghe_cleaned$code <- "S98"
ghe_cleaned$financing_source <- mapply(get_the_source_channel, as.character(ghe_cleaned$fin_data_type))
ghe_cleaned = ghe_cleaned[financing_source != "the"]
ghe_cleaned$fileName <- "gpr_corrected_final_gbd4.csv"

ghe_cleaned$fin_data_type <- mapply(transform_fin_data_type, as.character(ghe_cleaned$fin_data_type)) #CHECK FOR ESTIMATES VS MODELS
# ----------------------------------------------
# ## rbind the DAH and the forecasted HIV THE: 
# ----------------------------------------------
totalFgh <- rbind(fghData, ghe_cleaned) #Are these not overwriting each other? Where does the data come from? EKL

# ----------------------------------------------
# add RT variables to the FGH data 
# ----------------------------------------------


totalFgh$data_source <- "fgh"
totalFgh$period <- 365
totalFgh$start_date <- paste0(totalFgh$year, "-01-01")
totalFgh$start_date  <- as.Date(totalFgh$start_date,"%Y-%m-%d")
totalFgh$end_date <- paste0(totalFgh$year, "-12-31") 
totalFgh$budget <- 0 #Why do this? Why not make NA? 0 is different than NA!!
totalFgh$expenditure <- 0 


#--------------------------------
#AAAAAAAAAAAAAAAAAAAAAAAAHHHHH
# Why would we do this??? EKL
totalFgh$gf_module <-"all"
totalFgh$module <- "all"
totalFgh$gf_intervention <- "all"
totalFgh$intervention <- "all"
totalFgh$abbrev_module <-"all"
totalFgh$abbrev_intervention <- "all"
#--------------------------------

totalFgh$adm2 <- totalFgh$adm1
totalFgh$grant_number <- "none"
totalFgh$recipient <- "none"
totalFgh$lang <- "eng"

#-----------------------------------
totalFgh$cost_category <- "all" #Why not make this NA? 
#-----------------------------------

totalFgh$coefficient <- 1
totalFgh$country = mapply(get_country_name, totalFgh$loc_name)
totalFgh$loc_name = tolower(totalFgh$loc_name)
totalFgh = totalFgh[!grep("total_", sda_activity)] #This should not happen here!!! EKL Needs to happen above. 

# ----------------------------------------------
# export the FGH data 
# ----------------------------------------------

write.csv(totalFgh,"J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_current_fgh.csv", row.names=FALSE)


