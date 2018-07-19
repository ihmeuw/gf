
# ----------------------------------------------
# Irena Chen
#
# 2/22/2018

### This code cleans the FGH csv file and turns it into something that is like our resource tracking database

# ----------------------------------------------
# call libraries 
rm(list=ls())
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(readxl)
library(reshape)
library(scales)

# ----------------------------------------------
## load the fgh DAH data

fgh_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/ihme_dah_cod_uga_gtm_1990_2016.csv",fileEncoding="latin1"))

setnames(fgh_data, c("source", "iso3_rc"), c("dah_origin","country"))

get_source_channel <- function(channel){
  x <- "other_dah"
  if(channel=="GFATM"){
    x <- "gf"
  } else if(channel=="BIL_USA"){
    x <- "bil_usa"
  } else {
    x <- x
  }
  return(x)
}

fgh_data$financing_source <- mapply(get_source_channel, fgh_data$channel)
# now get the columns we want: 

toMatch <- c("hiv", "mal", "tb", "hss", "year", "source", "country")

drop.cols <- (grep(paste(toMatch, collapse="|"), colnames(fgh_data)))
fghData<- fgh_data[,drop.cols, with=FALSE]

## "melt" the data: 

fghData <- melt(fghData, id=c("year", "financing_source", "country"), variable.name = "sda_activity", value.name="disbursement")


get_disease <- function(sda_orig){
  x <- "hss"
  if(grepl("hiv", sda_orig)){
    x <- "hiv"
  } else if(grepl("mal", sda_orig)){
    x <- "malaria"
  } else if (grepl("tb", sda_orig)){
    x <- "tb"
  } else {
    x <- x
  }
  return(x)
}

##get the disease column: 
fghData$disease <- mapply(get_disease, fghData$sda_activity)

## add in 
fghData[country=='COD', country:='Congo (Democratic Republic)'] 
fghData[country=='GTM', country:='Guatemala'] 
fghData[country=='UGA', country:='Uganda'] 

##sum the disbursement by the other variables just to remove any duplicates: 
byVars = names(fghData)[names(fghData)%in%c('source', 'year', 'disease', 'sda_activity', 'country')]
fghData = fghData[, list(disbursement=sum(na.omit(disbursement))), 
                  by=byVars]
fghData$data_source <- "fgh"
fghData$period <- 365
fghData$start_date <- paste0(fghData$year, "-01-01")
fghData$start_date  <- as.Date(fghData$start_date,"%Y-%m-%d")
fghData$budget <- 0 
fghData$expenditure <- 0 
fghData$gf_module <- fghData$sda_activity
fghData$module <- fghData$sda_activity
fghData$gf_intervention <- fghData$gf_module
fghData$intervention <- fghData$gf_intervention



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
fghData$loc_name <- mapply(get_loc_id, fghData$country)



write.csv(fghData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/fgh_data_prepped.csv", row.names=FALSE)

# ----------------------------------------------
#load the GHE data: 

ghe_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/gf/gpr_corrected_final_gbd4.csv",fileEncoding="latin1"))

##country codes for GTM, UGA, and DRC
country_codes <- c(128, 190,171)

ghe_data <- ghe_data[grepl(paste0(country_codes, collapse="|"), ghe_data$location_id),]

value_codes <- unique(ghe_data$value_code)

ghe_data$model <- NULL
ghe_wide <- reshape(ghe_data,direction='wide',
                    idvar=c("location_id", "year_id","hiv_pop"),
                   timevar="value_code")

## (THE - OOP+PPP+GHE = DAH (in theory))

oop_vars <- names(ghe_wide)[grepl(c("oop"), names(ghe_wide))]
ppp_vars <- names(ghe_wide)[grepl(c("ppp"), names(ghe_wide))]
ghe_vars <- names(ghe_wide)[grepl(c("public"), names(ghe_wide))]

## sum the average values of OOP+PPP+GHE
ghe_wide$mean_oop_ppp_ghe_agg <- (ghe_wide$ensemble_mean.fs_hiv_domestic_private_oop+
                                    ghe_wide$ensemble_mean.fs_hiv_domestic_private_ppp + 
                                    ghe_wide$ensemble_mean.fs_hiv_domestic_public)
  
#lower estimates
ghe_wide$lower_oop_ppp_ghe_agg <- (ghe_wide$ensemble_lower.fs_hiv_domestic_private_oop+
                                     ghe_wide$ensemble_lower.fs_hiv_domestic_private_ppp + 
                                     ghe_wide$ensemble_lower.fs_hiv_domestic_public)

##upper estimates
ghe_wide$upper_oop_ppp_ghe_agg <- (ghe_wide$ensemble_upper.fs_hiv_domestic_private_oop+
                                     ghe_wide$ensemble_upper.fs_hiv_domestic_private_ppp + 
                                     ghe_wide$ensemble_upper.fs_hiv_domestic_public)

##subtract OOP+PPP+GHE from THE to get DAH: 
ghe_wide$mean_dah <- (ghe_wide$ensemble_mean.the_hiv - ghe_wide$mean_oop_ppp_ghe_agg)
ghe_wide$lower_dah <- (ghe_wide$ensemble_lower.func_hiv_prev - ghe_wide$lower_oop_ppp_ghe_agg)
ghe_wide$upper_dah <- (ghe_wide$ensemble_upper.the_hiv - ghe_wide$upper_oop_ppp_ghe_agg)




ghe_cleaned <- melt(ghe_wide, id.vars = c("location_id","year_id", "hiv_pop"),
                    variable.name = "fin_data_type", value="disbursement")

ghe_cleaned$disease <- "hiv"

##assign country names based on their ISO codes: 
ghe_cleaned[location_id==128, country:="Guatemala"]
ghe_cleaned[location_id==190, country:="Uganda"]
ghe_cleaned[location_id==171, country:="Congo (Democratic Republic)"]


## rbind the DAH and the forecasted HIV THE: 






write.csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/ghe_fgh_prepped.csv", row.names=FALSE)

