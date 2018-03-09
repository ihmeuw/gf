
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


## load the fgh data

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

fgh_data$source <- mapply(get_source_channel, fgh_data$channel)

# now get the columns we want: 

toMatch <- c("hiv", "mal", "tb", "hss", "year", "source", "country")

drop.cols <- (grep(paste(toMatch, collapse="|"), colnames(fgh_data)))
fghData<- fgh_data[,drop.cols, with=FALSE]

## "melt" the data: 

fghData <- melt(fghData, id=c("year", "source", "country"), variable.name = "sda_orig", value.name="disbursement")


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


fghData$disease <- mapply(get_disease, fghData$sda_orig)

## add in 


fghData[country=='COD', country:='Congo (Democratic Republic)'] 
fghData[country=='GTM', country:='Guatemala'] 
fghData[country=='UGA', country:='Uganda'] 



## something that might be cool is to get the absorption ratio of disb/budget by year, source, disease, etc. 
byVars = names(fghData)[names(fghData)%in%c('source', 'year', 'disease', 'sda_orig', 'country')]
fghData = fghData[, list(disbursement=sum(na.omit(disbursement))), 
                  by=byVars]

fghData$data_source <- "fgh"

write.csv(fghData, "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/fgh_data_prepped.csv", row.names=FALSE)