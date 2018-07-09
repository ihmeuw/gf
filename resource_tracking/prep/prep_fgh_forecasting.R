
# ----------------------------------------------
# Irena Chen
#
# 6/2/2018
## code to integrate the FGH forecasted DAH, THE and GHE data 
##  NOTE: this is not disease specific - only by country and year 

###  RUN THIS ON THE CLUSTER 

# ----------------------------------------------

# Set up R
rm(list=ls())
library(data.table)
library(feather)

##set up to read files from the J Drive 
if (Sys.info()[1] == 'Windows') {
  username <- "irenac2"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}


##set the directory for where to write the prepped files 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/')

input_dir <- "Project/IRH/Forecasting/data/feather_storage_draws_2018/scenarios_he_raked/"


##names of the files to be cleaned 
ghe_file <- "GHES"
the_file <- "THE"
dah_file <- "DAH"

## function that takes the raw CSV and outputs a cleaned dataset with the mean, 2%, and 97.5% uncertainty percentiles 
get_prepped_forecast <- function(root,input_dir, fin_type){
  ghe_forecast <- data.table(read_feather(paste0(root, input_dir, fin_type,"_totes_compile.feather")))
  pce_forecast <- ghe_forecast[iso3%in%c("GTM", "UGA", "COD")&scenario=="reference"]
  pce_forecast$scenario <- NULL
  pce_forecast$df <- NULL
  
  ## get the columns that we'll be taking the mean & percentiles from: 
  groupCols <- names(pce_forecast)[!names(pce_forecast)%in%c('year', 'iso3')]
  
  ##mean of the 1000 draws, row-wise 
  #pce_forecast[,mean_draws:=rowMeans(.SD, na.rm=TRUE),.SDcols=groupCols]
  
  pce_forecast[, mean:=apply(.SD, 1, mean), .SDcols=paste0('draw_', c(1:1000))]
  ## 2% percentile of the draws, row-wise
  pce_forecast[,lower_perc:=apply(.SD,1, quantile, probs=0.02, na.rm=TRUE, names=FALSE),.SDcols=paste0('draw_', c(1:1000))]
  ## 97.5% percentile of the draws, row-wise
  pce_forecast[,upper_perc:=apply(.SD, 1, quantile, probs=0.975, na.rm=TRUE),.SDcols=paste0('draw_', c(1:1000))]
  
## we only want these five columns in the final dataset: 
  pce_prepped <- pce_forecast[, c("year", "iso3", "mean", "lower_perc", "upper_perc")]
  if(fin_type=="DAH"){
    pce_prepped$code <- "S99"
  } else {
    pce_prepped$code <- "S98"
  }

  return(pce_prepped)
}


ghe_prepped <- get_prepped_forecast(root, input_dir,ghe_file)
ghe_prepped$financing_source <- "ghe_forecasted"
the_prepped <- get_prepped_forecast(root, input_dir,the_file)
the_prepped$financing_source <- "the_forecasted"
dah_prepped <- get_prepped_forecast(root, input_dir, dah_file)
dah_prepped$financing_source <- "dah_forecasted"

pce_forecast <- rbind(ghe_prepped, the_prepped, dah_prepped)

pce_forecast <- melt(pce_forecast, id.vars = c("year", "iso3", "financing_source", "coefficient"), variable.name = "fin_data_type", value.name = "disbursement")
pce_forecast$data_source <- "fgh"




setnames(pce_forecast, "iso3", "adm1")
pce_forecast$adm2 <- pce_forecast$adm1
pce_forecast$start_date <- paste0(pce_forecast$year, "-01-01")
pce_forecast$period <- 365
pce_forecast$end_date <- paste0(pce_forecast$year, "-12-31") 
pce_forecast$module <- "all"
pce_forecast$intervention <- "all"
pce_forecast$coefficient <- 1
pce_forecast$gf_module <- "all"
pce_forecast$gf_intervention <- "all"
pce_forecast$abbrev_module <- "all"
pce_forecast$abbrev_intervention <- "all"
pce_forecast$disease <- "all"
pce_forecast$budget <- 0
pce_forecast$expenditure <- 0 
pce_forecast$lang <- "eng"
pce_forecast$cost_category <- "all"
pce_forecast$sda_activity <- "all"
pce_forecast$recipient <- pce_forecast$adm2
pce_forecast$grant_number <- pce_forecast$adm2


write.csv(pce_forecast, paste0(root, 'Project/Evaluation/GF/resource_tracking/multi_country/mapping/fgh_forecasting_prepped.csv'), row.names=FALSE)


