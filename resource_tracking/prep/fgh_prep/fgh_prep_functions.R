# ----------------------------------------------
# PURPOSE: Functions to help prep Financing Global Health Data. 
# AUTHOR: Emily Linebarger, based off of code from Irena Chen
# DATE: Last updated January 2019
# ----------------------------------------------

get_dah_source_channel <- function(channel){ #EKL do we like these classifications? 
  x <- "Other bilateral assistance"
  if(channel=="GFATM"){
    x <- "The Global Fund"
  } else if(channel=="BIL_USA"){
    x <- "U.S. bilateral assistance"
  } else if(channel == "NGO" | channel == "INTLNGO" | channel == "US_FOUND" | channel == "BMGF"){
    x <- "NGOs and foundations"
  } else if(channel == "AfDB" | channel == "WB_IDA" | channel == "WB_IBRD" | channel == "AsDB" | channel == "IDB" | channel == "UNITAID"
            | channel == "UNFPA" | channel == "UNICEF" | channel == "UNAIDS" | channel == "WHO" | channel == "PAHO"){
    x <- "UN agencies, The World Bank and other regional development banks"
  } else if (channel == "CEPI" | channel == "GAVI"){
    x = "Multilateral organizations (GAVI, CEPI)"
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
  } 
  if (grepl("swap|hss", sda_orig)){
    x <- "hss"
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
## function that takes the aggregated FGH forecasts and
# outputs a dataset that contains the mean, 2%, and 97.5% uncertainty percentiles of estimates 
# ----------------------------------------------
get_prepped_forecast <- function(root,input_dir, fin_type){
  fin_type = "DAH"
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
  pce_prepped$disease <- "all"
  return(pce_prepped)
}

# ----------------------------------------------
## function that takes the HIV forecasts and 
# outputs a dataset that contains the mean, 2%, and 97.5% uncertainty percentiles of estimates 
# ----------------------------------------------

get_hiv_forecast <- function(root, hiv_dir, hiv_file, pce_codes){
  hiv_data <- data.table(fread(paste0(root, hiv_dir, hiv_file)))
  ##subset to the countrains that we want 
  pce_data <- hiv_data[location_id%in%pce_codes]
  
  pce_data$V1 <- NULL #I think someone forgot to remove this variable - it appears to just be a row index #EKL no- this means a melt or reshape somewhere else didn't work correctly. 
  pce_data$ghe = pce_data$public
  
  pce_data_new = subset(pce_data, select = c("location_id", "year_id", "variable", "ghe", "ppp", "oop"))
  
  
  ##create "financing_source" and "funding_forecast" as variables: 
  hiv_forecast <- melt(pce_data_new, id.vars = c("location_id", "year_id", "variable"), 
                       variable.name = "financing_source",value.name="funding_forecast")
  hiv_forecast$funding_forecast <- as.numeric(hiv_forecast$funding_forecast)
  
  ##reshape "wide" so that each draw is a column 
  hiv_wide <- reshape(hiv_forecast,direction='wide',
                      idvar=c("location_id", "year_id", "financing_source"), timevar = "variable")
  
  ## vector of columns excluding the ones we want to "group by" 
  groupCols <- names(hiv_wide)[!names(hiv_wide)%in%c("location_id", "year_id", "financing_source")]
  
  ##calculate the mean, 2% and 97.5% quantiles 
  hiv_wide[,mean:=apply(.SD,1, mean),.SDcols=groupCols]
  hiv_wide[,lower_perc:=apply(.SD, 1, quantile, probs=0.02, na.rm=TRUE),.SDcols=groupCols]
  hiv_wide[,upper_perc:=apply(.SD, 1, quantile, probs=0.975, na.rm=TRUE),.SDcols=groupCols]
  
  ## we only want these five columns in the final dataset: 
  hiv_prepped <- hiv_wide[, c("location_id", "year_id", "financing_source", "mean","lower_perc", "upper_perc")]
  setnames(hiv_prepped, c("location_id", "year_id"), c("adm1", "year"))
  hiv_prepped$disease <- "hiv"
  hiv_prepped$code <- "S98"
  
  return(hiv_prepped)
}
