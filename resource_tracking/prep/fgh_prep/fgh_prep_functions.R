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
  } else if(channel == "AfDB" | channel == "WB_IDA" | channel == "WB_IBRD" | channel == "AsDB" | channel == "IDB" | channel == "UNITAID"){
    x <- "UN agencies, The World Bank and other regional development banks"
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
