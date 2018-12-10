
# ----------------------------------------------
# Irena Chen
#
# 6/2/2018
## code to integrate the FGH forecasted DAH, THE and GHE data 
##  NOTE: this is not disease specific - only by country and year 

### Caution: RUN THIS ON THE CLUSTER - the files are very big 

# ----------------------------------------------


# Set up R
rm(list=ls())
library(data.table)
library(feather)

pce_codes <-  c(128, 190,171) ##GTM, UGA, COD ISO country codes 


# ----------------------------------------------
##set the directory for where to write the prepped files
# ---------------------------------------------- 

if (Sys.info()[1] == 'Windows') {
  username <- "username"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}


# ----------------------------------------------
## import DAH by disease 
# ----------------------------------------------

fgh_current <- data.table(read.csv(paste0(root, 'Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_current_fgh.csv'), stringsAsFactors = FALSE))


percent_disbursed = fgh_current[,.(year, country, disease, financing_source, disbursement)]
percent_disbursed = percent_disbursed[financing_source %in% c("bil_usa", "other_dah", "gf")]

percent_disbursed$financing_source = NULL
percent_disbursed = unique(percent_disbursed)
  
percent_disbursed = percent_disbursed[, numerator := sum(disbursement), by = c('year', 'country', 'disease')]
percent_disbursed = percent_disbursed[, denominator:= sum(disbursement), by = c('year', 'country')]
percent_disbursed$disbursement = NULL
percent_disbursed = unique(percent_disbursed)
percent_disbursed =  percent_disbursed[, disbursed_weight := numerator/denominator, by = c('year', 'country', 'disease')]
percent_disbursed$numerator = NULL
percent_disbursed$denominator = NULL
percent_disbursed$financing_source = "dah"
dah_weight = percent_disbursed[year == 2016] 
dah_weight$year = NULL


# continue
output_dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/')
input_dir <- paste0("Project/IRH/Forecasting/data/feather_storage_draws_2018/scenarios_he_raked/")
hiv_dir <- paste0("Project/IRH/HIV/03_model_outputs/forecast/for_gbd_hiv/")

##names of the files to be cleaned 
ghe_file <- "GHES"
the_file <- "THE"
dah_file <- "DAH"
hiv_file <- "base_case.csv"
oop_file <- "OOP"
ppp_file <- "PPP"


# ----------------------------------------------
## function that assigns country codes to names 
# ----------------------------------------------

get_country_names <- function(code, adm1){
  x <- adm1
  if(code==128){
    x <- "gtm"
  } else if(code==171){
    x <- "cod"
  } else if (code==190){
    x <- "uga"
  } else {
    x <- x
  }
  return(x)
}
## reverse of the above function
get_country_codes <- function(loc_name, code){
  x <- code
  if(loc_name=="gtm"){
    x <- 128
  } else if(loc_name=="cod"){
    x <- 171
  } else if (loc_name=="uga"){
    x <-190
  } else {
    x <- x
  }
  return(x)
}

## get the full name of the country 
get_country <- function(loc_name){
  x <- loc_name
  if(grepl("gtm", loc_name)){
    x <-  "Guatemala"
  } else if(grepl("cod", loc_name)){
    x <- "Congo (Democratic Republic)"
  } else if(grepl("uga", loc_name)){
    x <- "Uganda"
  }else {
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

# ----------------------------------------------
## get the forecasted datasets using the above function: 
# ----------------------------------------------
ghe_prepped <- get_prepped_forecast(root, input_dir,ghe_file)
ghe_prepped$financing_source <- "ghe_forecasted"
#the_prepped <- get_prepped_forecast(root, input_dir,the_file)
# the_prepped$financing_source <- "the_forecasted"
dah_prepped <- get_prepped_forecast(root, input_dir, dah_file)
dah_prepped$financing_source <- "dah_forecasted"
# oop_prepped <- get_prepped_forecast(root, input_dir, oop_file)
# oop_prepped$financing_source <- "oop_forecasted"
# ppp_prepped <- get_prepped_forecast(root, input_dir, ppp_file)
# ppp_prepped$financing_source <- "ppp_forecasted"

#pce_forecast <- rbind(ghe_prepped, dah_prepped, oop_prepped, ppp_prepped) #, the_prepped
pce_forecast <- rbind(ghe_prepped, dah_prepped)
setnames(pce_forecast, "iso3", "loc_name")
pce_forecast$adm1 <- mapply(get_country_codes, tolower(pce_forecast$loc_name), "all")

hiv_prepped <- get_hiv_forecast(root, hiv_dir, hiv_file, pce_codes)
hiv_prepped$loc_name <- mapply(get_country_names, hiv_prepped$adm1,"all")


pce_total <- rbind(hiv_prepped, pce_forecast)


pce_total  <- melt(pce_total , id.vars = c("year", "adm1", "loc_name", "disease",
                                           "financing_source", "code"), variable.name = "fin_data_type", value.name = "disbursement")
 


pce_total$financing_source = ifelse(pce_total$financing_source == "ghe_forecasted", "ghe", as.character(pce_total$financing_source))
pce_total$financing_source = ifelse(pce_total$financing_source == "dah_forecasted", "dah", as.character(pce_total$financing_source))
# pce_total$financing_source = ifelse(pce_total$financing_source == "oop_forecasted", "oop", as.character(pce_total$financing_source))
# pce_total$financing_source = ifelse(pce_total$financing_source == "ppp_forecasted", "ppp", as.character(pce_total$financing_source))

#pce_total$fin_data_type = ifelse(pce_total$financing_source == "the_forecasted", "forecasted", as.character(pce_total$fin_data_type))
#pce_total$financing_source = ifelse(pce_total$financing_source == "the_forecasted", "the", as.character(pce_total$financing_source))
pce_total$fin_data_type = ifelse(pce_total$fin_data_type == "mean", "model_estimates", as.character(pce_total$fin_data_type))
pce_total$fin_data_type = ifelse(pce_total$fin_data_type == "lower_perc", "model_estimates_lower_ci", as.character(pce_total$fin_data_type))
pce_total$fin_data_type = ifelse(pce_total$fin_data_type == "upper_perc", "model_estimates_upper_ci", as.character(pce_total$fin_data_type))

pce_total$country <- mapply(get_country, tolower(pce_total$loc_name))

# split up DAH by disease based on weight from year 2017
dah_financ_source = pce_total[financing_source == "dah"]
merge_weighted_average = function(dt_dah, dt_weighted, disease){
  dt_dah$disease = disease
  dt_merged = merge(dt_dah, dt_weighted, by = c("country", "financing_source", "disease"), all.x = TRUE, all.y = FALSE)
  dt_merged$disbursement = dt_merged$disbursed_weight * dt_merged$disbursement
  dt_merged$disbursed_weight = NULL
  return(dt_merged)
}

dah_malaria = merge_weighted_average(dah_financ_source, dah_weight, "malaria")
dah_tb = merge_weighted_average(dah_financ_source, dah_weight, "tb")
dah_hiv = merge_weighted_average(dah_financ_source, dah_weight, "hiv")
dah_hss = merge_weighted_average(dah_financ_source, dah_weight, "hss")
dah_other = merge_weighted_average(dah_financ_source, dah_weight, "other")

pce_dah = rbind(dah_malaria, dah_tb, dah_hiv, dah_hss, dah_other)
pce_not_dah = pce_total[financing_source != "dah"]
pce_total = rbind(pce_dah, pce_not_dah)


pce_total$data_source <- "fgh"


# ----------------------------------------------
##add in RT variables so we can join this data to the RT database
# ----------------------------------------------


pce_total$adm2 <- pce_total$adm1
pce_total$start_date <- paste0(pce_total$year, "-01-01")
pce_total$period <- 365
pce_total$end_date <- paste0(pce_total$year, "-12-31") 

# Why are we doing this??? EKL
# -----------------------------
pce_total$module <- "all"
pce_total$intervention <- "all"
pce_total$coefficient <- 1
pce_total$gf_module <- "all"
pce_total$gf_intervention <- "all"
pce_total$abbrev_module <- "all"
pce_total$abbrev_intervention <- "all"
# -----------------------------

#pce_total$disease <- "all"
pce_total$budget <- 0
pce_total$expenditure <- 0 
pce_total$lang <- "eng"
pce_total$cost_category <- "all"
pce_total$sda_activity <- "Unspecified (Summary budget)"
pce_total$recipient <- pce_total$adm2
pce_total$grant_number <- "none"


pce_total$fileName = ifelse(pce_total$financing_source == "dah", "DAH.csv", 
                            ifelse(pce_total$financing_source == "ghe", "GHES.csv", 
                                   ifelse(pce_total$financing_source == "ppp", "PPP.csv",
                                          "OOP.csv")))


total_fgh <- rbind(fgh_current, pce_total)
total_fgh$loc_name = tolower(total_fgh$loc_name)

# ----------------------------------------------
##export to the J Drive: 
# ----------------------------------------------
write.csv(total_fgh, paste0(root, 'Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_prepped_fgh_total.csv'), row.names=FALSE)

