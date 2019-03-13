# ----------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chen
# PURPOSE: Prep FGH forecast data.   
# DATE: Last updated March 2019. 
# ----------------------------------------------

# ----------------------------------------------
# Import DAH by disease 
# ----------------------------------------------

fgh_estimates <- fread(paste0(j, 'Project/Evaluation/GF/resource_tracking/multi_country/mapping/prepped_current_fgh.csv'), stringsAsFactors = FALSE)


percent_disbursed = fgh_estimates[,.(year, country, disease, financing_source, disbursement)]
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
output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/')
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
## get the forecasted datasets using the above function: 
# ----------------------------------------------
ghe_prepped <- get_prepped_forecast(root, input_dir,ghe_file)
ghe_prepped$financing_source <- "ghe_forecasted"

dah_prepped <- get_prepped_forecast(root, input_dir, dah_file)
dah_prepped$financing_source <- "dah_forecasted"

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
pce_total$period <- 365

pce_total$fileName = ifelse(pce_total$financing_source == "dah", "DAH.csv", 
                            ifelse(pce_total$financing_source == "ghe", "GHES.csv", 
                                   ifelse(pce_total$financing_source == "ppp", "PPP.csv",
                                          "OOP.csv")))


total_fgh <- rbind(fgh_estimates, pce_total)
total_fgh$loc_name = tolower(total_fgh$loc_name)

# ----------------------------------------------
##export to the J Drive: 
# ----------------------------------------------
write.csv(total_fgh, paste0(root, 'Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_prepped_fgh_total.csv'), row.names=FALSE)

