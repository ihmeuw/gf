# ----------------------------------------------------------------------------
# AUTHOR: Emily Linebarger, based on code by Irena Chen
# PURPOSE: Prep FGH estimates data. This raw data contains estimates for 
# Government Health Expenditure (GHE), Out-of-Pocket Spending (OOP), Public-Private
# Partnerships (PPP), and Development Assistance for Health (DAH). From all of these, 
# Total Health Expenditure (THE) can theoretically be calculated. 
# DATE: Last updated March 2019. 
# ----------------------------------------------------------------------------

# ----------------------------------------------
# Read in the raw FGH estimates 
# ----------------------------------------------
fgh_estimates <- fread(paste0(fgh_raw, "gpr_corrected_final_gbd4.csv"))
final_mapping = data.table(read.csv(paste0(mapping_dir, "all_interventions.csv")))
final_mapping = unique(final_mapping[, .(module_eng, intervention_eng, code)])


#Subset to our countries. 
fgh_estimates <- fgh_estimates[location_id %in%code_lookup_tables$ihme_country_code]
fgh_estimates = fgh_estimates[, .(location_id, year_id, value_code, ensemble_mean, ensemble_lower, ensemble_upper)] #Drop 'model' and 'hiv_pop' variables

setnames(fgh_estimates, c("location_id", "year_id"), c("adm1", "year")) ##POSSIBLY HERE
fgh_estimates_wide <- reshape(fgh_estimates,direction='wide',
                    idvar=c("adm1", "year"),
                    timevar="value_code")

## (THE - OOP+PPP+GHE = DAH (in theory)) #EKL this seems questionable to me- are we depending on this? 4/1/19

oop_vars <- names(fgh_estimates_wide)[grepl(c("oop"), names(fgh_estimates_wide))]
ppp_vars <- names(fgh_estimates_wide)[grepl(c("ppp"), names(fgh_estimates_wide))]
ghe_vars <- names(fgh_estimates_wide)[grepl(c("public"), names(fgh_estimates_wide))]


##only take the variables that we want: 
fgh_estimates_cleaned <- fgh_estimates_wide[, c("adm1", "year", 
                            "ensemble_mean.fs_hiv_domestic_private_oop","ensemble_lower.fs_hiv_domestic_private_oop", "ensemble_upper.fs_hiv_domestic_private_oop",
                            "ensemble_mean.fs_hiv_domestic_private_ppp",  "ensemble_lower.fs_hiv_domestic_private_ppp", "ensemble_upper.fs_hiv_domestic_private_ppp",
                            "ensemble_mean.fs_hiv_domestic_public",  "ensemble_lower.fs_hiv_domestic_public", "ensemble_upper.fs_hiv_domestic_public",
                            "ensemble_upper.the_hiv", "ensemble_lower.the_hiv", "ensemble_mean.the_hiv"), with=FALSE]

#reshape "long" so that all of the dah/the/other estimates are in 1 column: 
fgh_estimates_cleaned <- melt(fgh_estimates_cleaned, id.vars = c("adm1", "year"),
                    variable.name = "fin_data_type", value="disbursement")

# Add code. Will be 'unspecified' for all. 
fgh_estimates_cleaned$code <- 'H99' #Is everything HIV?? Emily 

#Map to final mapping 
fgh_estimates_mapped <- merge(fgh_estimates_cleaned, final_mapping, by='code', all.x = TRUE)
stopifnot(nrow(fgh_estimates_mapped[is.na(module_eng)])==0)

##assign loc_name id based on their ISO codes:
for (i in 1:nrow(code_lookup_tables)){
  fgh_estimates_mapped[adm1==code_lookup_tables$ihme_country_code[[i]], loc_name:=code_lookup_tables$iso_code[[i]]]
}
stopifnot(nrow(fgh_estimates_mapped[is.na(loc_name)])==0)

fgh_estimates_mapped$financing_source <- mapply(get_the_source_channel, as.character(fgh_estimates_mapped$fin_data_type))
fgh_estimates_mapped = fgh_estimates_mapped[financing_source != "the"]
fgh_estimates_mapped$fileName <- "gpr_corrected_final_gbd4.csv"

fgh_estimates_mapped$fin_data_type <- mapply(transform_fin_data_type, as.character(fgh_estimates_mapped$fin_data_type)) #CHECK FOR ESTIMATES VS MODELS

# ----------------------------------------------
# Import DAH by disease
# ----------------------------------------------

# fgh_actuals <- fread(paste0(mapping_dir, 'prepped_current_fgh.csv'), stringsAsFactors = FALSE)
# fgh_actuals = fgh_actuals[!fin_data_type == 'actual']
# 
# percent_disbursed = fgh_actuals[,.(year, country, disease, financing_source, disbursement)]
# percent_disbursed = percent_disbursed[financing_source %in% c("bil_usa", "other_dah", "gf")]
# 
# percent_disbursed$financing_source = NULL
# percent_disbursed = unique(percent_disbursed)
# 
# percent_disbursed = percent_disbursed[, numerator := sum(disbursement), by = c('year', 'country', 'disease')]
# percent_disbursed = percent_disbursed[, denominator:= sum(disbursement), by = c('year', 'country')]
# percent_disbursed$disbursement = NULL
# percent_disbursed = unique(percent_disbursed)
# percent_disbursed =  percent_disbursed[, disbursed_weight := numerator/denominator, by = c('year', 'country', 'disease')]
# percent_disbursed$numerator = NULL
# percent_disbursed$denominator = NULL
# percent_disbursed$financing_source = "dah"
# dah_weight = percent_disbursed[year == 2016]
# dah_weight$year = NULL

# 
# # continue
# output_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/')
# input_dir <- paste0("Project/IRH/Forecasting/data/feather_storage_draws_2018/scenarios_he_raked/")
# hiv_dir <- paste0("Project/IRH/HIV/03_model_outputs/forecast/for_gbd_hiv/")
# 
# ##names of the files to be cleaned
# ghe_file <- "GHES"
# the_file <- "THE"
# dah_file <- "DAH"
# hiv_file <- "base_case.csv"
# oop_file <- "OOP"
# ppp_file <- "PPP"

# ----------------------------------------------
## get the forecasted datasets using the above function:
# ----------------------------------------------
# ghe_prepped <- get_prepped_forecast(j, input_dir, ghe_file)
# ghe_prepped$financing_source <- "ghe_forecasted"
# 
# dah_prepped <- get_prepped_forecast(j, input_dir, dah_file)
# dah_prepped$financing_source <- "dah_forecasted"
# 
# pce_forecast <- rbind(ghe_prepped, dah_prepped)
# setnames(pce_forecast, "iso3", "loc_name")
# # pce_forecast$adm1 <- mapply(get_country_codes, tolower(pce_forecast$loc_name), "all")
# 
# hiv_prepped <- get_hiv_forecast(j, hiv_dir, hiv_file, pce_codes)
# hiv_prepped$loc_name <- mapply(get_country_names, hiv_prepped$adm1,"all")
# 
# pce_total <- rbind(hiv_prepped, pce_forecast)
# pce_total  <- melt(pce_total , id.vars = c("year", "adm1", "loc_name", "disease",
#                                            "financing_source", "code"), variable.name = "fin_data_type", value.name = "disbursement")
# 
# pce_total$financing_source = ifelse(pce_total$financing_source == "ghe_forecasted", "ghe", as.character(pce_total$financing_source))
# pce_total$financing_source = ifelse(pce_total$financing_source == "dah_forecasted", "dah", as.character(pce_total$financing_source))
# 
# pce_total$fin_data_type = ifelse(pce_total$fin_data_type == "mean", "model_estimates", as.character(pce_total$fin_data_type))
# pce_total$fin_data_type = ifelse(pce_total$fin_data_type == "lower_perc", "model_estimates_lower_ci", as.character(pce_total$fin_data_type))
# pce_total$fin_data_type = ifelse(pce_total$fin_data_type == "upper_perc", "model_estimates_upper_ci", as.character(pce_total$fin_data_type))
# 
# pce_total$country <- mapply(get_country, tolower(pce_total$loc_name))
# 
# # split up DAH by disease based on weight from year 2017
# dah_financ_source = pce_total[financing_source == "dah"]
# merge_weighted_average = function(dt_dah, dt_weighted, disease){
#   dt_dah$disease = disease
#   dt_merged = merge(dt_dah, dt_weighted, by = c("country", "financing_source", "disease"), all.x = TRUE, all.y = FALSE)
#   dt_merged$disbursement = dt_merged$disbursed_weight * dt_merged$disbursement
#   dt_merged$disbursed_weight = NULL
#   return(dt_merged)
# }
# 
# dah_malaria = merge_weighted_average(dah_financ_source, dah_weight, "malaria")
# dah_tb = merge_weighted_average(dah_financ_source, dah_weight, "tb")
# dah_hiv = merge_weighted_average(dah_financ_source, dah_weight, "hiv")
# dah_hss = merge_weighted_average(dah_financ_source, dah_weight, "hss")
# dah_other = merge_weighted_average(dah_financ_source, dah_weight, "other")
# 
# pce_dah = rbind(dah_malaria, dah_tb, dah_hiv, dah_hss, dah_other)
# pce_not_dah = pce_total[financing_source != "dah"]
# pce_total = rbind(pce_dah, pce_not_dah)
# 
# # ----------------------------------------------
# ##add in RT variables so we can join this data to the RT database
# # ----------------------------------------------
# 
# pce_total$fileName = ifelse(pce_total$financing_source == "dah", "DAH.csv",
#                             ifelse(pce_total$financing_source == "ghe", "GHES.csv",
#                                    ifelse(pce_total$financing_source == "ppp", "PPP.csv",
#                                           "OOP.csv")))
# 
# 
# total_fgh <- rbind(fgh_estimates, pce_total) #THIS NEVER TOOK INTO ACCOUNT THE MAPPING DONE ABOVE?? EMILY 6/19/19

#Pausing this binding process for the moment to focus on FGH estimates code.
total_fgh = copy(fgh_estimates_mapped)
total_fgh$loc_name = tolower(total_fgh$loc_name)

# ----------------------------------------------
##export to the J Drive: 
# ----------------------------------------------
saveRDS(fgh_estimates_mapped, paste0(fgh_prepped, "prepped_fgh_estimates.rds"))
write.csv(fgh_estimates_mapped, paste0(fgh_prepped, 'prepped_fgh_estimates.csv'), row.names=FALSE)

