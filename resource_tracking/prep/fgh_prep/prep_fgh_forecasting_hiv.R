

# ----------------------------------------------
# Irena Chen
#
# 6/13/2018
# ### Prep FGH HIV Forecasted Data 

# ----------------------------------------------
##RUN THIS ON THE CLUSTER ### 

# Set up R
rm(list=ls())
library(data.table)
library(feather)

if (Sys.info()[1] == 'Windows') {
  username <- "irenac2"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}


j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/')

hiv_forecast <- data.table(fread(paste0(root, "Project/IRH/HIV/03_model_outputs/forecast/for_gbd_hiv/base_case.csv"))
pce_codes <-  c(128, 190,171)
pce_forecast <- hiv_forecast[location_id%in%pce_codes]
pce_forecast$V1 <- NULL
pce_forecast <- melt(pce_forecast, id.vars = c("location_id", "year_id", "variable"), variable.name = "sda",value.name="funding_forecast")


pce_wide <- reshape(pce_forecast,direction='wide',
             idvar=c("location_id", "yeavarir_id", "sda"),
             timevar="variable")

groupCols <- names(pce_wide)[!names(pce_wide)%in%c("location_id", "year_id", "sda")]

pce_wide[,mean_draws:=lapply(.SD, mean, na.rm=TRUE),.SDcols=groupCols]
pce_wide$mean_draws<- unlist(pce_wide$mean_draws)
pce_wide[,lower_perc_list:=lapply(.SD, quantile, probs=0.02, na.rm=TRUE, names=FALSE),.SDcols=groupCols]
pce_wide$lower_perc <- unlist(pce_wide$lower_perc_list)
pce_wide[,upper_perc:=sapply(.SD, quantile, probs=0.975, na.rm=TRUE),.SDcols=groupCols]

hiv_forecast_prepped <- pce_wide[, c("location_id", "year_id", "sda", "mean_draws", "lower_perc", "upper_perc"), with=FALSE]


write.csv(hiv_forecast_prepped, paste0(root, 'Project/Evaluation/GF/resource_tracking/multi_country/mapping/hiv_forecast_prepped.csv'), row.names=FALSE)