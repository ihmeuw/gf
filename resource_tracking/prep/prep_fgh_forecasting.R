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


the_forecast <- data.table(read_feather(paste0(root,"Project/IRH/Forecasting/data/feather_storage_draws_2018/scenarios_he_raked/THE_totes_compile.feather")))
pce_forecast <- the_forecast[iso3%in%c("GTM", "UGA", "COD")&scenario=="reference"]
pce_forecast$scenario <- NULL
pce_forecast$df <- NULL


groupCols <- names(pce_forecast)[!names(pce_forecast)%in%c('year', 'iso3')]


pce_forecast[,mean_draws:=lapply(.SD, mean, na.rm=TRUE),.SDcols=groupCols]
pce_forecast$mean_draws<- unlist(pce_forecast$mean_draws)
pce_forecast[,lower_perc_list:=lapply(.SD, quantile, probs=0.02, na.rm=TRUE, names=FALSE),.SDcols=groupCols]
pce_forecast$lower_perc <- unlist(pce_forecast$lower_perc_list)
pce_forecast[,upper_perc:=sapply(.SD, quantile, probs=0.975, na.rm=TRUE),.SDcols=groupCols]


the_prepped <- pce_forecast[, c("year", "iso3", "mean_draws", "lower_perc", "upper_perc")]

write.csv(the_prepped, paste0(root, 'Project/Evaluation/GF/resource_tracking/multi_country/mapping/the_forecasting_prepped.csv', row.names=FALSE))