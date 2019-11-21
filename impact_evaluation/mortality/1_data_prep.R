# Francisco and Audrey
# data prep for Drivers of mortality analysis plan
# August 8, 2019

# set up
library(data.table)
library(ggplot2)

j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, "/Project/Evaluation/GF/impact_evaluation/mortality/raw_gbd_estimates/")

set_disease = 'tb' # change to 'tb' to do TB

inFile = ifelse(set_disease == 'tb', paste0(dir, "gbd_estimates_tb_pce_countries.csv"), paste0(dir, "gbd_estimates_malaria_pce_countries.csv"))
outFile =  ifelse(set_disease == 'tb', paste0(dir, "../prepped_data/tb_pce_data.RDS"), paste0(dir, "../prepped_data/malaria_pce_data.RDS"))

# read data
dt = fread(inFile)

# subset data to include:
if ( length(unique(dt$metric_name))!= 1 & unique(dt$metric_name) != "Rate" ) dt = dt[metric_name=="Rate"]
if ( length(unique(dt$age_group_name))!= 1 & unique(dt$age_group_name) != "Age-standardized" ) dt = dt[age_group_name=="Age-standardized"]

# keep columns of interest
dt = dt[,.(year_id, location_name, cause_name, measure_name, val, lower, upper)]
names(dt) = c('year', 'country', 'disease', 'measure', 'val', 'lower', 'upper')

# sum across TB types
if (set_disease == 'tb'){
  dt = dt[, .(val = sum(val), lower=sum(lower), upper=sum(upper)), by = .(year, country, measure)]
  dt[, disease := "tb_all_types"]
}

# reshape the data to be wide
data = dcast(dt, year + country + disease ~ measure, value.var = c("val",'lower','upper'))
setnames(data, c('val_Deaths','val_Incidence'), c('Deaths','Incidence'))

# remove 2018 because right now 2018 not avail (it's just NA)
data = data[!is.na(Deaths)]

#  compute MI ration
data[, mi_ratio := Deaths/Incidence]

# save data for further analysis
saveRDS(data, outFile)


# combine both data sources after prep:
dt1 =  readRDS(paste0(dir, "../prepped_data/tb_pce_data.RDS"))
dt2 =  readRDS(paste0(dir, "../prepped_data/malaria_pce_data.RDS"))
dt = rbindlist(list(dt1, dt2), use.names = TRUE)

# change country names to standard format
dt[grepl(country, pattern = 'guatemala', ignore.case = TRUE), country := 'gtm']
dt[grepl(country, pattern = 'congo', ignore.case = TRUE), country := 'cod']
dt[grepl(country, pattern = 'senegal', ignore.case = TRUE), country := 'sen']
dt[grepl(country, pattern = 'uganda', ignore.case = TRUE), country := 'uga']

# change disease names
dt[disease == 'tb_all_types', disease := 'tb']
dt[, disease:=tolower(disease)]

saveRDS(dt, paste0(dir, '../prepped_data/tb_malaria_pce_countries_data.RDS'))
write.csv(dt, paste0(dir, '../prepped_data/tb_malaria_pce_countries_data.csv'), row.names=FALSE)
