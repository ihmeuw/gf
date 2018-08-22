
rm(list=ls())
library(data.table)

# root directory
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/'
# input file
inFile = paste0(dir, 'total_resource_tracking_data.csv')
# load
data = fread(inFile)

data = data[country == "Uganda"]

fgh_data = data[data_source == "fgh"]
fgh_data = data[fin_data_type == "actual" | fin_data_type == "model_estimates" | fin_data_type == "forecasted_mean"]
fgh_disbursment = fgh_data[,sum(disbursement),by=c('financing_source','year', 'disease')][order(financing_source, year)]

current_fgh = fgh_disbursment[year > 2014 & year < 2018] 
future_fgh = fgh_disbursment[year > 2017 & year < 2021] 

fpm_data = data[data_source == "fpm"]
fpm_budget = fpm_data[,sum(budget),by=c('financing_source','year', "disease")][order(financing_source, year)]

current_fpm = fpm_budget[year > 2014 & year < 2018]
future_fpm = fpm_budget[year > 2017 & year < 2021] 

prep_dah_of_TotalExpend = function(expenditure, time_frame){
  ### Percentage of total health expenditure provided through international development assistance 2015-2017 // 2018-2020
  # (dah + dah_other) / the
  expenditure$disease = NULL
  expenditure$year = NULL
  expenditure = expenditure[, total_disbursed:= sum(V1), by = 'financing_source']
  expenditure$V1 = NULL
  expenditure = unique(expenditure)
  
  numerator = expenditure[financing_source %in% c("dah", "other_dah"), sum(total_disbursed)]
  denominator = sum(expenditure$total_disbursed)
  
  dt = data.table(source = "dah_of_TotalExpend", years = time_frame, value = numerator/denominator, disease = "all")
  
  return(dt)
}

prep_disease_focused_disburse = function(fpm, fgh, disease_name, time_frame){
  ### Percentage of malaria-focused international development assistance provided by The Global Fund 2015-2017 // 2018-2020
  # fpm_budget(gf) / fgh_disbursemnet(bil_usa + other_dah + dah_other)
  
  fpm_vals = fpm[disease == disease_name & financing_source == "gf"]
  numerator = sum(fpm_vals$V1)
  
  fgh_vals = fgh[disease == disease_name]
  fgh_vals$disease = NULL
  fgh_vals$year = NULL
  
  fgh_total = fgh_vals[, total_disbursed:= sum(V1), by = 'financing_source']
  fgh_total$V1 = NULL
  fgh_total= unique(fgh_total)
  
  denominator = fgh_total[financing_source %in% c("bil_usa", "other_dah", "gf", "dah"), sum(total_disbursed)]
  
  dt = data.table(source = paste(disease_name, "focused disbursement"), years = time_frame, value = numerator/denominator, disease = disease_name)
  
  return(dt)
}

total_exp = rbind(prep_dah_of_TotalExpend(current_fgh, "2015-2017"), prep_dah_of_TotalExpend(future_fgh, "2018-2020"))

malaria_disbursment = rbind(prep_disease_focused_disburse(current_fpm, current_fgh, "malaria", "2015 - 2017"), 
                            prep_disease_focused_disburse(future_fpm , future_fgh,"malaria", "2018 - 2020"))

hiv_disbursment = rbind(prep_disease_focused_disburse(current_fpm, current_fgh, "hiv", "2015 - 2017"), 
                            prep_disease_focused_disburse(future_fpm , future_fgh,"hiv", "2018 - 2020"))
tb_disbursment = rbind(prep_disease_focused_disburse(current_fpm, current_fgh, "tb", "2015 - 2017"), 
                            prep_disease_focused_disburse(future_fpm , future_fgh,"tb", "2018 - 2020"))
### Percentage of malaria-focused international development assistance provided by partners 2015-2017 // 2018-2020
malaria_focused_partners = 1 - malaria_focused_disbursment


#Percentage of Global Fund malaria investments allocated for treatment 2015-2017 // 2018-2020
# subset fpm -----------abbrev_intervention / total fpm malaria spending
fpm_disease = fpm_data[disease == "malaria"]
malaria_treatment = subset(fpm_disease, abbrev_intervention == "Facility-based treatment" | abbrev_intervention == "iCCM" |
                                  abbrev_intervention =="Severe malaria"  | abbrev_intervention == "Private sector case management")
malaria_treatment_t2 = subset(fpm_disease, abbrev_intervention == "Facility-based treatment" | abbrev_intervention == "iCCM" |
                             abbrev_intervention =="Severe malaria"  | abbrev_intervention == "Private sector case management" | 
                               abbrev_intervention == "Procurement strategy" | abbrev_intervention == "Nat. product selection")

# 
# # subset
# data = data[country=="Uganda"]
# fgh_data = data[data_source=='fgh' & disease%in%c('all','malaria')]
# 
# # compute disease-percentages from actuals
# all_dah = fgh_data[fin_data_type=='forecasted' & financing_source=='dah', c('disbursement','year')]
# disease_dah = fgh_data[fin_data_type=='actual' & grepl('total', module), .(total=sum(disbursement)), by=c('year','disease')]
# disease_dah = dcast.data.table(disease_dah, year~disease, value.var='total')
# all_dah = merge(all_dah, disease_dah, by='year', all.x=TRUE)
# all_dah[, pct_malaria:=malaria/disbursement]
# 
# # extend the last percentage forward
# last_value = all_dah[!is.na(pct_malaria)][year==max(year)]$pct_malaria
# all_dah[is.na(malaria), malaria:=last_value*disbursement]
# all_dah[is.na(pct_malaria), pct_malaria:=last_value]
# 
# # compute number that's from the global fund
# fpm_data = data[data_source=='fpm' & disease=='malaria']
# fpm_data = fpm_data[, .(gf=sum(budget)), by='year']
# all_dah = merge(all_dah, fpm_data, by='year')
# 
# # aggregate by window
# all_dah[year>=2015 & year<=2017, window:='2015-2017']
# all_dah[year>=2018, window:='2018-2020']
# agg = all_dah[, .(malaria=sum(malaria), gf=sum(gf)), by='window']
# agg[, pct_gf:=gf/malaria]
# agg

# 
