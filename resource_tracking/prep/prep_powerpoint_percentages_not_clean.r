rm(list=ls())
library(data.table)
library(ggplot2)
# root directory
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/'
# input file
inFile = paste0(dir, 'total_resource_tracking_data.csv')

# load
data = fread(inFile)

country_name = "Congo (Democratic Republic)" #"Congo (Democratic Republic)" "Uganda" "Guatemala" 
disease_name = "malaria"  #"hiv"     "malaria" "tb"      "hss"     "all" 

#will fix these laters
if(country_name == "Uganda"){
  data = data[grant_number != "UGA-S-TASO"]
}
if(country_name == "Congo (Democratic Republic)"){
  data = data[grant_number != 'FR798-COD-C']
}

prep_dah_of_TotalExpend = function(expenditure, time_frame){
  ### Percentage of total health expenditure provided through international development assistance 2015-2017 // 2018-2020 {ALL DISEASE}
  # FGH -> (dah + dah_other) / the
  expenditure$disease = NULL
  expenditure$year = NULL
  expenditure = expenditure[, total_disbursed:= sum(V1), by = 'financing_source']
  expenditure$V1 = NULL
  expenditure = unique(expenditure)
  
  numerator = expenditure[financing_source == "dah" |financing_source == "other_dah" | financing_source == "bil_usa" | financing_source == "gf", sum(total_disbursed)]
  denominator = sum(expenditure$total_disbursed)
  
  dt = data.table(source = "dah_of_TotalExpend", gf = numerator, dah_fgh = denominator, years = time_frame, value = numerator/denominator, disease = "all")
  return(dt)
}

prep_disease_focused_disburse = function(fpm, fgh, disease_name, time_frame){
  ### Percentage of malaria-focused international development assistance provided by The Global Fund 2015-2017 // 2018-2020
  # fpm_budget(gf) / fgh_disbursemnet(bil_usa + other_dah + gf)
  
  fpm_vals = fpm[disease == disease_name]
  numerator = sum(fpm_vals$budget)

  fgh_vals = fgh[financing_source %in% c("bil_usa", "other_dah", "gf", 'dah') & disease == disease_name]
 
  denominator = sum(fgh_vals$disbursement)
  
  dt = data.table(source = paste(disease_name, "focused disbursement"), gf = numerator, dah_fgh = denominator, years = time_frame, value = numerator/denominator, disease = disease_name)
  
  return(dt)
}

disease_invest = function(fpm, disease_name,time_frame){
  ### Percentage of Global Fund malaria investments allocated for treatment 2015-2017 // 2018-2020
  # subset fpm -----------abbrev_intervention / total fpm malaria spending
  denominator = sum(fpm$budget)
  
  if(disease_name == "malaria"){
    #treatment = subset(fpm, module == "casemanagement")
    treatment = subset(fpm, gf_module == "Case management")
  }
  if(disease_name == "tb"){
    treatment = subset(fpm, gf_module == "TB care and prevention")
  }
  
  numerator = sum(treatment$budget)
  
  return(data.table(source = paste(disease_name, "investments allocated for", unique(treatment$gf_module)), gf = numerator, dah_fgh = denominator, years = time_frame, value = numerator/denominator, disease = disease_name))
}


#choose country & disease
country_data = data[country == country_name]

#This is a TEMPORARTY FIX
# Find disbursment data with fgh
fgh_data = country_data[data_source == "fgh"]
fgh_data = fgh_data[!grepl("total", fgh_data$module),]

# before 2017, use "actuals" after 2017 use "model_estimates"
actual_fgh = subset(fgh_data, year < 2017)
actual_fgh = actual_fgh[fin_data_type == "actual"]
test_2016 = actual_fgh[year == 2016]

test_2017 = actual_fgh[year == 2016]
test_2017$year = 2017

test_2018 = actual_fgh[year == 2016]
test_2018$year = 2018

test_2019 = actual_fgh[year == 2016]
test_2019$year = 2019

test_2020 = actual_fgh[year == 2016]
test_2020$year = 2020

estimate_fgh = subset(fgh_data, year > 2016)
estimate_fgh = estimate_fgh[fin_data_type == "model_estimates"]

total_fgh_data = unique(rbind(actual_fgh, test_2017, test_2018, test_2019, test_2020))
#total_fgh_disease = total_fgh_data[disease == disease_name]

slideOne = unique(rbind(actual_fgh, estimate_fgh))


fgh_disbursment = slideOne[,sum(disbursement),by=c('financing_source','year', 'disease')]

# Find fpm data
fpm_data = country_data[data_source == "fpm"]
fpm_budget = fpm_data[,sum(budget),by=c('financing_source','year', "disease", "fin_data_type")][order(financing_source, year)]

fpm_disease = fpm_data[disease == disease_name]
future = fpm_disease[year > 2017 & year < 2021]
current = fpm_disease[year > 2014 & year < 2018]
gtm_disease = fpm_disease[year > 2015 & year < 2019]

current_fgh = fgh_disbursment[year > 2014 & year < 2018] 
future_fgh = fgh_disbursment[year > 2017 & year < 2021] 
gtm_fgh = fgh_disbursment[year > 2015 & year < 2019]

current_fpm = fpm_budget[year > 2014 & year < 2018]
future_fpm = fpm_budget[year > 2017 & year < 2021]
gtm_fpm = fpm_budget[year > 2015 & year < 2019]



if(country_name == 'Guatemala'){
  total_exp = prep_dah_of_TotalExpend(gtm_fgh, "2016-2018")
  disease_exp = prep_disease_focused_disburse(fpm_data[year > 2015 & year < 2019], fgh_data[year > 2015 & year < 2019], disease_name, "2016-2018")
  disease_alloc = disease_invest(gtm_disease, disease_name, "2016-2018")
  
}else{
  total_exp = rbind(prep_dah_of_TotalExpend(current_fgh, "2015-2017"), prep_dah_of_TotalExpend(future_fgh, "2018-2020"))
  test = prep_disease_focused_disburse(fpm_data[year == 2017], total_fgh_data[year == 2017], disease_name, "2017")
  disease_exp = rbind(prep_disease_focused_disburse(fpm_data[year > 2014 & year < 2018], total_fgh_data[year > 2014 & year < 2018], disease_name, "2015-2017"), 
                      prep_disease_focused_disburse(fpm_data[year > 2017 & year < 2021], total_fgh_data[year > 2017 & year < 2021], disease_name, "2018-2020"))
  disease_alloc = rbind(disease_invest(current, disease_name, "2015-2017"), disease_invest(future,disease_name, "2018-2020"))
}

total_for_slides = rbind(total_exp, disease_exp, disease_alloc)

#Row 3




#malaria_distrub = rbind(disease_invest(current, disease_name, "2015-2017"), disease_invest(future,disease_name, "2018-2020"))

# let's make a pie chart
# Find fpm data
malaria_data = country_data[data_source == "fpm" & disease == disease_name]
fpm_pie = malaria_data[gf_module == "Case management"]
fpm_intvent = fpm_pie[,total_budget := sum(budget),by=c('year', "gf_intervention")][order(gf_intervention, year)]
fpm_intvent = unique(fpm_intvent[,c("year", "gf_intervention", "total_budget")])
current_fpm = fpm_pie[year > 2014 & year < 2018]
future_fpm = fpm_pie[year > 2017 & year < 2021] 
fpm_intvent = future_fpm[,total_budget := sum(budget),by=c("gf_intervention")][order(gf_intervention)]
fpm_intvent = unique(future_fpm[,c("gf_intervention", "total_budget")])



current_fpm$year = NULL
future_fpm$year = NULL

current_fpm = current_fpm[,sum(total_budget),by=c("gf_intervention")][order(gf_intervention)]
future_fpm = future_fpm[,sum(total_budget),by=c("gf_intervention")][order(gf_intervention)]

write.csv(current_fpm, "J:/temp/ninip/current_malaria_fpm.csv")
write.csv(future_fpm, "J:/temp/ninip/future_malaria_fpm.csv")

bp<- ggplot(current_fpm, aes(x="", y=V1, fill=gf_intervention))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0)

bp_future<- ggplot(future_fpm, aes(x="", y=V1, fill=gf_intervention))+
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(y = V1,label = floor(V1)), size = 5) +
  coord_polar("y") +
  xlab("") +
  ylab("Amount Budgeted (USD)") +
  theme(axis.text.y = element_text(angle=-20)) +
  #labs(fill='Module') +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()


fpm_intvent = fpm_data[,total_budget := sum(budget),by=c('year', "gf_module")][order(gf_module, year)]
fpm_intvent = unique(fpm_intvent[,c("year", "gf_module", "total_budget")])
fpm_intvent_year = fpm_intvent[,total_budget_all := sum(total_budget),by=c('year')]
fpm_intvent_year = unique(fpm_intvent_year[,c("year", 'total_budget_all')])
