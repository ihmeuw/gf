#------------------------------------------------------------------
# PURPOSE: Quick calculation for TERG mixed methods presentation
# DATE: September 5, 2019 
# AUTHOR: Emily Linebarger
#------------------------------------------------------------------

#Set working director to root of gf repository!!
#Want to put the coefficient between cases started on treatment >- treatment success rate into person-terms. 

# so the steps are
# 1. look up the average effect size for Cases Started -> log(Tx Success). Let's assign this to beta1
# 2. aggregate Tx Success (in natural space) to the national level using Cases Started as the weight
# 3. compute National Tx Success*National Population
# 4.  compute exp(beta1*log(National Tx Success))*National Population
# 5. subtract 3 from 4

rm(list=ls())
source('./impact_evaluation/gtm/set_up_r.r')

#Read in latest model object 
load(outputFile5a)
urFits1 = copy(urFits)
urFits1[, se_ratio.std:=se.std/est.std]
urFits1[, se_ratio:=se/est]

#Read in the untransformed data 
load(outputFile4a) 
raw_data = untransformed[, .(department, date, Treatment_Success_Rate_imp, Cases_Started_on_Treatment_out)]

#Collapse down to the mean by department.
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
urFit1 = urFits1[, lapply(.SD, mean, na.rm=TRUE), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit1[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit1[se>abs(se_ratio*est), se:=abs(se_ratio*est)]

#Step 1
beta1 = urFits[lhs=="Treatment_Success_Rate_imp" & rhs=="Cases_Started_on_Treatment_out_cumulative", mean(est)]

# step 2
national_totals = raw_data[, .(Cases_Started_on_Treatment_out=sum(Cases_Started_on_Treatment_out, na.rm=T), 
                               Successful_Treatments=sum(Treatment_Success_Rate_imp*Cases_Started_on_Treatment_out)),by='date']
national_totals[, national_tx_success_rate:=Successful_Treatments/Cases_Started_on_Treatment_out]

#If there are cases with 0, it's because data was missing. Replace these with NA. 
national_totals[national_tx_success_rate==0, national_tx_success_rate:=NA]


#Step 3 - pull in national population - was extracted from the World Bank database at this link: 
# https://data.worldbank.org/indicator/SP.POP.TOTL?end=2018&locations=GT&start=2009

pop = fread("J:/Project/Evaluation/GF/impact_evaluation/gtm/raw_data/worldbank_population.csv")
new_names = c('country', 'country_code', 'indicator_name', 'indicator_code', as.character(seq(1960, 2018, by=1)))
names(pop) = new_names

#Cut out first 5 rows 
pop = pop[6:nrow(pop)]

#Subset down to the data you want 
pop = pop[country_code=="GTM" & indicator_code == "SP.POP.TOTL", c("country", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")]
pop = melt(pop, id.var='country', variable.name='date', value.name='population')
pop[, date:=as.integer(levels(date))]
pop[, population:=as.numeric(population)]

national_totals = merge(national_totals, pop[, .(date, population)], by='date', all=T)

#Step 4 - compute national treatment success rate
national_totals[, num_addl_cases_success_tx:=((beta1*national_tx_success_rate)*Cases_Started_on_Treatment_out)]

