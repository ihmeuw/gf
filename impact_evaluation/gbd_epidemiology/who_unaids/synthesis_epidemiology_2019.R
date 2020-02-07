# ----------------------------------------------
# Caitlin O'Brien-Carelli 
# Estimates of mortality and incidence from GBD
# Edited by Emily Linebarger 1/21/20 to run on WHO/UNAIDS estimates
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(stringr)
options(scipen=999)
# --------------------
# detect the user 

user = Sys.info()[['user']]

#--------------------------------
# set directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/impact_over_time_gbd/')

code_dir = paste0('C:/Users/', user, '/Documents/gf/impact_evaluation/gbd_epidemiology/')

#--------------------------------
# upload the data sets
dt_ihme = fread(paste0(dir, 'ihme_age_standardized_2017.csv')) # For reference 

unaids_incidence = fread("J:/Project/Evaluation/GF/impact_evaluation/impact_over_time_gbd/unaids/New HIV infections_HIV incidence per 1000 population_Population_ All ages.csv")
unaids_mortality = fread("J:/Project/Evaluation/GF/impact_evaluation/impact_over_time_gbd/unaids/AIDS-related deaths_Number of AIDS-related deaths_Population_ All ages.csv")
unaids_art_coverage = fread("J:/Project/Evaluation/GF/impact_evaluation/impact_over_time_gbd/unaids/Treatment cascade_People living with HIV receiving ART (%)_Population_ All ages.csv")
who_tb = fread("J:/Project/Evaluation/GF/impact_evaluation/impact_over_time_gbd/who_tb/TB_burden_countries_2020-01-21.csv")
who_malaria = readRDS("J:/Project/Evaluation/GF/impact_evaluation/impact_over_time_gbd/who_malaria/estimated_malaria_cases_and_deaths_2010_2018.RDS")
ihme_mal = fread("J:/Project/Evaluation/GF/outcome_measurement/multi_country/coverage/raw_data/anti_malaria_1131.csv")

# subset and rename variables - only all ages included
ihme_mal = ihme_mal[ ,.(indicator = covariate_name_short, country = location_name,
            sex, year = year_id,
            mean = 100*mean_value, lower = 100*lower_value, upper = 100*upper_value)]

# the sex stratified estimates are identical, drop male
ihme_mal = ihme_mal[sex=='Male']
ihme_mal[ ,sex:=NULL]
ihme_mal[ ,indicator:='Effective antimalarial coverage']
ihme_mal[, measure:="Treatment coverage"]
ihme_mal[, metric:="Percentage"]
ihme_mal[, cause:="Malaria"]

setnames(ihme_mal, c('country', 'mean'), c('location', 'val'))

#Format UNAIDS incidence data - is per 1,000 people
names(unaids_incidence) = as.character(unaids_incidence[1, ])
unaids_incidence = unaids_incidence[2:nrow(unaids_incidence), ]

hiv_inc_melt = melt(unaids_incidence, id.vars=c('Country'), value.var='val')
hiv_inc_melt[grepl('lower', variable), category:='lower']
hiv_inc_melt[grepl('upper', variable), category:='upper']
hiv_inc_melt[is.na(category), category:='val']

hiv_inc_melt[, variable:=gsub('_lower', "", variable)]
hiv_inc_melt[, variable:=gsub('_upper', "", variable)]
setnames(hiv_inc_melt, c('variable', 'Country'), c('year', 'location'))

# Need to take out "less-than" and "greater-than" signs and make values numeric. - TO DISCUSS
hiv_inc_melt[, value:=gsub("<|>", "", value)]
hiv_inc_melt[, value:=as.numeric(value)]

hiv_inc = dcast(hiv_inc_melt, location+year~category, value.var='value')
hiv_inc[, measure:="Incidence"]
hiv_inc[, cause:="HIV/AIDS"]
hiv_inc[, metric:="Rate"]

# Make HIV incidence per 100,000 population instead of 1,000 - TO DISCUSS. 
for (var in c('lower', 'upper', 'val')) {
  hiv_inc[, (var):=get(var)*100]
}

#Format UNAIDS mortality data - is per 1,000 people
names(unaids_mortality) = as.character(unaids_mortality[1, ])
unaids_mortality = unaids_mortality[2:nrow(unaids_mortality), ]

hiv_mort_melt = melt(unaids_mortality, id.vars=c('Country'), value.var='val')
hiv_mort_melt[grepl('lower', variable), category:='lower']
hiv_mort_melt[grepl('upper', variable), category:='upper']
hiv_mort_melt[is.na(category), category:='val']

hiv_mort_melt[, variable:=gsub('_lower', "", variable)]
hiv_mort_melt[, variable:=gsub('_upper', "", variable)]
setnames(hiv_mort_melt, c('variable', 'Country'), c('year', 'location'))

# Need to take out "less-than" and "greater-than" signs and make values numeric. - TO DISCUSS
hiv_mort_melt[, value:=gsub("<|>", "", value)]
hiv_mort_melt[, value:=as.numeric(value)]

hiv_mort = dcast(hiv_mort_melt, location+year~category, value.var='value')
hiv_mort[, measure:="Mortality"]
hiv_mort[, cause:="HIV/AIDS"]
hiv_mort[, metric:="Number"]

# Do the same checks for HIV ART coverage data 
names(unaids_art_coverage) = as.character(unaids_art_coverage[1, ])
unaids_art_coverage = unaids_art_coverage[2:nrow(unaids_art_coverage), ]

hiv_art_melt = melt(unaids_art_coverage, id.vars=c('Country'), value.var='val')
hiv_art_melt[grepl('lower', variable), category:='lower']
hiv_art_melt[grepl('upper', variable), category:='upper']
hiv_art_melt[is.na(category), category:='val']

hiv_art_melt[, variable:=gsub('_lower', "", variable)]
hiv_art_melt[, variable:=gsub('_upper', "", variable)]
setnames(hiv_art_melt, c('variable', 'Country'), c('year', 'location'))

# Need to take out "less-than" and "greater-than" signs and make values numeric. - TO DISCUSS
hiv_art_melt[, value:=gsub("<|>", "", value)]
hiv_art_melt[, value:=as.numeric(value)]

hiv_art = dcast(hiv_art_melt, location+year~category, value.var='value')
hiv_art[, measure:="Treatment coverage"]
hiv_art[, metric:="Percentage"]
hiv_art[, cause:="HIV/AIDS"]

# Format WHO TB data - is per 100,000 cases
who_tb = who_tb[, .(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi, e_mort_100k, e_mort_100k_lo, e_mort_100k_hi, c_cdr, c_cdr_lo, c_cdr_hi)]
who_melt = melt(who_tb, id.vars=c('country', 'year'))
who_melt[grepl('inc', variable), measure:="Incidence"]
who_melt[grepl('mort', variable), measure:="Mortality"]
who_melt[grepl('cdr', variable), measure:="Treatment coverage"] # The WHO says that case detection and treatment coverage are the same thing. 
who_melt[grepl('hi', variable), category:='upper']
who_melt[grepl('lo', variable), category:='lower']
who_melt[is.na(category), category:='val']
who_melt$variable <- NULL 

who_tb_new = dcast(who_melt, country+year+measure~category, value.var='value')
who_tb_new[measure%in%c('Incidence', 'Mortality'), metric:="Rate"]
who_tb_new[measure=="Treatment coverage", metric:="Percentage"]
who_tb_new[, cause:="Tuberculosis"]
setnames(who_tb_new, 'country', 'location')

# Format the WHO malaria data 
who_malaria = who_malaria[, .(country, year, incidence_lower, incidence_point, incidence_upper, mortality_lower, mortality_point, mortality_upper)]
who_melt2 = melt(who_malaria, id.vars=c('country', 'year'))
who_melt2[grepl('incidence', variable), measure:="Incidence"]
who_melt2[grepl('mortality', variable), measure:="Mortality"]
who_melt2[grepl('upper', variable), category:='upper']
who_melt2[grepl('lower', variable), category:='lower']
who_melt2[is.na(category), category:='val']
who_melt2$variable <- NULL 

who_mal_new = dcast(who_melt2, country+year+measure~category, value.var='value')
who_mal_new[, metric:="Rate"]
who_mal_new[, cause:="Malaria"]
setnames(who_mal_new, 'country', 'location')

# make these disparate datasets match the format of the GBD data 
dt = rbindlist(list(hiv_inc, hiv_mort, hiv_art, who_tb_new, who_mal_new, ihme_mal), fill=T)
dt = dt[location%in%c("Cambodia", "Democratic Republic of the Congo",
                      "Guatemala", "Mozambique", "Myanmar", "Senegal", "Sudan", "Uganda", "Global")]

# reset the order for facet wrapped graphs
dt$location = factor(dt$location, c("Cambodia", "Democratic Republic of the Congo",
      "Guatemala", "Mozambique", "Myanmar", "Senegal", "Sudan", "Uganda", "Global"), 
      c("Cambodia", "DRC", "Guatemala", "Mozambique", "Myanmar", "Senegal", 
        "Sudan", "Uganda", "Global Trend"))
                        
#-------------------------
# calculate annualized rates of change - 2000 to 2017
#EL 1/21/2020 - we have through 2017 for all of these data sources, so updating this to be 2010-2017
rates = dt[metric=='Rate' & (year==2010 | year==2018),.(measure, location, cause, year, val)]
rates = dcast(rates, measure+location+cause~year)
setnames(rates, c('2010', '2018'), c('y2010', 'y2018'))
rates[ , roc:=round((log(y2018/y2010)/8), 3)]
rates[ ,roc:=roc*100]
rates[ ,c('y2010', 'y2018'):=NULL]

# merge in annualized roc
dt = merge(dt, rates, by=c('measure', 'location', 'cause'), all=T)

# label the locations with associated rates of change
dt[ , label:=paste0(location, ' (', roc, '%)')]

# Add in sex - it's always both, and ages is always 'all ages'
dt[, sex:='Both']
dt[, age:='All Ages']
#-------------------------
# source outside code for tables and figures

source(paste0(code_dir, "who_unaids/trend_figures_synthesis.R"))
#source(paste0(code_dir, "who_unaids/visualize_treatment_coverage.R"))

