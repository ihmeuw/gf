# ----------------------------------------------
# Caitlin O'Brien-Carelli 
# Prep incidence and mortality data from WHO/UNAIDS
# Set function arguments to determine which data sources
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
dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/synthesis_epidemiology/')

code_dir = paste0('C:/Users/', user, '/local/gf/impact_evaluation/gbd_epidemiology/')

outDir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/synthesis_epidemiology/')

#-----------------------------------------------------------------
# read in who tb 
tb = fread(paste0(dir, 'raw_data/who_tb/TB_burden_countries_2020-01-21.csv'))

#---------------------------------
# before subsetting, get a global population estimate
global_pop = tb[ ,.(population = sum(e_pop_num)), by=year]
global_pop[ ,location:='Global'] # use for hiv estimates

#---------------------------------
# calculate global incidence and mortality for tb 
# merge in later

# sum everything to the global level, shape long and subset
glob_tb = tb[ ,lapply(.SD, sum, na.rm=T), .SDcols=7:50, by=year]
glob_tb = melt(glob_tb, id.vars='year')

keep_glob =  c('year', 'e_pop_num', 'e_inc_num', 'e_inc_num_hi', 'e_inc_num_lo',
               'e_mort_num', 'e_mort_num_hi', 'e_mort_num_lo')
glob_tb = glob_tb[variable %in% keep_glob]

# shape wide and calculate
glob_tb = dcast(glob_tb, year~variable)
glob_tb[ ,pop_factor:=e_pop_num/100000]
glob_tb_rates = glob_tb[ ,lapply(.SD, function(x) x = x/pop_factor), .SDcols=3:8, by=.(year, pop_factor)]
glob_tb_rates[ ,pop_factor:=NULL]
glob_tb[ ,c('pop_factor', 'e_pop_num'):=NULL]

# keep mortality and incidence rates and counts
setnames(glob_tb_rates, c("year", "e_inc_num", "e_inc_num_lo",
            "e_inc_num_hi", "e_mort_num", "e_mort_num_lo", "e_mort_num_hi"), 
           c('year', 'e_inc_100k', 'e_inc_100k_lo', 'e_inc_100k_hi',
            'e_mort_100k', 'e_mort_100k_lo', 'e_mort_100k_hi'))

glob_tb = merge(glob_tb, glob_tb_rates)
glob_tb = melt(glob_tb, id.vars = 'year')
glob_tb[ ,location:='Global']
#--------------------------------

# subset and rename variables
countries = c("Cambodia", "Democratic Republic of the Congo",
  "Guatemala", "Mozambique", "Myanmar", "Senegal", "Sudan", "Uganda", "Global")

# subset to relevant countries, does not include global estimates
tb = tb[country %in% countries]

# --------------------------------------
# extract population data and use later to calculate hiv mortality rates

pop = tb[ ,.(population = e_pop_num), by = .(country, year)]
setnames(pop, 'country', 'location')
pop = rbind(pop, global_pop)

# --------------------------------------
# PREP TB DATA 

# drop unecessary variables and shape long
tb[ ,c('iso2', 'iso3', 'iso_numeric', 'g_whoregion'):=NULL]
tb = melt(tb, id.vars = c('country', 'year')) # some are numerics and some integers

# keep mortality and incidence rates and counts
keep = c('country', 'year', 'e_inc_100k', 'e_inc_100k_hi', 'e_inc_100k_lo', 
         'e_mort_100k', 'e_mort_100k_hi', 'e_mort_100k_lo',
         'e_inc_num', 'e_inc_num_hi', 'e_inc_num_lo',
         'e_mort_num', 'e_mort_num_hi', 'e_mort_num_lo')

tb = tb[variable %in% keep]
setnames(tb, 'country', 'location')

# add in global data 
tb = rbind(tb, glob_tb)

# format to match
tb[grepl('hi', variable), bound:='upper']
tb[grepl('lo', variable), bound:='lower']
tb[!grepl('lo', variable) & !grepl('hi', variable), bound:='val']

# designate mortality and incidence, rates and counts
tb[grepl('mort', variable), measure:='Deaths']
tb[grepl('inc', variable), measure:='Incidence']
tb[grepl('100k', variable), metric:='Rate']
tb[!grepl('100k', variable), metric:='Number']

# add age
tb[metric=='Number', age:='All Ages']
tb[metric=='Rate', age:='Age-standardized']

# add variables not included in the data
tb[ ,sex:='Both']
tb[ ,cause:='Tuberculosis']

tb[ ,variable:=NULL]
tb = dcast(tb, measure+location+sex+age+cause+metric+year~bound)

#---------------------
# output prepped tb data 

saveRDS(tb, paste0(outDir, 'prepped_data/who_tb_2000_2018_prepped.rds'))
#---------------------

#-------------------------------------------------------
# PREP HIV DATA 

#------------------------
# upload and prep unaids data 

hiv = fread(paste0(dir, 'raw_data/unaids/New HIV infections_HIV incidence per 1000 population_Population_ All ages.csv'))
hiv2 = fread(paste0(dir, 'raw_data/unaids/AIDS-related deaths_Number of AIDS-related deaths_Population_ All ages.csv'))
hiv3 = fread(paste0(dir, 'raw_data/unaids/new_hiv_infections_counts.csv'))
#----------------
# write a function to format the unaids data 
format = function(x) {

# rename the columns and drop the first row as it imports weird
name_list = as.character(x[1])
setnames(x, name_list)
x = x[-1]

# subset and shape long
x = x[Country %in% countries]
setnames(x, 'Country', 'location')
x = melt(x, id.vars='location') 
return(x)}
#----------------

#------------------------
# format the data and bind them together

hiv = format(hiv)
hiv2 = format(hiv2)
hiv3 = format(hiv3)

hiv[ ,measure:='Incidence']
hiv[ , metric:='Rate']
hiv2[ , measure:='Deaths']
hiv2[ , metric:='Number']


#--------------------------
# hiv rates are per 1,000 - convert to rate per 100k

hiv[ , value:=100*value]

#--------------------------

hiv = rbind(hiv, hiv2)
hiv[metric=='Number', age:='All Ages']
hiv[metric=='Rate', age:='Age-standardized']# check if age standardized

# shape wide 
hiv[grepl('upper', variable), bound:='upper']
hiv[grepl('lower', variable), bound:='lower']
hiv[!grepl('lower', variable) & !grepl('upper', variable), bound:='val']

# split and add year
hiv$year = unlist(lapply(str_split(hiv$variable, '_'), '[', 1))
hiv[ ,variable:=NULL]
hiv = dcast(hiv, location+measure+metric+age+year~bound)

#------------------------
# new infection counts need special wrangling

# reset names and develop bounds
setnames(hiv3, 'variable', 'year')
hiv3$val = trimws(unlist(lapply(str_split(hiv3$value, '\\['), '[', 1)))
hiv3$lower = trimws(unlist(lapply(str_split(hiv3$value, '\\['), '[', 2)))
hiv3$lower = trimws(unlist(lapply(str_split(hiv3$lower, '\\-'), '[', 1))) #split twice for lower bound
hiv3$upper = trimws(unlist(lapply(str_split(hiv3$value, '\\['), '[', 2)))
hiv3$upper = trimws(unlist(lapply(str_split(hiv3$upper, '\\-'), '[', 2)))
hiv3[ ,upper:=gsub('\\]', '', upper)]
hiv3[ ,value:=NULL]

# add in identifiers
hiv3[ ,age:='All Ages']
hiv3[ ,measure:='Incidence']
hiv3[ , metric:='Number']

hiv = rbind(hiv, hiv3)

#--------------------------------------------------
# final formatting of numbers for the merge

# if the estimate is listed as "less than," delete from the data
hiv[grep('<', val)][order(location, year)]
hiv = hiv[!grepl('<', val)]

# only one estimate for cambodia has a less than, in 1990
hiv = hiv[!grepl('<', upper)]

# if the lower bound is listed as less than 1,000, estimate as 0
hiv[grepl('<', lower), lower:=0]

# trim the spaces from the values
hiv[ ,val:=as.numeric(gsub(' ', '', val))]
hiv[ ,lower:=as.numeric(gsub(' ', '', lower))]
hiv[ ,upper:=as.numeric(gsub(' ', '', upper))]

# add the cause
hiv[ ,cause:='HIV/AIDS']
hiv[ ,sex:='Both']

# confirm year is a numeric
hiv[, year:=as.numeric(as.character(year))]

#--------------------------------------------------
# calculate mortality rates

death_rates = hiv[measure=='Deaths' & 2000 <= year]
death_rates = merge(death_rates, pop, by=c('location','year'))

# create rates
death_rates[ ,population:=(population/100000)]
death_rates[ ,val:=val/population]
death_rates[ ,lower:=lower/population]
death_rates[ ,upper:=upper/population]

death_rates[ ,metric:='Rate']
death_rates[ ,age:='Age-standardized']
death_rates[ ,population:=NULL]

# bind death rates into the data 
hiv = rbind(hiv, death_rates)

#--------------------------------------------------
# save the data set and bind tb and hiv together

# output prepped hiv data 
saveRDS(hiv, paste0(outDir, 'prepped_data/unaids_hiv_2000_2018_prepped.rds'))

# bind the data sets together
tb_hiv = rbind(tb, hiv)

#-------------------------------------------------------
# prep malaria data 

mal = readRDS(paste0(dir, 'raw_data/who_malaria/estimated_malaria_cases_and_deaths_2010_2018.rds'))

# format the data and shape long
setnames(mal, 'country', 'location')
mal[ ,sex:='Both']
mal[ ,population_at_risk:=NULL]
mal = melt(mal, id.vars=c('location', 'year', 'sex'))

# format to match
mal[grepl('upper', variable), bound:='upper']
mal[grepl('lower', variable), bound:='lower']
mal[grepl('point', variable), bound:='val']

# designate mortality and incidence, rates and counts
mal[grepl('deaths', variable) | grepl('mortality', variable) , measure:='Deaths']
mal[grepl('inc', variable) | grepl('cases', variable), measure:='Incidence']

mal[grepl('inc', variable) | grepl('mortality', variable), metric:='Rate']
mal[grepl('deaths', variable) | grepl('cases', variable), metric:='Number']

# add age
mal[metric=='Number', age:='All Ages']
mal[metric=='Rate', age:='Age-standardized']

# add variables not included in the data
mal[ ,sex:='Both']
mal[ ,cause:='Malaria']
mal[ ,variable:=NULL]

# shape wide
mal = dcast(mal, measure+location+sex+age+cause+metric+year~bound)

# match locations in other data
mal[location=='DRC', location:='Democratic Republic of the Congo'] 

#--------------------------------------------------
# save the data set and bind tb and hiv together

# output prepped hiv data 
saveRDS(mal, paste0(outDir, 'prepped_data/who_malaria_2010_2018_prepped.rds'))

# bind the data sets together
dt = rbind(tb_hiv, mal)

# ensure year is not a factor
dt[ ,year:=as.numeric(as.character(year))]
#------------------------------------------------
# save the final product

saveRDS(dt, paste0(outDir, 'prepped_data/who_unaids_prepped.rds'))

#------------------------------------------------
 
