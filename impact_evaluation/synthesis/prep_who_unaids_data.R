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

#--------------------------------

dt = fread(paste0(dir, 'raw_data/gbd/ihme_age_standardized_2017.csv'))

#-----------------------------------------------------------------
# read in who tb 
tb = fread(paste0(dir, 'raw_data/who_tb/TB_burden_countries_2020-01-21.csv'))

# subset and rename variables
countries = c("Cambodia", "Democratic Republic of the Congo",
  "Guatemala", "Mozambique", "Myanmar", "Senegal", "Sudan", "Uganda", "Global")

# subset to relevant countries, does not include global estimates
tb = tb[country %in% countries]


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

hiv = rbind(hiv, hiv2)
hiv[ ,age:='All Ages']

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


hiv[ ,val:=gsub(' ', '', val)]
hiv[ ,lower:=gsub(' ', '', lower)]
hiv[ ,upper:=gsub(' ', '', upper)]





# designate mortality and incidence, rates and counts
tb[grepl('mort', variable), measure:='Deaths']
tb[grepl('inc', variable), measure:='Incidence']
tb[grepl('100k', variable), metric:='Rate']
tb[!grepl('100k', variable), metric:='Number']

tb[!grepl('100k', variable), metric:='Number']

# add age
tb[metric=='Number', age:='All ages']
tb[metric=='Rate', age:='Age-standardized']






#-------------------------------------------------------


# prep malaria data 

mal = readRDS(paste0(dir, 'who_malaria/estimated_malaria_cases_and_deaths_2010_2018.rds'))

setnames(mal, c('country'),
         c('location'))
mal[ ,sex:='Both']