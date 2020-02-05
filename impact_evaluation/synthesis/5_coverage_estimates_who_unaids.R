# ----------------------------------------------
# Caitlin O'Brien-Carelli 
# Estimates of treatment coverage WHO/UNAIDS
# Sourced from 2_synthesis_epidemiology.R
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

#--------------------------------
# upload the data sets

mal = fread(paste0(dir, 'coverage/anti_malaria_1131.csv'))
tb = fread(paste0(dir, 'coverage/tb_tx_coverage.csv'))
art = fread(paste0(dir, 'coverage/Treatment cascade_People living with HIV receiving ART (%)_Population_ All ages.csv'))
#-----------------------------
# format the malaria estimates

mal = mal[sex=='Male',.(location = location_name, year = year_id, 
        mean = round(100*mean_value, 1), 
        lower = round(100*lower_value, 1), 
        upper = round(100*upper_value, 1))]

mal[ , cause:='Malaria']

#---------------------
# format the tb estimates

setnames(tb, c('location', 'year', 'mean',
               'lower', 'upper'))

# remove the percent signs 
tb[ ,mean:=gsub('%', '', mean)]
tb[ ,lower:=gsub('%', '', lower)]
tb[ ,upper:=gsub('%', '', upper)]

# remove cameroon...
tb = tb[location!='Cameroon']

# add the cause
tb[ ,cause:='Tuberculosis']
#---------------------
# format the hiv data 

# rename the columns after the first row and drop
name_list = as.character(art[1])
setnames(art, name_list)
art = art[-1]

# reshape long and then format wide
art = melt(art, id.vars='Country')
setnames(art, 'Country', 'location')

# lower, upper, mean estimates
art[ , year:=unlist(lapply(str_split(art$variable, '_'), '[', 1))]
art[grepl('upper', variable), var:='upper']
art[grepl('lower', variable), var:='lower']
art[!grepl('lower', variable) & !grepl('upper', variable), var:='mean']
art[ ,variable:=NULL]

# subset to the appropriate countries
countries = c("Cambodia", "Democratic Republic of the Congo",
              "Guatemala", "Mozambique", "Myanmar", "Senegal",
              "Sudan", "Uganda", "Global")
art = art[location %in% countries]

# shape wide again
setnames(art, 'var', 'variable')
art[ , value:=as.numeric(value)]
art = dcast(art, location+year~variable)

# add the cause
art[ ,cause:='HIV/AIDS']
#----------------------------------------
# rbind the data sets together and format the variables

dt = rbind(tb, mal)
dt = rbind(dt, art)

dt[ ,mean:=round(as.numeric(mean), 1)]
dt[ ,lower:=round(as.numeric(lower), 1)]
dt[ ,upper:=round(as.numeric(upper), 1)]
dt[ ,year:=as.numeric(year)]

#----------------------------------

saveRDS(dt, paste0(dir, 'prepped_data/who_unaids_coverage_prepped.rds'))

#----------------------------------




