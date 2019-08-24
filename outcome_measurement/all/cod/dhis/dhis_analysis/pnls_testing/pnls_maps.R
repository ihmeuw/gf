# Create maps of the PNLS testing data at the DPS level
# Caitlin O'Brien-Carelli
# 8/17/2019
# ----------------------------------------------

# --------------------
library(data.table)
library(ggplot2)
library(dplyr)
library(stringr) 
library(maptools)
library(raster)
library(RColorBrewer)
library(gridExtra)
library(grid)
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
# dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
# # read in the data 
# dt = readRDS(paste0(dir, 'prepped/pnls_final/pnls_vct_final.rds'))

# local directory
dir = "C:/Users/ccarelli/Documents/pnls_data/"

# read in the data locally
dt = readRDS(paste0(dir, 'pnls_vct_final.rds'))

# subset the data table to only a single year
year = 2018

dt = dt[year(date)==year]

# start with all provinces, then maps for just gf health zones

#---------------------------------------------------
# import the shape file

# set working directory
setwd('C:/Users/ccarelli/Documents/drc_shape_files/')

# dps level shapefile
map = shapefile('gadm36_COD_1.shp')
coord = fortify(map, region='NAME_1')
# names = map@data$NAME_1
coord = data.table(coord)

coord[id=="Bas-Uélé", id:="Bas Uele"]
coord[id=="�???quateur" , id:="Equateur"]
coord[id=="Haut-Lomami" , id:="Haut Lomami"]
coord[id=="Haut-Uélé" , id:="Haut Uele"]
coord[id=="Kasaï", id:="Kasai"]
coord[id=="Kasaï-Central", id:="Kasai Central"]
coord[id=="Maï-Ndombe", id:="Maindombe"]
coord[id=="Nord-Kivu", id:="Nord Kivu"]
coord[id=="Nord-Ubangi", id:="Nord Ubangi"]
coord[id=="Sud-Kivu", id:="Sud Kivu"]
coord[id=="Sud-Ubangi", id:="Sud Ubangi"]
coord[id=="Kasaï-Oriental", id:="Kasai Oriental"]
coord[grep("ateur", id), id:="Equateur"]
coord[id=="Kongo-Central", id:="Kongo Central"]

names = coord[ ,unique(id)]
dps = dt[ ,unique(dps)]
dps[!(dps %in% names)]




#---------------------------------------------------
# #------------------------------------
# # factor sub populations for graphs 
# 
# dt$subpop = factor(dt$subpop, 
#                    c("prisoner", "trans", "idu", "trucker",  "uniform", "msm", "csw_customer", 
#                      "fisher", "miner", "other_groups", "couple", "csw", "client", "patient"),    
#                    c("Prisoners", "Trans people", "IDUs", "Truckers", "Military personnel",
#                      "MSM", "CSW Clients", "Fisher people", "Miners", 
#                      "Other groups", "Couples", "CSWs", "Clients", "Patients")) 
# 
# #----------------------------------
# # equality constraints check on testing and positive
# # if there are more HIV cases reported than tests, remove the value
# 
# check = dt[variable=='Tested and received the results' | variable=='HIV+']
# check = check[ ,.(value=sum(value)), by = .(org_unit_id, date, variable, sex, age, subpop)]
# check = dcast(check, org_unit_id+sex+age+subpop+date~variable)
# setnames(check, c('org_unit_id', 'sex', 'age', 'subpop', 'date', 'hiv', 'tests'))
# check[ , eq:=(hiv > tests)]
# check[ , missing_one:=(is.na(hiv) | is.na(tests))]
# check = check[eq==T]
# 
# check[ , check_var:=paste0(org_unit_id, sex, age, subpop, date)]
# dt[ , check_var:=paste0(org_unit_id, sex, age, subpop, date)]
# dt = dt[!(check_var %in% check$check_var)]
# 
# #----------------------------------
# # create smaller health facility groupings for graphs 
# 
# dt[grep('hospital',facility_level), next_level:='Hospitals']
# dt[facility_level=='reference_health_center', next_level:='Reference health centers']
# dt[facility_level=='health_center' | facility_level=='health_post' | facility_level=='dispensary', next_level:='Health centers, posts, and dispensaries']
# dt[is.na(next_level), next_level:='Other types of facilities']
# 
# # factor facility level for graphs
# dt$facility_level = factor(dt$facility_level, 
#                            rev(c("health_center", "reference_health_center", "health_post", "hospital", 
#                                  "general_reference_hospital", "hospital_center", "medical_center",
#                                  "clinic", "secondary_hospital",  "dispensary","polyclinic", "medical_surgical_center")),
#                            rev(c("Health Center", "Reference Health Center", "Health Post", "Hospital", 
#                                  "General Reference Hospital", "Hospital Center", "Medical Center",
#                                  "Clinic", "Secondary Hospital",  "Dispensary","Polyclinic", "Medical surgical center")))
# 
# 
# #------------------------------------------------------------------
# # bind in he client variables
# 
# dt[variable=='Clients counseled', variable:='Counseled']
# dt[variable=='Clients tested', variable:='Tested'] # only in clients
# dt[variable=='Clients tested and received the results', variable:='Tested and received the results']
# dt[variable=='Clients enrolled in case management', variable:='Enrolled in case management'] # only in clients
# 
# dt[variable=='Clients HIV+', variable:='Counseled']
# dt[variable=='Clients HIV+ and informed of their results', variable:='HIV+ and informed of the results']
# dt[variable=='Clients with indeterminate status', variable:='Indeterminate status'] # only in clients

#------------------------------------------------------------------
# HIV Testing Visualizations

# EXPORT AS A PDF
# pdf(paste0(dir, 'outputs/pnls_hiv_testing/pnls_vct_dps_maps.pdf'), width=12, height=9)

# export locally as a pdf
# pdf(paste0(dir, '/pnls_dps_maps.pdf'), width=14, height=9)

#----------------------
# COLOR SCHEMES

quad_colors = c('#542788','#66bd63', '#b2182b', '#4575b4')
sex_colors = c('#31a354', '#b2182b', '#4575b4')
tri_colors = c('#a50026', '#fdae61', '#abd9e9')
test_colors = c('#a50026', '#fdae61',  '#4575b4')
colors12 = c(brewer.pal(11, 'Spectral'), '#a6d96a')
bi= c("#fdae61", "#8073ac")
op = c('#f1a340', '#998ec3')
#----------------------

# subset only to the most important variables
# make DPS maps of each 

key_vars = c('Tested and received the results','HIV+')

# check you have the correct variables
dt[variable %in% key_vars, unique(variable), by=subpop]

# subset
maps = dt[variable %in% key_vars]

#---------------------------------------------------
# create a data set with maps only 

maps = dt[ ,.(value = sum(value)), by=.(variable, subpop, id = dps)]

# calculate the percent positivity rate 
# shape the data wide and calculate
maps_wide = dcast(maps, id+subpop~variable)
setnames(maps_wide, c("Tested and received the results", "HIV+"), 
         c('tested_r', 'hiv'))
maps_wide[ , percent_pos:=round(100*(hiv/tested_r), 1)]


maps2 = maps[variable== ,.(value=sum(value)), by=]


coord2 = data.table(rbind(coord, coord, coord, coord, coord, coord, coord, coord))



merge(coord, maps, by='id', all=T)















