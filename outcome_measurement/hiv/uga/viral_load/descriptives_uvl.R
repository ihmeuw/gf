# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 12/26/2018
# Descriptive statistics and maps for the Uganda Viral Load Dashboard
# ----------------------------------------------
# Set up R

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(maptools)
library(plyr)

# --------------------
# detect if on windows or on the cluster 

root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# ----------------------------------------------
# Files and directories

# set input/output directory
dir = paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')
 
# # upload the data with month, year, sex
 uvl = readRDS(paste0(dir, "/prepped_data/sex_data.rds"))

# view the data set and variable names
str(uvl)

# there should be 112 districts to match the shape file
length(unique(uvl$district))

# ----------------------------------------------
#upload the shape file

# set working directory
setwd('J:/Project/Evaluation/GF/mapping/uga/')

# load the shapefile
shapeData = shapefile('uga_dist112_map.shp')

# check that shapeData is a spatialpolygonsdataframe
class(shapeData)

# plot the shape file in the base package
plot(shapeData)

# simplify the shape data 
gSimplify(shapeData, tol=0.5, topologyPreserve=TRUE)

#---------------------------------------
# create a data table that contains only district names and ids from the shape file

shape_names = data.table(district=shapeData@data$dist112_na, id=shapeData@data$dist112)

#----------------------------------------
# total and annual counts and suppression ratios by district

# total for all time - sum over all years 
ratio_table = uvl[ , .(patients_received=sum(patients_received), 
                        samples_received=sum(samples_received), 
                        dbs_samples=sum(dbs_samples), 
                        valid_results=sum(valid_results),
                        suppressed=sum(suppressed), 
                        suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                   by=.(district)][order(district)]


# annual counts and ratios - sum by year 
ratio_year = uvl[ , .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                          dbs_samples=sum(dbs_samples), valid_results=sum(valid_results), 
                          suppressed=sum(suppressed),
                          dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                          suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                          by=.(year, district)][order(year, district)]
              
# -----------------------
# facilities reporting
              
# create a table of the number of facilities reporting annualy by district         
facilities_tab = uvl[, .(facilities_report=length(unique(facility_id))), by=.(district, year)]
              
        # create a table of the total number of facilities reporting in each district, all years
        total_fac = uvl[, .(total_facilities=length(unique(facility_id))), by=district]
              
        # merge on district name
        facilities_table = merge(facilities_tab, total_fac, by='district')
        facilities_table[ , ratio:=(100*(facilities_report/total_facilities))]
              
        # merge count and ratio of facilities reporting into ratio_year
        ratio_year = merge(ratio_year, facilities_table, by=c('district', 'year'), all.x=TRUE)
          
#-------------------------------------------------------------------

# ------------------------
# merge the data tables with shapenames to get the district id #s
              
# merge shape and uvl data on district names
ratio_table = merge(shape_names, ratio_table, by="district")
ratio_year = merge(shape_names, ratio_year, by="district")
              
# -----------------
# create data tables for annual maps stratified by sex

# females, annual map
ratio_female = uvl[ sex=='Female', 
                         .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                            dbs_samples=sum(dbs_samples), valid_results=sum(valid_results),
                            suppressed=sum(suppressed), dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                            suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                            by=.(district, year)] [order(year, district)]

ratio_female = merge(shape_names, ratio_female, by="district")

# males, annual map
ratio_male = uvl[sex=='Male', 
                           .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                             dbs_samples=sum(dbs_samples), valid_results=sum(valid_results),
                             suppressed=sum(suppressed), dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                            by=.(district, year)] [order(year, district)]

ratio_male = merge(shape_names, ratio_male, by="district")

# -----------------

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates = data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)]

# coordinates by year for faceting (repeat 5 times for 5 years of data)
coordinates_ann = rbind(coordinates, coordinates, coordinates, coordinates, coordinates)
coordinates_ann[, year:=rep(2014:2018, each=nrow(coordinates))]

# merge on district id - all time total, annual totals, sex stratified totals
coordinates = merge(coordinates, ratio_table, by="id", all.x=TRUE)
coordinates_year = merge(coordinates_ann, ratio_year, by=c('id', 'year'), all.x=TRUE)
coordinates_female = merge(coordinates_ann, ratio_female, by=c('id','year'), all.x=TRUE)
coordinates_male = merge(coordinates_ann, ratio_male, by=c('id','year'), all.x=TRUE)

# ----------------------------------------------
# GRAPHS: create data tables for annual graphs with multiple outcome variables displayed

# create a data set shape long and add dates to both
uvl_1 = uvl[, .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                  samples_tested=sum(samples_tested), dbs_samples=sum(dbs_samples),
                  valid_results=sum(valid_results), suppressed=sum(suppressed)), 
                  by=.(sex, date, year)]
# reshape long
uvl_1 = melt(uvl_1, id.vars=c("date", "sex", "year"))

# label the variables for graph titles and put the graphs in an intuitive order
uvl_1$variable = factor(uvl_1$variable, 
                  levels=c("patients_received", "samples_received", "dbs_samples",  "samples_tested", "valid_results", "suppressed"), 
                  labels=c("Patients", "Samples Received","DBS Samples", "Samples Tested", "Valid Results", "Suppressed"))

# ----------------------
# annual facet-wrapped graphs with total facilities
# create a data set shaped long 
uvl_year = uvl[, .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                         dbs_samples=sum(dbs_samples), valid_results=sum(valid_results), suppressed=sum(suppressed)), 
                         by=.(sex, date)]

        # create a table of the total number of facilities reporting in each district, all years
        total_fac_year = uvl[, .(total_facilities=as.numeric(length(unique(facility_id)))), by=.(date)]

        uvl_year = merge(uvl_year, total_fac_year, by="date", all.x=TRUE)

        # reshape long
        uvl_year = melt(uvl_year, id.vars=c("sex","date"))
        
        # keep single values for facilities (females only for ease) - only after reshaped long
        uvl_year = uvl_year[!(variable=="total_facilities" & (sex=="Male"| sex=="Unknown")) ]
        uvl_year = uvl_year[variable=="total_facilities", sex:="Facility"]
        
# label the variables for graph titles and put the graphs in an intuitive order
uvl_year$variable = factor(uvl_year$variable, 
                         levels=c("total_facilities", "patients_received", "samples_received", "dbs_samples",
                                  "valid_results", "suppressed"), 
                         labels=c("Facilities Reporting", "Patients Submitting Samples", 
                                  "Samples Received","DBS Samples Received", "Valid Test Results", "Suppressed"))

uvl_year$sex = factor(uvl_year$sex, levels=c("Female", "Male", "Unknown", "Facility"),
                         labels=c("Female", "Male", "Unknown", "Health Facility"))

# ----------------------------------------------
# table for country-level graphs to add to PDF

table_1 = uvl[  ,.(patients_received = sum(patients_received), samples_received = sum(samples_received), 
                    dbs_samples=sum(dbs_samples), plasma_samples=sum(plasma_samples),
                    valid_results = sum(valid_results), suppressed = sum(suppressed),
                    dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                    plasma_ratio=100*(sum(plasma_samples)/sum(samples_received)),
                    valid_samples_ratio=100*(sum(valid_results)/sum(samples_received)),
                    suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                        by=.(sex, date, year)] [order(date, sex)]

# --------------------
# count of facilities reporting by month
table_2 = uvl[, .(facilities_report=length(unique(facility_id))), by=.(date, year)][order(date)]


# ----------------------------------------------
# color palettes for maps and plots

# store colors
ratio_colors = brewer.pal(8, 'Spectral')
results_colors = brewer.pal(6, 'Blues')
sup_colors = brewer.pal(6, 'Reds')
ladies = brewer.pal(11, 'RdYlBu')
gents = brewer.pal(9, 'Purples')

# red colors for bar graph
bar_colors = c('Not Suppressed'='#de2d26', 'Suppressed'='#fc9272')

graph_colors = c('#bd0026', '#fecc5c', '#74c476','#3182bd', '#8856a7')
tri_sex = c('#bd0026', '#74c476', '#3182bd')
wrap_colors = c('#3182bd', '#fecc5c', '#bd0026', '#74c476', '#8856a7', '#f768a1')
sex_colors = c('#bd0026', '#3182bd', '#74c476', '#8856a7') # colors by sex plus one for facilities
single_red = '#bd0026'

# breaks for log transformation legends
breaks = c(1, 20, 400, 8100)

#----------------------------------------------



