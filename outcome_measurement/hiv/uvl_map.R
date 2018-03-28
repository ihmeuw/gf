# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 3/26/2018
# First map of Uganda by suppression ratio by district
# ----------------------------------------------
# Set up R
# raster package is most of what you need

rm(list=ls())
library(data.table)
library(rgeos)
library(raster)
library(ggplot2)
library(rgdal)
library(tibble)
library(dplyr)
library(RColorBrewer)


# set input/output directory
# ----------------------------------------------
dir <- 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard'

# upload the data with month, year, sex
uganda_vl <- readRDS(paste0(dir, "/sex_data.rds"))

# drop out the current month
uganda_vl <- uganda_vl[!(month==3 & year==2018)]

# view the data set and variable names
class(uganda_vl) # check that it is a data table
str(uganda_vl)

# drop 'District' from the district names
uganda_vl[, dist_name:=gsub('District','', district_name)]
uganda_vl[, dist_name:=gsub(' ','', dist_name)]
uganda_vl[, dist_name]

# Change Luwero=Luweero and Sembabule=Ssembabule
uganda_vl[dist_name=="Luwero", dist_name:="Luweero"]
uganda_vl[dist_name=="Sembabule", dist_name:="Ssembabule"]

# ----------------------------------------------
#upload the shape file

# set working directory
setwd('J:/Project/Evaluation/GF/mapping/uga/')

# load the shapefile
shapeData <- shapefile('uga_dist112_map.shp')

# check that shapeData is a spatialpolygonsdataframe
class(shapeData)

# plot the shape file in the base package
plot(shapeData)


# simplify the shape data (could create little gaps, maybe don't do this)
# shapeData = gSimplify(shapeData, tol=0.01, topologyPreserve=TRUE)

# ----------------------------------------------


# ----------------------------------------------
# merge the files

# identify the variable that contains district names and codes
shapeData@data %>% as_tibble()
unique(shapeData@data$dist112_na)
length(unique(shapeData@data$dist112_na)) # 112 districts

# dist112 is district id #s, dist112_na is names; match on names, merge on ids
shapeData@data$dist112_na %>% as_tibble()
shapeData@data$dist112_na %>% as_tibble()

# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(dist_name=shapeData@data$dist112_na, dist_id=shapeData@data$dist112)
str(shape_names)

# total and annual counts and suppression ratios by district
ratio_table <- uganda_vl[ , .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                              dbs_samples=sum(dbs_samples), valid_results=sum(valid_results), suppressed=sum(suppressed),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                         by=.(dist_name)]
              ratio_table <- ratio_table[order(dist_name)]


ratio_year <- uganda_vl[ , .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                          dbs_samples=sum(dbs_samples), valid_results=sum(valid_results), suppressed=sum(suppressed),
                          suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                          by=.(dist_name, year)]
              ratio_year <- ratio_year[order(year, dist_name)]
              
              
        # ------------------------
        # facilities reporting
              
        # create a table of the number of facilities reporting by district         
        facilities_table <- uganda_vl[, .(facilities_report=length(unique(facility_id))), by=.(dist_name, year)]
        facilities_table <- facilities_table[order(year, dist_name)]
              
        # create a table of the total number of facilities reporting in each district, all years
        total_fac <- uganda_vl[, .(total_facilities=length(unique(facility_id))), by=.(dist_name)]
        total_fac[,.(dist_name, total_facilities)]
              
        # merge on district name
        facilities_table <- merge(facilities_table, total_fac, by='dist_name', all.x=TRUE)

         # divide facilities reporting in each year by total facilities ever reported for ratio
         facilities_table[, facility_ratio:=((facilities_report/total_facilities)*100), by=.(dist_name, year)]
              
              # check 
              facilities_table[year==2014, .(dist_name, facility_ratio)] 
              facilities_table[year==2015, .(dist_name, facility_ratio)] 
              facilities_table <- facilities_table[order(year, dist_name)]
              
              # merge count and ratio of facilities reporting into ratio_year
              ratio_year <- merge(ratio_year, facilities_table, by=c('dist_name', 'year'), all.x=TRUE)
          
            # ------------------------
              
# check for unmatched values
ratio <- ratio_table[,unique(dist_name)]
shape <- shape_names[, unique(dist_name)]
ratio <- sort(ratio)
shape <- sort(shape)
              
shape[!shape %in% ratio] # shape file contains all districts in uganda vl
ratio[!ratio %in% shape] 
length(ratio[!ratio %in% shape]) # 10 districts are in the uvl data but not the shape file
              
#merge shape and uvl data on district names; rename the district ids 'id'
ratio_table <- merge(shape_names, ratio_table, by="dist_name")
ratio_year <- merge(shape_names, ratio_year, by="dist_name")
              
# rename both district ids "id" for ease of reference
setnames(ratio_table, "dist_id", "id")
setnames(ratio_year, "dist_id", "id")


# -----------------

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)]

# coordinates by year for faceting
coordinates_year <- rbind(coordinates, coordinates, coordinates, coordinates, coordinates)
coordinates_year[, year:=rep(2014:2018, each=nrow(coordinates))]

# merge on district id
coordinates <- merge(coordinates, ratio_table, by="id", all.x=TRUE)
coordinates_year <- merge(coordinates_year, ratio_year, by=c('id', 'year'), all.x=TRUE)



# ----------------------------------------------
# create plots and export as a PDF

# store colors
ratio_colors <- brewer.pal(8, 'Spectral')
results_colors <- brewer.pal(6, 'Blues')
sup_colors <- brewer.pal(6, 'Reds')

# -------------------
# maps to export

"J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs"

pdf('C:/Users/ccarelli/plots.pdf', height=6, width=9)

# suppression ratio for all years 
ggplot(coordinates, aes(x=long, y=lat, group=group, fill=as.numeric(suppression_ratio))) + 
  geom_polygon() + 
  geom_path() + 
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() + 
  labs(title="Viral suppression ratios by district, Uganda", subtitle=" August 2014 - February 2018",
       caption="Source: Uganda Viral Load Dashboard", fill="Percent virally suppressed") +
        theme(plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
              plot.caption=element_text(vjust=6))


# annual suppression ratios
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(suppression_ratio))) + 
  geom_polygon() + 
  geom_path() + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Viral suppression ratios by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="Percent virally suppressed") +
      theme(plot.title=element_text(vjust=-2), plot.caption=element_text(vjust=6))

# -------------------

# annual percentage of facilities reporting results
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(facility_ratio))) + 
  geom_polygon() + 
  geom_path() + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Percentage of total facilities reporting on viral suppression, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="Percentage of facilities reporting") +
  theme(plot.title=element_text(vjust=-2), plot.caption=element_text(vjust=6))

# annual count of facilities reporting results
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(total_facilities))) + 
  geom_polygon() + 
  geom_path() + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors) + 
  theme_void() +
  labs(title="Number of facilities reporting, Uganda", subtitle=" n=2,040",
       caption="Source: Uganda Viral Load Dashboard", 
       fill="Facilities reporting") +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2),
        plot.caption=element_text(vjust=6))

# -------------------

# annual patients received
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(patients_received))) + 
  geom_polygon() + 
  geom_path() + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=resultscolors) + 
  theme_void() +
  labs(title="Number of patients received, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=" n=1,974,938", fill="Patients received") +
  theme(plot.title=element_text(vjust=-3), plot.subtitle=element_text(vjust=-3), plot.caption=element_text(vjust=6))

# annual samples received
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(samples_received))) + 
  geom_polygon() + 
  geom_path() + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=resultscolors) + 
  theme_void() +
  labs(title="Number of samples received, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=" n=1,980,551", fill="Samples received") +
  theme(plot.title=element_text(vjust=-3), plot.subtitle=element_text(vjust=-3), plot.caption=element_text(vjust=6))

# dbs samples received
# samples received
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(dbs_samples))) + 
  geom_polygon() + 
  geom_path() + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=resultscolors) + 
  theme_void() +
  labs(title="Number of DBS samples received, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=" n=1,345,501", fill="DBS samples received") +
  theme(plot.title=element_text(vjust=-3), plot.subtitle=element_text(vjust=-3), plot.caption=element_text(vjust=6))

# annual number of valid test results
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(valid_results))) + 
  geom_polygon() + 
  geom_path() + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=resultscolors) + 
  theme_void() +
  labs(title="Number of valid viral load test results, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle="  n=1,896,731", fill="Valid test results") +
  theme(plot.title=element_text(vjust=-3), plot.subtitle=element_text(vjust=-3), plot.caption=element_text(vjust=6))

# annual number of virally suppressed persons
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(suppressed))) + 
  geom_polygon() + 
  geom_path() + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=supcolors) + 
  theme_void() +
  labs(title="Number of virally suppressed persons, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=" n= 1,673,598 ", fill="Suppressed") +
  theme(plot.title=element_text(vjust=-3), plot.subtitle=element_text(vjust=-3), plot.caption=element_text(vjust=6))

dev.off()

# -----------------




