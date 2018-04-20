# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
#4/18/2018
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


# there should be 112 districts to match the shape file
length(unique(uganda_vl$dist_name))


# check that 2014 and 2018 data only includes appropriate months (8/14 - 12/14, 1/18 - present)
uganda_vl[year==2014, sum(samples_received), by=month]
uganda_vl[year==2018, sum(samples_received), by=month]

# ----------------------------------------------
# create a data set shape long and add dates to both

# Reshape indicators long
idVars <- c("facility_name", "facility_id", "dist_name", "district_id",
            "dhis2name","hub_id", "sex", "month", "year")
  
uganda_vl_long <- melt(uganda_vl, id.vars=idVars)

# make date variable
uganda_vl_long[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# add date to uganda_vl
uganda_vl[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

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
gSimplify(shapeData, tol=0.5, topologyPreserve=TRUE)
plot(shapeData)

# ----------------------------------------------


# ----------------------------------------------
# merge the data with the shape file

# identify the variable that contains district names and codes
shapeData@data %>% as_tibble()
unique(shapeData@data$dist112_na)
length(unique(shapeData@data$dist112_na)) # 112 districts

# check for unmatched values
uvl_check <- uganda_vl[,unique(dist_name)]
shape_check <- unique(shapeData@data$dist112_na)

shape_check[!shape_check %in% uvl_check] # shape file contains all districts in uganda vl
uvl_check[!uvl_check %in% shape_check] 

# more efficient check
unique(uganda_vl$dist_name)[!(unique(uganda_vl$dist_name)) %in% unique(shapeData@data$dist112_na)]


# dist112 is district id #s, dist112_na is names; match on names, merge on ids
shapeData@data$dist112_na %>% as_tibble()
shapeData@data$dist112_na %>% as_tibble()

# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(dist_name=shapeData@data$dist112_na, dist_id=shapeData@data$dist112)
str(shape_names)

# total and annual counts and suppression ratios by district
# total for all time
ratio_table <- uganda_vl[ , .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                              dbs_samples=sum(dbs_samples), valid_results=sum(valid_results),
                              suppressed=sum(suppressed),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                         by=.(dist_name)]
                       ratio_table <- ratio_table[order(dist_name)]

# annual counts and ratios
ratio_year <- uganda_vl[ , .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                          dbs_samples=sum(dbs_samples), valid_results=sum(valid_results), suppressed=sum(suppressed),
                          dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                          suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                          by=.(dist_name, year)]
              ratio_year <- ratio_year[order(year, dist_name)]
              
      # ------------------------
      # add logged variables to ratio year     
      logs_year <- ratio_year[ ,.(log_patients_received=log(patients_received), log_samples_received=log(samples_received), 
                     log_dbs_samples=log(dbs_samples), log_valid_results=log(valid_results), 
                     log_suppressed=log(suppressed)),  by=.(dist_name, year)]
       ratio_year <- merge(ratio_year, logs_year, by=c('dist_name', 'year'), all.x=TRUE)
              
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
# create a table for annual maps stratified by sex

# females, annual
ratio_female <- uganda_vl[ sex=='Female', 
                           .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                             dbs_samples=sum(dbs_samples), valid_results=sum(valid_results),
                             suppressed=sum(suppressed), dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                         by=.(dist_name, year)]

ratio_female <- ratio_female[order(year, dist_name)]
ratio_female <- merge(shape_names, ratio_female, by="dist_name")
setnames(ratio_female, "dist_id", "id")

# males, annual
ratio_male <- uganda_vl[sex=='Male', 
                           .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                             dbs_samples=sum(dbs_samples), valid_results=sum(valid_results),
                             suppressed=sum(suppressed), dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                           by=.(dist_name, year)]

ratio_male <- ratio_male[order(year, dist_name)]
ratio_male <- merge(shape_names, ratio_male, by="dist_name")
setnames(ratio_male, "dist_id", "id")


# -----------------

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)]

# coordinates by year for faceting
coordinates_ann <- rbind(coordinates, coordinates, coordinates, coordinates, coordinates)
coordinates_ann[, year:=rep(2014:2018, each=nrow(coordinates))]


# merge on district id
coordinates <- merge(coordinates, ratio_table, by="id", all.x=TRUE)
coordinates_year <- merge(coordinates_ann, ratio_year, by=c('id', 'year'), all.x=TRUE)

coordinates_female <- merge(coordinates_ann, ratio_female, by=c('id','year'), all.x=TRUE)
coordinates_male <- merge(coordinates_ann, ratio_male, by=c('id','year'), all.x=TRUE)


# ----------------------------------------------
# GRAPHS: create annual graphs with variables on the same page 

# create a data set shape long and add dates to both
uvl_1 <- uganda_vl[, .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                       total_results=sum(total_results),
                       dbs_samples=sum(dbs_samples), valid_results=sum(valid_results), suppressed=sum(suppressed)), 
                       by=.(sex, month, year)]

# reshape long
idVars <- c("month", "year", "sex")
uvl_1 <- melt(uvl_1, id.vars=idVars)


# label the variables for graph titles and put the graphs in an intuitive order
uvl_1$variable <- factor(uvl_1$variable, 
                            levels=c("patients_received", "samples_received", "dbs_samples",  "total_results", "valid_results", "suppressed"), 
                            labels=c("Patients", "Samples Received","DBS Samples", "Total Results", "Valid Results", "Suppressed"))

# ----------------------
# annual facet-wrapped graphs with total facilities
# create a data set shape long and add dates to both
uvl_year <- uganda_vl[, .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                         dbs_samples=sum(dbs_samples), valid_results=sum(valid_results), suppressed=sum(suppressed)), 
                          by=.(sex, date)]

        # create a table of the total number of facilities reporting in each district, all years
        total_fac_year <- uganda_vl[, .(total_facilities=length(unique(facility_id))), by=.(date)]

        uvl_year <- merge(uvl_year, total_fac_year, by="date", all.x=TRUE)

        # reshape long
        uvl_year <- melt(uvl_year, id.vars=c("sex","date"))
        
        # keep single values for facilities (females only)
        uvl_year <- uvl_year[!(variable=="total_facilities" & (sex=="Male"| sex=="Unknown")) ]
        uvl_year <- uvl_year[variable=="total_facilities", sex:="Facility"]
        

  # label the variables for graph titles and put the graphs in an intuitive order
  uvl_year$variable <- factor(uvl_year$variable, 
                         levels=c("total_facilities", "patients_received", "samples_received", "dbs_samples",
                                  "valid_results", "suppressed"), 
                         labels=c("Facilities Reporting", "Patients", 
                                  "Samples Received","DBS Samples Received", "Valid Test Results", "Suppressed"))

  uvl_year$sex <- factor(uvl_year$sex, levels=c("Female", "Male", "Unknown", "Facility"),
                         labels=c("Females", "Males", "Unknown", "Facilities"))
  
# ----------------------------------------------
# table for country-level graphs to add to PDF

table_1 <- uganda_vl[,
                     .(patients_received = sum(patients_received), samples_received = sum(samples_received), 
                       dbs_samples=sum(dbs_samples), valid_results = sum(valid_results), suppressed = sum(suppressed),
                       dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                       valid_samples_ratio=100*(sum(valid_results)/sum(samples_received)),
                       suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                     by=.(sex, month,year)]
table_1 <- table_1[order(year, month, sex)]

# count of facilities reporting
# create a table of the number of facilities reporting by district         
table_2 <- uganda_vl[, .(facilities_report=length(unique(facility_id))), by=.(month, year)]
table_2 <- table_2[order(month, year)]


# total facilities reporting all time
facilities_1 <- uganda_vl[ ,.(facilities_report=length(unique(facility_id))), by=.(month, year) ]
facilities_1[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]


# ----------------------------------------------
# create maps and plots and export as a PDF

# ---------------
# store colors

ratio_colors <- brewer.pal(8, 'Spectral')
results_colors <- brewer.pal(6, 'Blues')
sup_colors <- brewer.pal(6, 'Reds')

ladies <- brewer.pal(6, 'PuBuGn')
gents <- brewer.pal(6, 'Purples')

# red colors for bar graph
bar_colors <- c('Not Suppressed'='#de2d26', 'Suppressed'='#fc9272')

graph_colors <- c('#bd0026', '#fecc5c', '#74c476','#3182bd', '#8856a7')
tri_sex <- c('#bd0026', '#74c476', '#3182bd')
wrap_colors <- c('#3182bd', '#fecc5c', '#bd0026', '#74c476', '#8856a7', '#f768a1')
sex_colors <- c('#bd0026', '#3182bd', '#74c476', '#8856a7')

# breaks for log transformation legends
breaks <- c(1, 20, 400, 8100)
  
# ---------------

# export as a pdf
pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/uvl_descriptives.pdf', height=6, width=9)

# annual data reported for major variables
ggplot(uvl_year, aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Year", y="Count", title="Monthly data reported, Uganda Viral Load Dashboard", color="Sex") +
  scale_color_manual(values=sex_colors)


# facilities reporting by month, year
ggplot(facilities_1, aes(x=date, y=facilities_report)) + 
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  labs(title="Number of facilities reporting", x="Date", y="Facilities reporting", caption="Source: Uganda Viral Load Dashboard")


# determine when scale up of reporting occurred
ggplot(table_2, aes(x=factor(month), y=facilities_report, group=year, color=factor(year))) + 
  geom_point(size=2.5) + 
  geom_line(alpha=0.8) + 
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") + ylab("Facilities reporting") + 
  labs(title = "Total facilities reporting viral load test results by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Year") +
  scale_color_manual(values=graph_colors)

# ---------------
# suppression ratio maps

# suppression ratio for all years 
ggplot(coordinates, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() + 
  labs(title="Viral suppression ratios by district, Uganda", subtitle=" August 2014 - February 2018",
       caption="Source: Uganda Viral Load Dashboard", fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# annual suppression ratios
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Viral suppression ratios by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# ---------------
# suppression ratio by sex

# suppression ratios among females
ggplot(coordinates_female, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ladies) + 
  theme_void() +
  labs(title="Viral suppression ratios among females by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# suppression ratios among males
ggplot(coordinates_male, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=gents) + 
  theme_void() +
  labs(title="Viral suppression ratios among males by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


# ---------------
# annual variable comparisons

ggplot(uvl_1[year==2014], aes(y=value, x=factor(month), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2014 Uganda Viral Load Dashboard data by sex") + theme_bw()

ggplot(uvl_1[year==2015], aes(y=value, x=factor(month), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2015 Uganda Viral Load Dashboard data by sex") + theme_bw()

ggplot(uvl_1[year==2016], aes(y=value, x=factor(month), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2016 Uganda Viral Load Dashboard data by sex") + theme_bw()

ggplot(uvl_1[year==2017], aes(y=value, x=factor(month), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2017 Uganda Viral Load Dashboard data by sex") + theme_bw()


# ---------------

# patients received by month, year
ggplot(table_1, aes(x=factor(month), y=patients_received, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + 
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") + ylab("Patients received") + 
  labs(title = "Patients received by month, year", caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# samples received by month, year
ggplot(table_1, aes(x=factor(month), y=samples_received, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + 
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") + ylab("Samples received") + 
  labs(title = "Samples received by month, year", caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# total DBS samples received by month, year
ggplot(table_1, aes(x=factor(month), y=dbs_samples, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() +
  facet_wrap(~year) +
  xlab("Month") + ylab("DBS samples received") + 
  labs(title = "DBS samples received by month, year", caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# DBS ratio by month, year
ggplot(table_1, aes(x=factor(month), y=dbs_ratio, col=factor(sex), group=sex)) + 
  geom_point(size=1.5, alpha=0.6) + geom_line(alpha=0.6) + theme_bw() + ylim(0,100) +
  xlab("Month") + ylab("Percent of total samples (%)") + 
  facet_wrap(~year) +
  labs(title = "Percentage of total samples that are DBS samples by month, year", 
      caption="Source: Uganda VL Dashboard", colour="Sex") +
    scale_color_manual(values=tri_sex)

# valid viral load test results by month, year
ggplot(table_1, aes(x=factor(month), y=valid_results, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() +
  facet_wrap(~year) +
  xlab("Month") + ylab("Valid test results") + 
  labs(title = "Viral load test results by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# valid results as a percent of samples received
ggplot(table_1, aes(x=factor(month), y=valid_samples_ratio, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() + 
  facet_wrap(~year) +
  xlab("Month") + ylab("Percent of total samples (%)") + 
  labs(title = "Valid test results as a percentage of total samples by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# total suppressed persons by month, year
ggplot(table_1, aes(x=factor(month), y=suppressed, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() +
  facet_wrap(~year) +
  xlab("Month") + ylab("Virally suppressed patients") + 
  labs(title = "Total virally suppressed patients by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# suppression ratio by month, year
ggplot(table_1, aes(x=factor(month), y=suppression_ratio, col=factor(sex), group=sex)) + 
  geom_point(size=1.5, alpha=0.5) + geom_line(alpha=0.7) + theme_bw() + ylim(0,100) +
  facet_wrap(~year) +
  xlab("Month") + ylab("Percent suppressed of valid test results (%)") + 
  labs(title = "Percent of patients that are virally suppressed by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# zoom in on suppression ratio by month, year (reduce size of y axis)
ggplot(table_1, aes(x=factor(month), y=suppression_ratio, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() + 
  facet_wrap(~year) +
  xlab("Month") + ylab("Percent suppressed of valid test results (%)") + 
  labs(title = "Percent of patients that are virally suppressed by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# stacked bar showing suppressed/not suppressed of valid test results by year 
ggplot(table_1, aes(x=factor(year), y=valid_results, fill='Not Suppressed')) + 
  geom_bar(stat="identity") + 
  geom_bar(aes(y=suppressed, fill='Suppressed'), stat='identity') + 
  scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
  xlab("Year") + ylab("Total valid test results") +
  labs(title = "Virally suppressed patients", caption="Source: Uganda VL Dashboard")


# ----------------------------------------------
# MAPS

# -------------------
# facilities reporting results

# annual count of facilities reporting results
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=total_facilities)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors) + 
  theme_void() +
  labs(title="Number of facilities reporting on viral suppression, Uganda", subtitle=" n=2,040",
       caption="Source: Uganda Viral Load Dashboard", 
       fill="Facilities reporting") +
  theme(plot.title=element_text(vjust=-1.5), plot.subtitle=element_text(vjust=-1.5),
        plot.caption=element_text(vjust=6)) 

# annual percentage of facilities reporting results
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=facility_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Percentage of total facilities reporting on viral suppression, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% of facilities reporting") +
  theme(plot.title=element_text(vjust=-0.5), plot.caption=element_text(vjust=6)) 


# -------------------
# log-transformed counts

# annual patients received
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=patients_received)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans="log", breaks=breaks, name="Patients received") + 
  theme_void() +
  labs(title="Number of patients received, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=" n=1,974,938") +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6))  

# annual samples received
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=samples_received)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans="log", breaks=breaks, name="Samples received") + 
  theme_void() +
  labs(title="Number of samples received, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=" n=1,980,551") +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6)) 

# dbs samples received
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=dbs_samples)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans="log", breaks=breaks, name="DBS samples received") + 
  theme_void() +
  labs(title="Number of DBS samples received, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=" n=1,345,501") +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6)) 

# annual ratio of all samples that are dbs samples
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=dbs_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Percentage of samples received that are DBS samples, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% DBS") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# annual number of valid test results
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=valid_results)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans="log", breaks=breaks, name="Test results") + 
  theme_void() +
  labs(title="Number of valid viral load test results, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle="  n=1,896,731", fill="Valid test results") +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6))

# annual number of virally suppressed persons
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(suppressed))) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=sup_colors, trans="log", breaks=breaks, name="Suppressed") + 
  theme_void() +
  labs(title="Number of virally suppressed persons, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=" n= 1,673,598 ", fill="Suppressed") +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6)) 


dev.off()

# -----------------



# -----------------

# scale up PDF - use to determine when scale up occurred

# export as a pdf
pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/scale_up.pdf', height=6, width=9)

# facilities reporting

facilities_1 <- uganda_vl[ ,.(facilities_report=length(unique(facility_id))), by=.(month, year) ]
uganda_vl[ , .(length(unique(facility_id)))]
facilities_1[ , percent_report:=((facilities_report/2039)*100)]
facilities_1[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# facilities reporting by month, year
ggplot(facilities_1, aes(x=date, y=facilities_report)) + 
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  labs(title="Number of facilities reporting", x="Date", y="Facilities reporting", caption="Source: Uganda Viral Load Dashboard")


# facilities reporting by month, year
ggplot(facilities_1, aes(x=date, y=percent_report)) + 
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  labs(title="Percentage of facilities reporting", x="Date", y="Percentage of facilities reporting", caption="Source: Uganda Viral Load Dashboard")

# years on separate graphs
ggplot(table_2, aes(x=factor(month), y=facilities_report, group=year, color=factor(year))) + 
  geom_point(size=2.5) + 
  geom_line(alpha=0.8) + 
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") + ylab("Facilities reporting") + 
  labs(title = "Total facilities reporting viral load test results by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Year") +
  scale_color_manual(values=graph_colors)




# detail of all data by month, year
ggplot(uvl_year, aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Year", y="Count", title="Monthly data reported, Uganda Viral Load Dashboard", color="Sex") +
  scale_color_manual(values=sex_colors)




# facilities reporting by month, year
ggplot(table_2, aes(x=factor(month), y=facilities_report, col=factor(year), group=year)) + 
  geom_point(size=2.5) + geom_line(alpha=0.8) + theme_bw() +
  xlab("Month") + ylab("Facilities reporting") + 
  labs(title = "Total facilities reporting viral load test results by month, year", 
       caption="Source: Uganda VL Dashboard", colour="Year") +
  scale_color_manual(values=graph_colors)


dev.off()



