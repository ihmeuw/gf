# ----------------------------------------------
# Caitlin O'Brien-Carelli
#
# 7/30/2018
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
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard')

# upload the data with month, year, sex
uvl <- readRDS(paste0(dir, "/prepped_data/sex_data.rds"))

# view the data set and variable names
str(uvl)

# there should be 112 districts to match the shape file
length(unique(uvl$district_name))

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
# merge the data with the shape file

# identify the variable that contains district names and codes
shapeData@data %>% as_tibble()
unique(shapeData@data$dist112_na)
length(unique(shapeData@data$dist112_na)) # 112 districts

# check for unmatched district names in the shape file
unique(uvl$district_name)[!(unique(uvl$district_name)) %in% unique(shapeData@data$dist112_na)]

# create a data table that contains only district names and ids from the shape file
shape_names <- data.table(district_name=shapeData@data$dist112_na, id=shapeData@data$dist112)
str(shape_names)

#----------------------------------------
# total and annual counts and suppression ratios by district

# total for all time - sum over all years 
ratio_table <- uvl[ , .(patients_received=sum(patients_received), 
                        samples_received=sum(samples_received), 
                        dbs_samples=sum(dbs_samples), 
                        valid_results=sum(valid_results),
                        suppressed=sum(suppressed), 
                        suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                   by=.(district_name)] [order(district_name)]


# annual counts and ratios - sum by year 
ratio_year <- uvl[ , .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                          dbs_samples=sum(dbs_samples), valid_results=sum(valid_results), suppressed=sum(suppressed),
                          dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                          suppression_ratio=100*(sum(suppressed)/sum(valid_results)),
                          ratio_valid=100*sum(valid_results)/sum(samples_received)), 
                          by=.(district_name, year)] [order(year, district_name)]
              
# ------------------------
# facilities reporting
              
# create a table of the number of facilities reporting annualy by district         
facilities_table <- uvl[, .(facilities_report=length(unique(facility_id))), by=.(district_name, year)] [order(year, district_name)]
              
        # create a table of the total number of facilities reporting in each district, all years
        total_fac <- uvl[, .(total_facilities=length(unique(facility_id))), by=.(district_name)]
              
        # merge on district name
        facilities_table <- join(facilities_table, total_fac, by='district_name', type='left') 
        facilities_table[order(district_name, year)]

         # divide facilities reporting in each year by total facilities ever reported for ratio
         facilities_table[ ,facility_ratio:=(as.numeric(facilities_report)/as.numeric(total_facilities))*100]
              
              # merge count and ratio of facilities reporting into ratio_year
              ratio_year <- merge(ratio_year, facilities_table, by=c('district_name', 'year'), all.x=TRUE)
          
#-------------------------------------------------------------------

# ------------------------
# merge the data tables with shapenames to get the district id #s
              
# merge shape and uvl data on district names
ratio_table <- merge(shape_names, ratio_table, by="district_name")
ratio_year <- merge(shape_names, ratio_year, by="district_name")
              
# -----------------
# create data tables for annual maps stratified by sex

# females, annual map
ratio_female <- uvl[ sex=='Female', 
                         .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                            dbs_samples=sum(dbs_samples), valid_results=sum(valid_results),
                            suppressed=sum(suppressed), dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                            suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                            by=.(district_name, year)] [order(year, district_name)]

ratio_female <- merge(shape_names, ratio_female, by="district_name")

# males, annual map
ratio_male <- uvl[sex=='Male', 
                           .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                             dbs_samples=sum(dbs_samples), valid_results=sum(valid_results),
                             suppressed=sum(suppressed), dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                             suppression_ratio=100*(sum(suppressed)/sum(valid_results))), 
                            by=.(district_name, year)] [order(year, district_name)]

ratio_male <- merge(shape_names, ratio_male, by="district_name")

# -----------------

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates <- data.table(fortify(shapeData, region='dist112')) 
coordinates[, id:=as.numeric(id)]

# coordinates by year for faceting (repeat 5 times for 5 years of data)
coordinates_ann <- rbind(coordinates, coordinates, coordinates, coordinates, coordinates)
coordinates_ann[, year:=rep(2014:2018, each=nrow(coordinates))]

# merge on district id - all time total, annual totals, sex stratified totals
coordinates <- merge(coordinates, ratio_table, by="id", all.x=TRUE)
coordinates_year <- merge(coordinates_ann, ratio_year, by=c('id', 'year'), all.x=TRUE)
coordinates_female <- merge(coordinates_ann, ratio_female, by=c('id','year'), all.x=TRUE)
coordinates_male <- merge(coordinates_ann, ratio_male, by=c('id','year'), all.x=TRUE)

# ----------------------------------------------
# GRAPHS: create data tables for annual graphs with multiple outcome variables displayed

# create a data set shape long and add dates to both
uvl_1 <- uvl[, .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                  samples_tested=sum(samples_tested), dbs_samples=sum(dbs_samples),
                  valid_results=sum(valid_results), suppressed=sum(suppressed)), 
                  by=.(sex, date, month, year)]
# reshape long
uvl_1 <- melt(uvl_1, id.vars=c("date", "sex", "month", "year"))

# label the variables for graph titles and put the graphs in an intuitive order
uvl_1$variable <- factor(uvl_1$variable, 
                  levels=c("patients_received", "samples_received", "dbs_samples",  "samples_tested", "valid_results", "suppressed"), 
                  labels=c("Patients", "Samples Received","DBS Samples", "Samples Tested", "Valid Results", "Suppressed"))

# ----------------------
# annual facet-wrapped graphs with total facilities
# create a data set shaped long 
uvl_year <- uvl[, .(patients_received=sum(patients_received), samples_received=sum(samples_received), 
                         dbs_samples=sum(dbs_samples), valid_results=sum(valid_results), suppressed=sum(suppressed)), 
                         by=.(sex, date)]

        # create a table of the total number of facilities reporting in each district, all years
        total_fac_year <- uvl[, .(total_facilities=as.numeric(length(unique(facility_id)))), by=.(date)]

        uvl_year <- merge(uvl_year, total_fac_year, by="date", all.x=TRUE)

        # reshape long
        uvl_year <- melt(uvl_year, id.vars=c("sex","date"))
        
        # keep single values for facilities (females only for ease) - only after reshaped long
        uvl_year <- uvl_year[!(variable=="total_facilities" & (sex=="Male"| sex=="Unknown")) ]
        uvl_year <- uvl_year[variable=="total_facilities", sex:="Facility"]
        
# label the variables for graph titles and put the graphs in an intuitive order
uvl_year$variable <- factor(uvl_year$variable, 
                         levels=c("total_facilities", "patients_received", "samples_received", "dbs_samples",
                                  "valid_results", "suppressed"), 
                         labels=c("Facilities Reporting", "Patients Submitting Samples", 
                                  "Samples Received","DBS Samples Received", "Valid Test Results", "Suppressed"))

uvl_year$sex <- factor(uvl_year$sex, levels=c("Female", "Male", "Unknown", "Facility"),
                         labels=c("Female", "Male", "Unknown", "Health Facility"))

# ----------------------------------------------
# table for country-level graphs to add to PDF

table_1 <- uvl[  ,.(patients_received = sum(patients_received), samples_received = sum(samples_received), 
                    dbs_samples=sum(dbs_samples), plasma_samples=sum(plasma_samples),
                    valid_results = sum(valid_results), suppressed = sum(suppressed),
                    dbs_ratio=100*(sum(dbs_samples)/sum(samples_received)),
                    plasma_ratio=100*(sum(plasma_samples)/sum(samples_received)),
                    valid_samples_ratio=100*(sum(valid_results)/sum(samples_received)),
                    suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                        by=.(sex, date, month, year)] [order(date, sex)]

# --------------------
# count of facilities reporting by month
table_2 <- uvl[, .(facilities_report=length(unique(facility_id))), by=.(date, month, year)][order(date)]

#---------------------
# percentage of patients with unknown sex by month
sex_tot <- uvl[ ,.(sex_total=sum(patients_received)), by=.(date, sex)]
tot <- uvl[ ,.(total=sum(patients_received)), by=.(date)]
sex_tot <- merge(sex_tot, tot, by='date', all=T)
sex_tot <- sex_tot[sex=='Unknown']
sex_tot[ ,percent:=(sex_total/total)*100]

# ----------------------------------------------
# create maps and plots and export as a PDF

# ---------------
# store colors
ratio_colors <- brewer.pal(9, 'Spectral')
results_colors <- brewer.pal(6, 'Blues')
sup_colors <- brewer.pal(6, 'Reds')
ladies <- brewer.pal(11, 'RdYlBu')
gents <- brewer.pal(9, 'Purples')

# red colors for bar graph
bar_colors <- c('Not Suppressed'='#de2d26', 'Suppressed'='#fc9272')

graph_colors <- c('#bd0026', '#fecc5c', '#74c476','#3182bd', '#8856a7')
tri_sex <- c('#bd0026', '#74c476', '#3182bd')
wrap_colors <- c('#3182bd', '#fecc5c', '#bd0026', '#74c476', '#8856a7', '#f768a1')
sex_colors <- c('#bd0026', '#3182bd', '#74c476', '#8856a7') # colors by sex plus one for facilities
single_red <- '#bd0026'

# breaks for log transformation legends
breaks <- c(1, 20, 400, 8100)

# ------------------------------------------------------------------
# Primary descriptive analysis PDF - August 2014 - June 2018

# export all summary descriptive figures as a pdf
pdf(paste0(dir, '/outputs/uvl_descriptives.pdf'), height=6, width=9)

# annual data reported for major variables
ggplot(uvl_year, aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Year", y="Count", title="Monthly data reported, Uganda Viral Load Dashboard", color="Sex") +
  scale_color_manual(values=sex_colors)

# facilities reporting by month, year
ggplot(table_2, aes(x=date, y=facilities_report, color='red')) + 
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  labs(title="Number of facilities reporting", x="Date", y="Facilities reporting", caption="Source: Uganda Viral Load Dashboard") +
  scale_color_manual(values=single_red) +
  theme(legend.position='none')

# determine when scale up of reporting occurred
ggplot(table_2, aes(x=factor(month), y=facilities_report, group=year, color=factor(year))) + 
  geom_point(size=2.5) + 
  geom_line(alpha=0.8) + 
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") + ylab("Facilities reporting") + 
  labs(title = "Total facilities reporting viral load test results by date (scale up)", 
       caption="Source: Uganda VL Dashboard", colour="Year") +
  scale_color_manual(values=graph_colors)

ggplot(uvl_year[variable=='Facilities Reporting' | variable=='Patients Submitting Samples'], aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Year", y="Count", title="Monthly data reported, Uganda Viral Load Dashboard", color="Sex") +
  scale_color_manual(values=sex_colors)


# scale up in terms of three variables - used for presentation slides
# pdf(paste0(dir, '/outputs/scale_up_figure.pdf'), height=6, width=12)
# ggplot(uvl_year[variable=='Facilities Reporting' | variable=='Patients Submitting Samples' | variable=="Valid Test Results"], aes(x=date, y=value, color=sex)) + 
#   facet_wrap(~variable, scales='free_y') +
#   geom_point(size=1, alpha=0.8) +
#   geom_line(alpha=0.5) +
#   theme_minimal() +
#   labs(x="Year", y="Count", title="Monthly data reported, Uganda Viral Load Dashboard", color="Sex") +
#   scale_color_manual(values=sex_colors) +
#   theme(plot.title=element_text(size=18), 
#         plot.subtitle=element_text(vjust=-4, size=18), 
#         plot.caption=element_text(vjust=6, size=14),
#         legend.title = element_text(size=16), 
#         legend.text = element_text(size=14),
#         strip.text.x=element_text(size=14)) 
# 
# # dev.off()

# percentage of missing sex data
ggplot(sex_tot, aes(x=date, y=percent)) + 
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="% of patients whose sex is unknown", y="Date", title="Reporting completeness: percentage of patients whose sex is unknown") 
  
# ---------------
# suppression ratio maps

# suppression ratio for all years 
ggplot(coordinates, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() + 
  labs(title="Percent virally suppressed by district, Uganda", subtitle=" August 2014 - February 2018",
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
  labs(title="Annual percent virally suppressed by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# annual patients received
n <- ratio_year[ ,sum(patients_received, na.rm=T)]

ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=patients_received)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans='log', breaks=breaks, name="Patients received") + 
  theme_void() +
  labs(title="Number of patients submitting samples, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=paste('n=', n)) +
  theme(plot.title=element_text(vjust=-2), plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6))   +
  coord_fixed() 

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

# stacked bar showing suppressed/not suppressed of valid test results by year 
ggplot(table_1, aes(x=date, y=valid_results, fill='Not Suppressed')) + 
  geom_bar(stat="identity") + 
  geom_bar(aes(y=suppressed, fill='Suppressed'), stat='identity') + 
  scale_fill_manual(name='', values=bar_colors) + theme_minimal() +
  labs(title = "Virally suppressed patients", x='Date', y="Total valid test results", caption="Source: Uganda VL Dashboard")


# ---------------
# annual variable comparisons

ggplot(uvl_1[year==2014], aes(y=value, x=factor(month), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2014 Uganda Viral Load Dashboard data by sex", color="Sex") + theme_bw()

ggplot(uvl_1[year==2015], aes(y=value, x=factor(month), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2015 Uganda Viral Load Dashboard data by sex", color="Sex") + theme_bw()

ggplot(uvl_1[year==2016], aes(y=value, x=factor(month), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2016 Uganda Viral Load Dashboard data by sex", color="Sex") + theme_bw()

ggplot(uvl_1[year==2017], aes(y=value, x=factor(month), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2017 Uganda Viral Load Dashboard data by sex", color="Sex") + theme_bw()

ggplot(uvl_1[year==2018], aes(y=value, x=factor(month), color=sex, group=sex)) + 
  geom_point() + 
  geom_line(alpha=0.5) + 
  facet_wrap(~variable) +
  labs(x="Month", y="Count", title="2017 Uganda Viral Load Dashboard data by sex", color="Sex") + theme_bw()

# ---------------
# patients received by month, year
ggplot(table_1, aes(x=factor(month), y=patients_received, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + 
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") + ylab("Patients submitting samples") + 
  labs(title = "Patients submitting samples by month, year", caption="Source: Uganda VL Dashboard", colour="Sex") +
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

# total plasma samples received by month, year
ggplot(table_1, aes(x=factor(month), y=plasma_samples, col=factor(sex), group=sex)) + 
  geom_point(size=1.5) + geom_line(alpha=0.8) + theme_bw() +
  facet_wrap(~year) +
  xlab("Month") + ylab("Plasma samples received") + 
  labs(title = "Plasma samples received by month, year", caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=tri_sex)

# Plasma ratio by month, year
ggplot(table_1, aes(x=factor(month), y=plasma_ratio, col=factor(sex), group=sex)) + 
  geom_point(size=1.5, alpha=0.6) + geom_line(alpha=0.6) + theme_bw() + ylim(0,100) +
  xlab("Month") + ylab("Percent of total samples (%)") + 
  facet_wrap(~year) +
  labs(title = "Percentage of total samples that are plasma samples by month, year", 
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

# ----------------------------------------------
# MAPS

# -------------------
# facilities reporting results
n2 <- uvl[ ,length(unique(facility_name))]

# annual count of facilities reporting results
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=total_facilities)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors) + 
  theme_void() +
  labs(title="Number of facilities reporting on viral suppression, Uganda", subtitle=paste0('n=', n2),
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
  labs(title="Percentage of all facilities that have ever submitted samples reporting in a given year, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% of facilities reporting") +
  theme(plot.title=element_text(vjust=-0.5), plot.caption=element_text(vjust=6)) 


# -------------------
# log-transformed counts
  
# annual samples received
n_samp <- uvl[ ,sum(samples_received)]

ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=samples_received)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans="log", breaks=breaks, name="Samples received") + 
  theme_void() +
  labs(title="Number of samples received (log scale), Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=paste0('n=', n_samp)) +
  theme(plot.subtitle=element_text(vjust=-4), plot.caption=element_text(vjust=6)) 
  
# dbs samples received
n_dbs <- uvl[ ,sum(dbs_samples)]

ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=dbs_samples)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=gents, trans="log", breaks=breaks, name="DBS samples received") + 
  theme_void() +
  labs(title="Number of DBS samples received (log scale), Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=paste0(" n=", n_dbs)) +
  theme(plot.subtitle=element_text(vjust=-2), plot.caption=element_text(vjust=6)) 

# annual ratio of all samples that are dbs samples
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=dbs_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ladies) + 
  theme_void() +
  labs(title="Percentage of samples received that are DBS samples, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% DBS") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# annual number of valid test results
n_val <- uvl[ ,sum(valid_results)]

ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=valid_results)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=results_colors, trans="log", breaks=breaks, name="Test results") + 
  theme_void() +
  labs(title="Number of valid viral load test results, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       subtitle=paste0("n=", n_val), fill="Valid test results") +
  theme(plot.subtitle=element_text(vjust=-4), plot.caption=element_text(vjust=6))

# annual number of virally suppressed persons
n_sup <- uvl[ ,sum(suppressed)]

ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=as.numeric(suppressed))) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=sup_colors, trans="log", breaks=breaks, name="Suppressed") + 
  theme_void() +
  labs(title="Number of virally suppressed persons, Uganda", caption="Source: Uganda Viral Load Dashboard", 
        fill="Suppressed", subtitle=paste0("n=", n_sup)) +
  theme(plot.subtitle=element_text(vjust=-4), plot.caption=element_text(vjust=6)) 


dev.off()

# -----------------


# -----------------
# scale up PDF - use to determine when scale up occurred

# export as a pdf
pdf(paste0(dir, '/outputs/scale_up.pdf'), height=6, width=9)

# facilities reporting data tables
facilities_1 <- uvl[ ,.(facilities_report=length(unique(facility_id))), by=.(month, year) ]
uvl[ , .(length(unique(facility_id)))]
facilities_1[ , percent_report:=((facilities_report/2039)*100)]
facilities_1[, date:=as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d')]

# facilities reporting by month, year
ggplot(facilities_1, aes(x=date, y=facilities_report)) + 
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  labs(title="Number of facilities reporting", x="Date", y="Facilities reporting", caption="Source: Uganda Viral Load Dashboard")

# percentage of facilities reporting by month, year
ggplot(facilities_1, aes(x=date, y=percent_report)) + 
  geom_point() +
  geom_line(alpha=0.6) +
  theme_bw() +
  labs(title="Percentage of facilities reporting", x="Date", y="Percentage of facilities reporting (%)", caption="Source: Uganda Viral Load Dashboard")

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

#-----------------------------

#--------------------
# TERG SLIDES - Outputs for the May TERG Presentation

terg1 <- uvl[, .(patients_received=sum(patients_received)),
                   by=.(sex, date)]

# create a table of the total number of facilities reporting in each district, all years
total_fac_year <- uvl[, .(total_facilities=as.numeric(length(unique(facility_id)))), by=.(date)]
terg <- merge(terg1, total_fac_year, by="date", all.x=TRUE)

# reshape long
terg <- melt(terg, id.vars=c("sex","date"))

# keep single values for facilities (females only)
terg <- terg[!(variable=="total_facilities" & (sex=="Male"| sex=="Unknown")) ]
terg <- terg[variable=="total_facilities", sex:="Facility"]

# label the variables for graph titles and put the graphs in an intuitive order
terg$variable <- factor(terg$variable, 
                        levels=c("patients_received", "total_facilities"), 
                        labels=c("Patients submitting samples for VL testing", "Facilities reporting VL tests performed"))

terg$sex <- factor(terg$sex, levels=c("Female", "Male", "Facility"),
                   labels=c("Females", "Males", "Facilities"))

# melt terg1 as an alternative
terg1 <- melt(terg1, id.vars=c("sex","date"))
# label the variables for graph titles and put the graphs in an intuitive order
terg1$variable <- factor(terg1$variable, 
                         levels=c("patients_received"), 
                         labels=c("Patients submitting samples for VL testing"))

terg1$sex <- factor(terg1$sex, levels=c("Female", "Male"),
                    labels=c("Females", "Males"))
total_fac_year[, sex:='Purple']

#--------------------
# subset coordinates_year to 2017 and 2018 only

coordinates_year <- coordinates_year[year==2017 | year==2018]

#--------------------

# export a PDF og the graphs needed for the TERG meeting

pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/uvl_terg.pdf', height=6, width=9)

#graph of facilities reporting and patients submitting samples for Vl testing
#annual data reported for major variables
ggplot(terg, aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Date", y="Count", color="Sex") +
  scale_color_manual(values=sex_colors)

# alternative graphs
ggplot(terg1, aes(x=date, y=value, color=sex)) + 
  facet_wrap(~variable, scales='free_y') +
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(x="Date", y="Count", color="Sex") +
  scale_color_manual(values=sex_colors)

col <- c('#3f007d')

ggplot(total_fac_year, aes(x=date, y=total_facilities, color=sex)) + 
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() +
  labs(title="Total facilities reporting viral toad tests performed by month", x="Date", y="Count", color="Sex") + 
  scale_color_manual(values=col)


# annual suppression ratios
ggplot(coordinates_year[year==2018], aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Viral suppression ratios by district, Uganda", fill="Percent virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


gents <- brewer.pal(6, 'Blues')

# breaks for log transformation legends
breaks <- c(1, 20, 400, 8100)


breaks <- c(1, 20000, 30000, 50000)

pdf(paste0(dir, 'mapss.pdf'), height=6, width=19)

# annual patients received
ggplot(coordinates_year[year==2018], aes(x=long, y=lat, group=group, fill=patients_received)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=gents, name="Patients", breaks=breaks ) + 
  theme_void() +
  labs(title="Patients submitting samples for viral load testing", caption="Source: Uganda Viral Load Dashboard") +
 theme(plot.title=element_text(vjust=-4, size=22), 
      plot.caption=element_text(vjust=6, size=14),
      legend.title = element_text(size=16), 
      legend.text = element_text(size=14)) 
  


dev.off()

#-----------------------

# info for describing the graph

x <- ratio_year[year==2017, .(suppression_ratio, patients_received), by=.(district_name, id)]
x <- x[order(suppression_ratio)]

#-----
# alternative graphs

level_ratio <- uvl[!is.na(level),.(suppression_ratio=100*(sum(suppressed)/sum(valid_results))),
                            by=.(date, level)]

# melt terg1 as an alternative
level1 <- melt(level_ratio, id.vars=c("date","level"))

# alternative graphs
ggplot(level1, aes(x=date, y=value, color=factor(level), group=level)) + 
  geom_point(size=1, alpha=0.8) +
  geom_line(alpha=0.5) +
  theme_minimal() 

  labs(x="Date", y="Count", color="Sex") +
  scale_color_manual(values=sex_colors)


# label the variables for graph titles and put the graphs in an intuitive order
level1$variable <- factor(terg1$variable, 
                         levels=c("patients_received"), 
                         labels=c("Patients submitting samples for VL testing"))

terg1$sex <- factor(terg1$sex, levels=c("Female", "Male"),
                    labels=c("Females", "Males"))
total_fac_year[, sex:='Purple']

# slide for the board of directors meeting
pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscrape_agg/outputs/board_vl.pdf', height=6, width=9)

# suppression ratio for all years 
ggplot(coordinates_year[year==2018], aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() + 
  labs(title="Viral suppression ratios by district, Uganda", subtitle="January - April 2018",
       caption="Source: Uganda Viral Load Dashboard", fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-4, size=24), plot.subtitle=element_text(vjust=-4, size=16), 
        plot.caption=element_text(vjust=6, size=15), legend.title=element_text(size=16, hjust=0.5)) + coord_fixed()

dev.off()

graph <- uvl[ ,.(ratio=100*(sum(suppressed)/sum(valid_results))), by=.(date, sex, month, year)]

y_tho <-c('#756bb1', '#66c2a4', '#bd0026' )

pdf(paste0(dir, '/ratio.pdf'), height=6, width=9)

ggplot(graph, aes(x=date, y=ratio, color=sex)) +
  geom_point() +
  geom_line() +
  labs(x="Date", y="Percent virally supperessed (%)", color="Sex",
       caption='Source: Uganda Viral Load Dashboard') +
  scale_color_manual(values=y_tho) +
  theme_minimal() +
  theme(legend.title = element_text(size=16), 
        plot.caption=element_text(vjust=6, size=14),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14), 
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=14)) 

dev.off()


