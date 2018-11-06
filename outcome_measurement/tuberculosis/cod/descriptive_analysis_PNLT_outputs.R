# ----------------------------------------------
# Audrey Batzel
# 10/03/18
# Descriptive analysis of PNLT outcomes
# ----------------------------------------------
## Set up R / install packages    
# ----------------------------------------------
rm(list=ls())
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(maptools)
library(grid)
library(gridExtra)
library(RColorBrewer)
setwd('C:/local/gf/')
source('./core/standardizeDPSnames_function.R')
# ----------------------------------------------


# ----------------------------------------------
## set up the directories to read/export files: 
# ----------------------------------------------
# Directories
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j/')
output_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLT_data/')
data_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/")
shape_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/gadm36_COD_shp/")

# input files
pnlt_case_screening_17 <- "PNLT_case_screening_2017.csv"
pnlt_case_screening_18 <- "PNLT_case_screening_2018.csv"
dps_shapefile <-  "gadm36_COD_1.shp"
hz_shapefile <- "health2.shp"

# output files
tests_national <- "outputs/TS - Tests completed - national.pdf"
# ----------------------------------------------


# ----------------------------------------------
## load data / prep continued (move to prep code)
# ----------------------------------------------
dt17 <- read.csv(paste0(data_dir, pnlt_case_screening_17), stringsAsFactors = FALSE)
dt17 <- as.data.table(dt17)

dt18 <- read.csv(paste0(data_dir, pnlt_case_screening_18), stringsAsFactors = FALSE)
dt18 <- as.data.table(dt18)
#---- changed in pnlt_translations doc, rerun prep code to change in prepped data-
setnames(dt18, "ziehl_comp", "tbPresumed_ziehl_comp")
setnames(dt18, "ziehl_pos", "tbPresumed_ziehl_pos")
setnames(dt18, "ziehl_comp.1", "ziehl_comp")
setnames(dt18, "ziehl_pos.1", "ziehl_pos")

dt <- rbindlist(list(dt17, dt18), use.names=TRUE, fill= TRUE)
dt$date <- as.Date(dt$date)

file_path <- paste0(shape_dir, dps_shapefile)
drcShape <- shapefile(file_path) # shapefile with all DPS (use a different one for health zones)
# ----------------------------------------------


# ----------------------------------------------
## make graphs
# ----------------------------------------------
dps_names <- unique(dt$dps)

# Number of diagnostic tests performed
dt_graph <- dt[, .(dps, ziehl_comp, xpert_comp, date, file_year, quarter)]
dt_graph[, tests_comp := ziehl_comp + xpert_comp]
dt_graph[, tests_comp_COD := sum(tests_comp, na.rm=TRUE), by="date"]
dt_graph_exc_missing <- dt_graph[! dps %in% c("kinshasa", "ituri", "mai-ndombe", "lualaba", "tshopo"),  .(tests_comp_COD_exc_missing = sum(tests_comp, na.rm=TRUE)), by="date" ]
# ----------------------------------------------

# ----------------------------------------------
pdf(paste0(output_dir, "outputs/TS - Tests completed - all DPS.pdf"), height=9, width=11)
g <- ggplot(dt_graph, aes(date, tests_comp, color=dps) ) + theme_bw() + 
  geom_point() + 
  geom_line() + xlab("Date") + ylab("Count") +
  ggtitle("Number of tests completed (Xpert and Ziehl/Auramine)") +
  scale_y_continuous(labels= scales:: comma) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20))
g
dev.off()

pdf(paste0(output_dir, "outputs/TS - Tests completed - by DPS with standard scale.pdf"), height=9, width=11)
for (d in dps_names){
  g <- ggplot(dt_graph[dps==d, ], aes(date, tests_comp) ) + theme_bw() + 
    geom_point(color = "darkblue") + 
    geom_line(color = "darkblue") + xlab("Date") + ylab("Count") +
    ggtitle(paste0(d, ": Number of tests completed (Xpert and Ziehl/Auramine)")) +
    ylim(0, 25000) +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
          legend.text =element_text(size=14), plot.title = element_text(size=20))
  print(g)
}
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# Graphs for report
# ----------------------------------------------
# Make year - quarter an ordered factor for x axis
dt_graph$quarter <- as.character(dt_graph$quarter)
dt_graph$quarter <- paste0("Q", dt_graph$quarter)
dt_graph[ , yr_qtr:= paste(file_year, quarter, sep="-")]

dt_graph$yr_qtr_ordered <- factor(dt_graph$yr_qtr, ordered = TRUE, 
                                levels = c("2017-Q1", "2017-Q2", "2017-Q3", "2017-Q4", "2018-Q1"))

dt_graph_exc_missing <- dt_graph[! dps %in% c("kinshasa", "ituri", "mai-ndombe", "lualaba", "tshopo"),  .(tests_comp_COD_exc_missing = sum(tests_comp, na.rm=TRUE)), by="yr_qtr_ordered"]

pdf(paste0(output_dir, tests_national), height=9, width=11)
g <- ggplot(dt_graph[], aes(yr_qtr_ordered, tests_comp_COD, group=1)) + theme_bw() + 
  geom_point() + 
  geom_line() + xlab("Date") + ylab("Count") +
  ggtitle("Number of TB tests completed (Xpert and Ziehl - Neelson/Auramine)") + 
  scale_y_continuous(labels= scales:: comma) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  labs(caption= "NOTE:  Kinshasa and Ituri missing data all of 2017; \nMai-Ndombe, Lualaba, and Tshopo missing data 2018")
g

g <- ggplot(dt_graph_exc_missing, aes(yr_qtr_ordered, tests_comp_COD_exc_missing, group = 1) ) + theme_bw() + 
  geom_point() + 
  geom_line() + xlab("Date") + ylab("Count") +
  ggtitle("Number of TB tests completed (Xpert and Ziehl - Neelson/Auramine)") + 
  scale_y_continuous(labels= scales:: comma) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  labs(caption= "NOTE:  Kinshasa, Ituri, Mai-Ndombe, Lualaba, and Tshopo excluded")
g
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# Number of notified cases of all forms of TB (i.e. bacteriologically confirmed + clinically diagnosed) Includes new and relapse cases
dt_graph <- dt[, .(dps, date, tpp_new, tpp_relapse, tpc_new, tpc_relapse, tep_new, tep_relapse, tot_incCase)]
dt_graph[, cases_new := tpp_new + tpc_new + tep_new]
dt_graph[, cases_relapse := tpp_relapse + tpc_relapse + tep_relapse]
dt_graph[, incCases_COD := sum(tot_incCase), by="date"]

pdf(paste0(output_dir, "outcomes/TS - Number of notified cases - all DPS.pdf"), height=9, width=11)
g <- ggplot(dt_graph, aes(date, tot_incCase, color=dps) ) + theme_bw() + 
  geom_point() + 
  geom_line() + xlab("Date") + ylab("Number of Cases") +
  ggtitle(" Number of notified cases of all forms of TB (i.e. bacteriologically confirmed + \nclinically diagnosed + extrapulmonary TB); Includes new and relapse cases") +
  scale_y_continuous(labels= scales:: comma) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20))
g
dev.off()

pdf(paste0(output_dir, "outcomes/TS - Number of notified cases - by DPS with standard scale.pdf"), height=9, width=11)
for (d in dps_names){
  g <- ggplot(dt_graph[dps==d, ], aes(date, tot_incCase) ) + theme_bw() + 
    geom_point(color = "darkblue") + 
    geom_line(color = "darkblue") + xlab("Date") + ylab("Number of Cases") +
    ggtitle(paste0(d, ": Number of notified cases of all forms of TB; includes new and relapse cases")) +
    ylim(0, 6200) +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
          legend.text =element_text(size=14), plot.title = element_text(size=20))
  print(g)
}
dev.off()

pdf(paste0(output_dir, "outcomes/TS - Number of notified cases - national.pdf"), height=9, width=11)
g <- ggplot(dt_graph, aes(date, incCases_COD) ) + theme_bw() + 
  geom_point() + 
  geom_line() + xlab("Date") + ylab("Number of Cases") +
  ggtitle("National: number of notified cases of all forms of TB (i.e. bacteriologically confirmed + \nclinically diagnosed + extrapulmonary TB); Includes new and relapse cases") +
  scale_y_continuous(labels= scales:: comma) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20))
g
dev.off()

dt_graph_type <- dt_graph[, .(dps, date, cases_new, cases_relapse)]
dt_graph_type <- melt.data.table(dt_graph_type, id.vars=c("dps", "date"))
dt_graph_type[, value_COD := sum(value), by=c("date", "variable")]

pdf(paste0(output_dir, "outcomes/TS - Notified cases stratified by new vs. relapse - by DPS with standard scale.pdf"), height=9, width=11)
for (d in dps_names){
  g <- ggplot(dt_graph_type[dps==d, ], aes(date, value, color=variable) ) + theme_bw() + 
    geom_point() + 
    geom_line() + xlab("Date") + ylab("Number of Cases") +
    ggtitle(paste0(d, ": Number of notified cases of all forms of TB: new and relapse cases")) +
    ylim(0, 5400)+
    theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
          legend.text =element_text(size=14), plot.title = element_text(size=20))
  print(g)
}
dev.off()

pdf(paste0(output_dir, "outcomes/TS - Notified cases stratified by new vs. relapse - national.pdf"), height=9, width=11)
g <- ggplot(dt_graph_type, aes(date, value_COD, color=variable)) + theme_bw() + 
  geom_point() + 
  geom_line() + xlab("Date") + ylab("Number of Cases") +
  ggtitle(paste0("National: number of notified cases of all forms of TB: new and relapse cases")) +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20))
print(g)
dev.off()

# Percentage of new and relapse TB patients tested using WHO recommended rapid tests at the time of diagnosis:
dt_graph <- dt[, .(dps, date, ziehl_comp, xpert_comp, tot_incCase, tb_pres)]
dt_graph[, percentage_rapid_tests := xpert_comp/tb_pres]
dt_graph[, xpert_comp_COD := sum(xpert_comp, na.rm=TRUE), by="date"]
dt_graph[, tb_pres_COD := sum(tb_pres, na.rm=TRUE), by="date"]
dt_graph[, percentage_rapid_tests_COD := xpert_comp_COD/tb_pres_COD, by="date"]

pdf(paste0(output_dir, "activities/TS - Proportion of presumed TB patients tested using xpert - all DPS.pdf"), height=9, width=11)
g <- ggplot(dt_graph, aes(date, percentage_rapid_tests, color=dps) ) + theme_bw() + 
  geom_point() + 
  geom_line() + xlab("Date") + ylab("Proportion") +
  ggtitle("Proportion of presumed TB patients tested using xpert") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20))
g
dev.off()

pdf(paste0(output_dir, "activities/TS - Proportion of presumed TB patients tested using xpert - by DPS with standard scale.pdf"), height=9, width=11)
for (d in dps_names){
  g <- ggplot(dt_graph[dps==d, ], aes(date, percentage_rapid_tests, color=dps) ) + theme_bw() + 
    geom_point() + 
    geom_line() + xlab("Date") + ylab("Proportion") +
    ggtitle(paste0(d, ": Proportion of presumed TB patients tested using xpert")) +
    ylim(0, 0.55)+
    theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
          legend.text =element_text(size=14), plot.title = element_text(size=20))
  print(g)
}
dev.off()

pdf(paste0(output_dir, "activities/TS - Percentage of presumed TB patients tested using xpert - national.pdf"), height=9, width=11)
g <- ggplot(dt_graph, aes(date, percentage_rapid_tests_COD*100) ) + theme_bw() + 
  geom_point() + 
  geom_line() + xlab("Date") + ylab("Percentage") +
  ggtitle("Percentage of presumed TB patients tested using xpert") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20))
g
dev.off()
# ----------------------------------------------


# ----------------------------------------------
## make maps
# ----------------------------------------------
# fortify the shapefile to get coordinates and convert it to a data table / standardize dps names for merging
# ----------------------------------------------
coordinates = as.data.table(fortify(drcShape, region='NAME_1'))
coordinates$id <- standardizeDPSnames(coordinates$id)
# this name doesn't save properly so you may have to re-copy and paste the name from the shapefile
coordinates$id <- gsub("ã???equateur", "equateur", coordinates$id)

# ----------------------------------------------
# calculate annual numbers (collapse data by dps)
# ----------------------------------------------
# add the variables to make maps of here:
variables_to_analyze <- c('xpert_comp', 'ziehl_comp')
variables_to_analyze <- c(variables_to_analyze, "tot_incCase")
# subset to just 2017 and the variables to make maps of
dt_annual <- dt[file_year == "2017", c("dps", "date", variables_to_analyze), with=FALSE]
# combine kongo-central-est and kongo-central-ouest into kongo-central
dt_annual[ dps %in% c("kongo-central-est", "kongo-central-ouest"), dps:= "kongo-central"]
# sum by dps over quarters to get an annual value for each variable
dt_annual <- dt_annual[, lapply(.SD, sum, na.rm=TRUE), by = dps, .SDcols = variables_to_analyze]
# ----------------------------------------------
# Mapping:
# ----------------------------------------------

# Number of diagnostic tests performed
dt_annual[, totTests := xpert_comp + ziehl_comp]

graphData <- merge(coordinates, dt_annual, by.x='id', by.y='dps', all=TRUE, allow.cartesian=TRUE)

max = max(graphData$ziehl_comp, na.rm=TRUE)
min = min(graphData$ziehl_comp, na.rm=TRUE)
med = max/2
# ********************************
# get center of coordinates
# make labels centered for each DPS 
centers = data.table(coordinates(drcShape))
setnames(centers, c('long', 'lat'))
centers[ , dps:=unique(drcShape@data$NAME_1)]
centers$dps <- standardizeDPSnames(centers$dps)
# this name doesn't save properly so you may have to re copy
centers$dps <- gsub("ã???equateur", "equateur", centers$dps)

centers$long <- round(centers$long, digits=3)
centers$lat <- round(centers$lat, digits=3)
# merge with data to get the values to include in the labels
centers <- merge(centers, dt_annual, by="dps")

centers[totTests==0, totTests:=NA]
graphData[totTests==0, totTests:=NA]

max = max(graphData$totTests, na.rm=TRUE)
min = min(graphData$totTests, na.rm=TRUE)
med = max/2
# total tests performed with bubble map of case notifications
m3 <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=totTests)) + 
  geom_point(data=centers, aes(x=long, y=lat, size=tot_incCase), alpha=0.9, shape=21, fill="maroon") +
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = (med*1.5), ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  scale_size_continuous(range=c(1,8)) +
  labs(title="2017 Tests completed (Xpert and Ziehl - Neelson/Auramine) vs incident TB cases notified", 
       fill='Number of tests completed', size= "Number of cases notified")
m3

pdf(paste0(output_dir, "outputs/Map - Tests completed with Case Notifications.pdf"), height=9, width=12)
m3
dev.off()
# ********************************

m1 <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=ziehl_comp)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = (med*1.5), ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  labs(title="Ziehl/Auramine Tests", 
       fill='Number completed')
m1

max = max(graphData$xpert_comp, na.rm=TRUE)
min = min(graphData$xpert_comp, na.rm=TRUE)
med = max/2

m2 <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=xpert_comp)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = (med*1.5), ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  labs(title="Xpert Tests", 
       fill='Number completed')
m2

max = max(graphData$totTests, na.rm=TRUE)
min = min(graphData$totTests, na.rm=TRUE)
med = max/2

m3 <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=totTests)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = (med*1.5), ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  labs(title="Total Tests", 
       fill='Number completed')
m3

  # Put maps together
  p1 = arrangeGrob(m1, m2, m3, ncol=2, nrow=2)
  
  # Save graph
  pdf(paste0(output_dir, "activities/Map - Tests completed.pdf"), height=10, width=15)
  grid.newpage()
  grid.draw(p1)
  dev.off()
  

# Number of notified cases of all forms of TB (i.e. bacteriologically confirmed + clinically diagnosed) Includes new and relapse cases
graphData <- merge(coordinates, dt_annual, by.x='id', by.y='dps', all=TRUE, allow.cartesian=TRUE)

max = max(graphData$tot_incCase, na.rm=TRUE)
min = min(graphData$tot_incCase, na.rm=TRUE)
med = max/2



pdf(paste0(output_dir, "outcomes/Map - Number of incident cases; all forms TB.pdf"), height=9, width=11)
m2 <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=tot_incCase)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = (med), ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  labs(title="Total case notifications- new and relapse cases \nall forms TB, 2017")
m2
dev.off()
