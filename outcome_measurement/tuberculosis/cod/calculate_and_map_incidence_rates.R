# Audrey Batzel
# 10/1/2018
#
# Incidence rates for TB in DRC based on PNLT

# ----------------------------------------------
## Set up R / install packages 
# ----------------------------------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(readxl)
library(rlang)
library(zoo)
library(lubridate)
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(maptools)
library(scales)
library(ggrepel)
library(naniar)
library(grid)
library(gridExtra)

setwd("C:/local/gf")
# ----------------------------------------------


# ----------------------------------------------
## Set up the directories to read/export files: 
# ----------------------------------------------
# Directories
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j/')

output_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLT_data/')
data_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/")
worldpop_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/worldpop_data/")
shape_dir <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/gadm36_COD_shp/")
source('./core/standardizeDPSNames.R')

# input files
data_2017 <- "PNLT_case_screening_2017.csv"
pop_estimates_2017 <- "2017_worldpop_DRC_DPS.csv"
dps_shapefile <-  "gadm36_COD_1.shp"

# output files
graphFile <- paste0(output_dir, "PNLT TB Incidence Rates 2017.pdf")
graphFile_justPNLTpop <- paste0(output_dir, "PNLT TB Incidence Rates 2017 - pop estimates from PNLT data.pdf")
# ----------------------------------------------


# ----------------------------------------------
## Load in prepped data & shapefile / set up
# ----------------------------------------------
dt <- read.csv(paste0(data_dir, data_2017))
dt <- as.data.table(dt)

drcShape <- shapefile(paste0(shape_dir, dps_shapefile)) # shapefile with all DPS (use a different one for health zones)
coordinates = as.data.table(fortify(drcShape, region='NAME_1'))
coordinates$id <- standardizeDPSNames(coordinates$id)

# subset dt
dt_cases <- dt[, .(dps, tpp_new, tpp_relapse, tpc_new, tpc_relapse, tep_new, tep_relapse, tot_incCase)]
dt_pop <- dt[, .(dps, pop_tot, pop_covered)]

# calculate annual numbers (collapse data by dps)
dt_cases <- dt_cases[, lapply(.SD, sum, na.rm=TRUE), by = dps, .SDcols = colnames(dt_cases)[2:length(colnames(dt_cases))]]
dt_pop <- dt_pop[, lapply(.SD, mean, na.rm=TRUE), by = dps, .SDcols = colnames(dt_pop)[2:length(colnames(dt_pop))]]

dt <- merge(dt_cases, dt_pop, by="dps")

# combine kongo-central-est and kongo-central-ouest into kongo-central
dt[dps %in% c("kongo-central-est", "kongo-central-ouest"), dps:= "kongo-central"]
dt <- dt[, lapply(.SD, sum, na.rm=TRUE), by = dps, .SDcols = colnames(dt)[2:length(colnames(dt))]]
# ----------------------------------------------


# ----------------------------------------------
## Load in world pop data
# ----------------------------------------------
dt_pop <- data.table(read.csv(paste0(worldpop_dir, pop_estimates_2017)))

dt_pop$X <- NULL
dt_pop$dps <- as.character(dt_pop$dps)
dt_pop$dps <- standardizeDPSNames(dt_pop$dps)

# merge prepped tb data wirth world pop data
dt <- merge(dt, dt_pop, by="dps")
setnames(dt, "pop_tot", "pop_tot_pnlt")
setnames(dt, "population", "pop_worldpop")
setnames(dt, "pop_covered", "pop_covered_pnlt")
# ----------------------------------------------


# ----------------------------------------------
## Calculate incidence rates
# ----------------------------------------------
dt[, inc_pop_tot_pnlt := ((tot_incCase/pop_tot_pnlt)*100000)]
dt[, inc_pop_cov_pnlt := ((tot_incCase/pop_covered_pnlt)*100000)]
dt[, inc_pop_worldpop := ((tot_incCase/pop_worldpop)*100000)]
# ----------------------------------------------


# ----------------------------------------------
## Map incidence rates for 2017 using PNLT
# ----------------------------------------------
graphData <- merge(coordinates, dt, by.x='id', by.y='dps', all=TRUE, allow.cartesian=TRUE)
graphData[inc_pop_tot_pnlt=="Inf", inc_pop_tot_pnlt := NA]
graphData[inc_pop_cov_pnlt=="Inf", inc_pop_cov_pnlt := NA]

drc_region_centroids <- data.frame(long = coordinates(drcShape)[, 1],lat = coordinates(drcShape)[, 2])
drc_region_centroids[, 'NAME_1'] <-drcShape@data[,'NAME_1']
drc_region_centroids$NAME_1 <- standardizeDPSNames(drc_region_centroids$NAME_1)

max = max(graphData$inc_pop_tot_pnlt, na.rm=TRUE)
min = min(graphData$inc_pop_tot_pnlt, na.rm=TRUE)
med = 180

map <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=inc_pop_tot_pnlt)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = med, ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  labs(title="TB Case Notification Rate 2017", 
       fill='Case notifications \nper 100,000 people',
       caption = "[Note: Ituri missing population data]") + 
  #geom_label_repel(data = drc_region_centroids, aes(label = NAME_1, x = long, y = lat, group = NAME_1), size = 3) +
  theme(plot.title = element_text(size = 20), legend.title = element_text(size=16), plot.caption =  element_text(size=16))
map

pdf(graphFile_justPNLTpop, height=9, width=12)
print(map)
dev.off()
# ----------------------------------------------

# ----------------------------------------------
## Comparison of denominators for popultaion - map incidence rates for 2017
# ----------------------------------------------
# note ------ # use these for standardizing scale
max = max(graphData$inc_pop_worldpop, na.rm=TRUE)
min = min(graphData$inc_pop_worldpop, na.rm=TRUE)
med = max/2

map1 <- ggplot() + 
       geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=inc_pop_worldpop)) + 
       coord_equal() +
       geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
       theme_void() +  
       scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = med, ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
       labs(title="Worldpop 2017 population estimates", 
            fill='Incidence per 100,000 people') 
map1
#       + geom_label_repel(data = drc_region_centroids, aes(label = NAME_1, x = long, y = lat, group = NAME_1), size = 3)


# max = max(graphData$inc_pop_tot_pnlt, na.rm=TRUE)
# min = min(graphData$inc_pop_tot_pnlt, na.rm=TRUE)
# med = max/2

map2 <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=inc_pop_tot_pnlt)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = med, ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  labs(title="Population estimates from PNLT", 
       fill='Incidence per 100,000 people')

map2

# max = max(graphData$inc_pop_cov_pnlt, na.rm=TRUE)
# min = min(graphData$inc_pop_cov_pnlt, na.rm=TRUE)
# med = max/2

map3 <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=inc_pop_cov_pnlt)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradient2(low='#9aeaea', mid='#216fff', high='#0606aa', 
                       na.value = "grey70", space = "Lab", midpoint = med, ## play around with this to get the gradient that you want, depending on data values 
                       breaks=round(seq(0, max, by=((max-min)/4))), 
                       limits=c(0,max)) +
  labs(title="'Population covered' estimates from PNLT", 
       fill='Incidence per 100,000 people')

map3

# Put maps together
p1 = arrangeGrob(map1, map2, map3, ncol=2, nrow=2, top= textGrob("TB Incidence Rates 2017 \n from PNLT incident cases and various denominators:", gp=gpar(fontsize=20,font=1)))

# Save graph
pdf(graphFile, height=10, width=15)
  grid.newpage()
  grid.draw(p1)
dev.off()
# ----------------------------------------------


# ----------------------------------------------
dt_graph <- dt[, .(dps, inc_pop_tot_pnlt, inc_pop_cov_pnlt, inc_pop_worldpop)]
dt_graph <- melt(dt_graph, id_vars="dps")
dt_graph[ variable == "inc_pop_cov_pnlt", order:= 1]
dt_graph[ variable == "inc_pop_tot_pnlt", order:= 2]
dt_graph[ variable == "inc_pop_worldpop", order:= 3]

g <- ggplot(dt_graph, aes(order, value, color=dps, label=dps)) + theme_bw() + 
  geom_point() + 
  geom_line(aes(order, value, color=dps)) +
  xlab( "PNLT population covered                                 PNLT population total                                Worldpop 2017 estimate") +
  ggtitle( "Incidence Rates by DPS, calculated using PNLT data for number of incident cases \nand different denominators for population") +
  geom_label() + guides(color=FALSE)
g

pdf(paste0(output_dir, "Graph comparing denominators for incidence rates.pdf"), height=8, width=9)
print(g)
dev.off()

dt_1 <- setorder(dt, inc_pop_cov_pnlt)
p1 <- paste0( dt_1$dps, " (", dt_1$inc_pop_cov_pnlt, ")")

dt_2 <- setorder(dt, inc_pop_tot_pnlt)
p2 <- paste0( dt_2$dps, " (", dt_2$inc_pop_tot_pnlt, ")")
  
dt_3 <- setorder(dt, inc_pop_worldpop)
p3 <- paste0( dt_3$dps, " (", dt_3$inc_pop_worldpop, ")")

data <- data.table(pop_covered_from_pnlt= p1, pop_tot_from_pnlt= p2, 
                   pop_from_worldpop= p3)

write.csv(data, paste0(data_dir, "comparison_of_incidence_rates_with_different_denominators.csv"))

