
# ----------------------------------------------
# Irena Chen
#
# 2/22/2018

### This code is to make the actual maps of Guatemala's municipalities using shape files and SICOIN data: 

# ----------------------------------------------


# raster package is most of what you need
library(rgeos)
library(raster)
library(ggplot2)
library(maptools)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(readxl)
library(reshape)
library(scales)


# ----------------------------------------------
# change directory
setwd('J:/Project/Evaluation/GF/mapping/gtm/')

# load the shapefile
shapeData = shapefile('GTM_munis_only.shp')

## load the sicoin data:

sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                   ,fileEncoding="latin1"))
sicoin_data$year <- year(sicoin_data$start_date)


##let's do HIV first since we have that at the yearly level (may have to work around w/ Malaria data)

hivData <- sicoin_data[disease=="hiv"]

## something that might be cool is to get the absorption ratio of disb/budget by year, source, disease, etc. 
byVars = names(hivData)[names(hivData)%in%c('source', 'loc_id', 'loc_name', 'year')]
hivData  = hivData [, list(budget=sum(budget), disbursement=sum(disbursement), expenditure=sum(expenditure)), 
                          by=byVars]

##do a sum of sicoin data to get data by muni, year, disease, and source

hivData <- hivData[with(hivData, order(source, loc_name, year)), ]
hivData[, cumulative_budget:=cumsum(budget), by=c('source', 'loc_name', 'year')]
hivData[, cumulative_disbursement:=cumsum(disbursement),by=c('source', 'loc_name', 'year')]
hivData[, absorption:=cumulative_disbursement/cumulative_budget]


gheData <- hivData[source=="ghe"]

# ----------------------------------------------
## get the important info from the shape data files - lat/long coordinates, muni codes, etc.

# shapeData is a spatialpolygonsdataframe
class(shapeData)

# these have plot methods
plot(shapeData)

# simplify the shape data (could create little gaps, maybe don't do this)
shapeData2 = gSimplify(shapeData, tol=0.01, topologyPreserve=TRUE)

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
# use IDs instead of names
coordinates = data.table(fortify(shapeData, region='Codigo'))
coordinates$id <- as.numeric(coordinates$id)

# merge on municipality names
names = data.table(shapeData@data)
coord_and_names = merge(coordinates, names, by.x='id', by.y='Codigo', allow.cartesian=TRUE)

# ----------------------------------------------
## let's do the government health expenditures first: 
# the sicoin loc_id has leading zeroes, which the shape files don't have
## this gets rid of it: 
gheData$id <- as.numeric(lapply(gheData$loc_id, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE)))

## do a histogram of GHE budget values: 
hist(gheData$budget/1000000)
boxplot(gheData$budget)


## get summary statistics of the budget/disb data:
library(pastecs)
stat.desc(gheData$budget) 
quantile(gheData$absorption, na.rm=TRUE)
quantile(gheData$budget, na.rm=TRUE)

##let's make buckets of the budget variable: 
library(Hmisc)
gheData$binned_absorption<- cut(gheData$absorption, breaks=c(0,0.003352666, 0.797546032,  0.991525912, 12.616891166), labels=c("0-0.03", "0.03-0.8","0.8-0.99", "0.99+"))



# merge on the data (all.x=TRUE so the shapefile data doesn't disappear)
graphData <- merge(coord_and_names, gheData,by='id', all.x=TRUE, allow.cartesian=TRUE)

graphData <- graphData[!is.na(year)]
graphData<- graphData[with(graphData, order(year)), ]


# draw the polygons using ggplot2

gtm_plots <- list()
i = 1
for (k in unique(graphData$year)){
  plot <- (ggplot(graphData[year==k], aes(x=long, y=lat, group=group, fill=as.numeric(binned_absorption))) + 
  	geom_path() + 
  	geom_polygon() +
  	theme_void() +
  	  scale_fill_gradient(low = "#bfefd1", high = "#14aa4b", space = "Lab", na.value="grey60",
  	                    labels=levels(graphData[year==k]$binned_absorption)
  	                    , breaks = c(1,2,3,4)
  	                    ) + 
    labs(title=paste(k, "HIV Absorption of GHE"), fill='Disbursement/Budget  (Cumulative)'))
  gtm_plots[[i]] <- plot
  i=i+1
}

pdf("H:/rt_data/testing.pdf", height=6, width=9)
invisible(lapply(gtm_plots, print))
dev.off()

	
	
