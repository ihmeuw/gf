# -------------------------------------------------------
# David Phillips
# 
# 12/28/2018
# Example of map-making at HZ level with SNIS
# -------------------------------------------------------


# -------------------------------------------------------
# Set up R
rm(list=ls()) 

# load packages
library(data.table) 
library(raster) 
library(rgeos)
library(ggplot2) 
library(RColorBrewer) 
library(gridExtra)
library(grid)

# set directory 
dir = 'J:/Project/Evaluation/GF/results_chains/cod/malaria'
dir = 'C:/local/GF_copy_01032019/results_chains/cod/malaria'
setwd(paste0(dir, '/prepped_data'))

# output file
outFile = paste0(dir, '/visualizations/output_maps.pdf')
# -------------------------------------------------------


# ---------------------------------------------------------------------
# Import base data 

# load data 
data = readRDS('snis_base_data_prepped_for_Constant.rds')

# aggregate to health zone level by year and indicator
byVars = c('former_dps_name','health_zone','year','element')
data = data[, sum(value, na.rm=TRUE), by=byVars]

# rename
setnames(data, 'V1', 'value')

# keep only 2017 since 2018 is incomplete
data = data[year==2017]
# ---------------------------------------------------------------------


# ---------------------------------------------------------------------
# Import sigl data 

# load data 
sigl = readRDS('snis_sigl_data_prepped_for_Constant.rds')

# aggregate to health zone level by year and indicator
byVars = c('former_dps_name','health_zone','year','element')
sigl = sigl[, sum(value, na.rm=TRUE), by=byVars]

# rename
setnames(sigl, 'V1', 'value')

# keep only 2017 since 2018 is incomplete
sigl = sigl[year==2017]
# ---------------------------------------------------------------------


# --------------------------------------
# Subset data 

# subset to just one variable
elementsUncomplicated = c('A 1.4 Paludisme simple confirmé traité [PN]', 'A 1.5 Paludisme simple confirmé traité selon PN-FE')
elementsSevere = c('A 1.5 Paludisme grave traité FE', 'A 1.4 Confirmed simple malarA 1.4 Paludisme grave traité')
elementsRDT = c('A 1.4 TDR réalisé')
elementsSP = c('A 2.1 Sulfadox. + Pyrimét 1ère dose reçue')
elementsITN= c('C1 12.1 MIILD - pièce - quantité consommée')
subset1 = data[element %in% elementsUncomplicated]
subset2 = data[element %in% elementsSevere]
subset3 = data[element %in% elementsRDT]
subset4 = data[element %in% elementsSP]
subset5 = sigl[element %in% elementsITN]

# add together both elements
byVars = c('former_dps_name','health_zone','year')
subset1 = subset1[, sum(value, na.rm=TRUE), by=byVars]
subset2 = subset2[, sum(value, na.rm=TRUE), by=byVars]
subset3 = subset3[, sum(value, na.rm=TRUE), by=byVars]
subset4 = subset4[, sum(value, na.rm=TRUE), by=byVars]
subset5 = subset5[, sum(value, na.rm=TRUE), by=byVars]
setnames(subset1, 'V1', 'value')
setnames(subset2, 'V1', 'value')
setnames(subset3, 'V1', 'value')
setnames(subset4, 'V1', 'value')
setnames(subset5, 'V1', 'value')
subset1[, indicator:='Patients with uncomplicated malaria treated']
subset2[, indicator:='Patients with severe malaria treated']
subset3[, indicator:='Rapid Diagnostic Tests (RDT)']
subset4[, indicator:='Sulfadoxine-Pyrimethamine (SP) used during 1st ANC visit']
subset5[, indicator:='Long-lasting insecticide-treated nets (LLINs)']

# make other subsets that you need
# --------------------------------------


# --------------------------------------------------------------------------
# Import shapefile 

# import shapefile (this one is stored as an rdata file type)
shapedata = readRDS('hz_shapefile.rds')

# make a simplified version of the map so it goes faster
simpleshapedata = gSimplify(shapedata, tol=.1)

# convert the shapefile to a data frame
shapedata_df = fortify(simpleshapedata, region='index')

# convert index to character in the shapefile so the merge (below) works 
shapedata@data$index = as.character(shapedata@data$index)

# merge the names of the health zones to the fortified data frame
shapedata_df = merge(shapedata_df, shapedata@data, by.x='id', by.y='index')
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# Merge the datasets 

# merge the data to the shape data
plotdata1 = merge(shapedata_df, subset1, by.x=c('Name', 'PROVNAME'), by.y=c('health_zone', 'former_dps_name'), all.x=TRUE)
plotdata2 = merge(shapedata_df, subset2, by.x=c('Name', 'PROVNAME'), by.y=c('health_zone', 'former_dps_name'), all.x=TRUE)
plotdata3 = merge(shapedata_df, subset3, by.x=c('Name', 'PROVNAME'), by.y=c('health_zone', 'former_dps_name'), all.x=TRUE)
plotdata4 = merge(shapedata_df, subset4, by.x=c('Name', 'PROVNAME'), by.y=c('health_zone', 'former_dps_name'), all.x=TRUE)
plotdata5 = merge(shapedata_df, subset5, by.x=c('Name', 'PROVNAME'), by.y=c('health_zone', 'former_dps_name'), all.x=TRUE)
	
# make sure the order is still preserved
plotdata1 = plotdata1[order(plotdata1$order),]
plotdata2 = plotdata2[order(plotdata2$order),]
plotdata3 = plotdata3[order(plotdata3$order),]
plotdata4 = plotdata4[order(plotdata4$order),]
plotdata5 = plotdata5[order(plotdata5$order),]

# rbind
plotdata = rbind(plotdata1, plotdata2)
plotdata = rbind(plotdata, plotdata3)
plotdata = rbind(plotdata, plotdata4)
plotdata = rbind(plotdata, plotdata5)
# --------------------------------------------------------------------------------------


# ---------------------------------------------------------------------
# Make a basic map 

# get nicer colors for map
mapColors = brewer.pal(10, 'RdYlBu')

# map 1
b1 = c(5000, 10000, 25000, 60000, 140000)
p1 = ggplot(plotdata1, aes(x=long, y=lat, group=group, fill=value)) + 
	geom_polygon() + 
	geom_path(color='grey65', size=.05) + 
	scale_fill_gradientn('Count', colours=mapColors, na.value='white', trans='log', breaks=b1) + 
	labs(title='Patients with uncomplicated\nmalaria treated') + 
	theme_void() + 
	theme(plot.title = element_text(hjust = 0.5))

# map 2
b2 = c(10, 50, 200, 750, 2000)
names(b2) = NULL
p2 = ggplot(plotdata2, aes(x=long, y=lat, group=group, fill=value)) + 
	geom_polygon() + 
	geom_path(color='grey65', size=.05) + 
	scale_fill_gradientn('Count', colours=mapColors, na.value='white', trans='log', breaks=b2) + 
	labs(title='Patients with severe\nmalaria treated') + 
	theme_void() + 
	theme(plot.title = element_text(hjust = 0.5))

# map 3
b3 = c(7000, 15000, 33000, 75000, 235000)
p3 = ggplot(plotdata3, aes(x=long, y=lat, group=group, fill=value)) + 
	geom_polygon() + 
	geom_path(color='grey65', size=.05) + 
	scale_fill_gradientn('Count', colours=mapColors, na.value='white', trans='log', breaks=b3) + 
	labs(title='Rapid diagnostic\ntests performed') + 
	theme_void() + 
	theme(plot.title = element_text(hjust = 0.5))

# map 4
b4 = c(1000, 2000, 5000, 12000, 30000)
p4 = ggplot(plotdata4, aes(x=long, y=lat, group=group, fill=value)) + 
	geom_polygon() + 
	geom_path(color='grey65', size=.05) + 
	scale_fill_gradientn('Count', colours=mapColors, na.value='white', trans='log', breaks=b4) + 
	labs(title='Sulfadoxine-Pyrimethamine used\nduring 1st ANC visit') + 
	theme_void() + 
	theme(plot.title = element_text(hjust = 0.5))

# map 5
b5 = c(100, 1000, 5000, 20000, 75000)
p5 = ggplot(plotdata5, aes(x=long, y=lat, group=group, fill=value)) + 
	geom_polygon() + 
	geom_path(color='grey65', size=.05) + 
	scale_fill_gradientn('Count', colours=mapColors, na.value='white', trans='log', breaks=b5) + 
	labs(title='Long-lasting\ninsecticide-treated nets used') + 
	theme_void() + 
	theme(plot.title = element_text(hjust = 0.5))
	
pdf(outFile, height=5.5, width=10)
grid.draw(grid.arrange(p5,p1,p3,p2,p4, ncol=3))
dev.off()
# ---------------------------------------------------------------------
