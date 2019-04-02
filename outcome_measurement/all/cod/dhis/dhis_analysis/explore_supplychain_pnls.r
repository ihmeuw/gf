# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Explore supply chain data for PNLS, mainly on HIV test kits. 
# DATE: Last updated March 2019
# --------------------------------------------------------------------

#---------------------------------------------------------------------
# TO DO LIST 
#  For elements where stock category is NA, investigate what's going on.
#  Make sex-stratified graphs for elements that have them. 
#  Add a variable for regimen - first, second. 
# Check for data entry errors - are there more than 30 days out of stock for a given facility in a given month? 
#  Move on to mapping first line treatment regimen - 
#  1. Map mean test kits per facility. Try without a facet wrap, and then facet wrap by year. 
#  2. Reporting completeness - length(unique(facility)), by = date
#  3. Look at first line treatment, days out of stock. 
#  4. Stratify the variables you can by sex.
# 
# 

#---------------------------------------------------------------------

#Observations about this dataset: 
# There is no data for December 2017. 
# If stock category is NA, then it is sex-stratified. 

rm(list=ls())
library(data.table)
library(ggplot2)
library(rgdal)

setwd("C:/Users/elineb/Documents/gf") #Set this to the root of your repo
source('./core/standardizeDPSNames.R')
#--------------------------------------------------------------
#Read in data 
#--------------------------------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
saveDir = paste0(dir, "outputs/pnls/")

dt = readRDS(paste0(dir, 'prepped/pnls_sets/pnls_drug_2017_01_01_2018_12_01.rds'))
shapefile = shapefile("J:/Project/Evaluation/GF/mapping/cod/gadm36_COD_shp/gadm36_COD_1.shp")
shapefile@data$NAME_1 = standardizeDPSNames(shapefile@data$NAME_1)

#Make it possible to merge the data with the shape file
shape_names = data.table(id = seq(0, 25, by=1), NAME_1=shapefile@data$NAME_1) #This matches the data when you fortify the shapefile below

dt[, NAME_1:=standardizeDPSNames(dps)]
dt[!NAME_1%in%shapefile@data$NAME_1] #Check that merge will work correctly - this data table should have 0 rows. 

dt = merge(dt, shape_names, by='NAME_1', all.x = TRUE)

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coord = data.table(fortify(shapefile)) 
coord[, id:=as.numeric(id)]
coord_ann = rbind(coord, coord)
coord_ann[, year:=rep(2017:2018, each=nrow(coord))] #What years do you have data for? 


#--------------------------------------------------------------
#Clean the data 
#--------------------------------------------------------------
dt = dt[, -c('subpop', 'maternity', 'case')]

dt[stock_category=='Nbr de jours RS', stock_category:='number_of_days_stocked_out']
dt[stock_category=="Stock disponible utilisable", stock_category:='available_usable_stock']
dt[stock_category=='Stock Initial', stock_category:='initial_stock']
dt[stock_category=="Entrée", stock_category:='input']
dt[stock_category=='Sortie', stock_category:='output']

#Make a year variable - be careful with the gaps in the data. 
dt[, year:=year(date)]

#Check to make sure there aren't impossible reporting periods for stock-out days per month, and drop these values
unique(dt[value >32 & stock_category == "number_of_days_stocked_out", .(value)]) #Caitlin do we want to clean any of these? 
dt = dt[!(value> 32 & stock_category == "number_of_days_stocked_out")]

#--------------------------------------------------------------
# Subset this cleaned data set into specialized data tables
#--------------------------------------------------------------

#Make a test kit data table 
test_kit_vars = c("HIV 1+2, Determine Complete, Kit de 100 tests", "HIV 1/2, Double Check Gold, Kit de 100 test", "HIV 1+2, Uni-Gold HIV, Kit de 20 tests")
test_kits = dt[element%in%test_kit_vars]
test_kits = test_kits[, .(element_id, date, element, value, stock_category)]
test_kits[, value:=sum(value), by=c('element_id', 'element', 'date', 'stock_category')]
test_kits = unique(test_kits)

#Make a data table to caculate facility weeks stocked out
dt[ ,total_facilities:=length(unique(org_unit_id)), by=dps]
dps_level = dt[, .(element, dps, value, stock_category, date, total_facilities)]
dps_level[, value:=sum(value), by=c('total_facilities', 'element', 'dps', 'stock_category', 'date')]
dps_level = unique(dps_level)

dps_level_so = dps_level[stock_category=="number_of_days_stocked_out"]
dps_level_so[, mean_fac_days_so:=value/total_facilities]


#o	For annual maps, I would recommend subsetting to Jan. – Aug. in 2018 given the data lag (dt[date < ‘2018-09-01’]). 
#So, when you compare days out of stock, make sure you’re comparing ‘apples to apples’ by subsetting both years to nine month periods (because obviously a year will have more days than 8 months). 
annual_dt = dt[(date< "2018-09-01" & year == 2018) | (date < '2017-09-01' & year == 2017)] #Subset to only 8 months of the year to handle time lag in 2018.
unique(annual_dt[, .(date, year)][order(date, year)]) #Visual inspection for the subset above

#Caitlin - for all of these data sets, do we want to sum by date or sum by year? 
det_annual_so = annual_dt[element == "HIV 1+2, Determine Complete, Kit de 100 tests" & stock_category == "number_of_days_stocked_out", .(element, stock_category, value, date, year, id)]
det_annual_so = det_annual_so[, .(value=sum(value, na.rm = T)), by=c('id', 'element', 'stock_category', 'year')] #Do we want to sum by date? Or sum by year?
uni_annual_so = annual_dt[element == "HIV 1+2, Uni-Gold HIV, Kit de 20 tests" & stock_category == "number_of_days_stocked_out", .(element, stock_category, value, date, year, id)]
uni_annual_so = uni_annual_so[, .(value=sum(value, na.rm = T)), by=c('id', 'element', 'stock_category', 'year')] #Do we want to sum by date? Or sum by year?
doub_annual_so = annual_dt[element == "HIV 1/2, Double Check Gold, Kit de 100 test" & stock_category == "number_of_days_stocked_out", .(element, stock_category, value, date, year, id)]
doub_annual_so = doub_annual_so[, .(value=sum(value, na.rm = T)), by=c('id', 'element', 'stock_category', 'year')] #Do we want to sum by date? Or sum by year?

# merge with coordinates
det_so_map = merge(det_annual_so, coord_ann, by=c('id', 'year'), all.y=TRUE)
uni_so_map = merge(uni_annual_so, coord_ann, by=c('id', 'year'), all.y = TRUE)
doub_so_map = merge(doub_annual_so, coord_ann, by=c('id', 'year'), all.y = TRUE)


#--------------------------------------------------------------
#Make some charts 
#--------------------------------------------------------------
# •	Graph national time trends for each test kit variable. For example, a graph would be facet wrapped by stock category with scales=’free_y’. It doesn’t make sense to use stock category
#as color (all on the same graph) because they are in different units – days out of stock in days, stock available is in… stuff. So, we want to facet with a continuous y axis.

tt1 = ggplot(test_kits[element=="HIV 1+2, Determine Complete, Kit de 100 tests"], aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  theme_bw() + 
  labs(title="Determine stock categories over time, with free_y scales")

tt2 = ggplot(test_kits[element=="HIV 1/2, Double Check Gold, Kit de 100 test"], aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  theme_bw() + 
  labs(title="Double-check gold stock categories over time, with free_y scales")

tt3 = ggplot(test_kits[element=="HIV 1+2, Uni-Gold HIV, Kit de 20 tests"], aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  theme_bw() + 
  labs(title="Uni-gold stock categories over time, with free_y scales")

# •	Make a DPS level map for each. I would start with a count of days out of stock per district (just sum by district) and make one map for each type of test kits 
#(answers the question: how many stock out days were there per district for first line versus second line test kits?). You can also facet wrap by year, 
#so you would have two maps (2017 and 2018) on the same page.


# •	Then, I would have some fun by using a distinct metric – mean stock out days per facility. The reason we want this is that you could have a district with a 
#lot of facilities that were rarely stocked out and it would look like there were more stock out days there than in a district with few facilities with long stock outs.
#So, create a data table dt[ ,total_facilities=length(unique(org_unit_id)), by=dps] and merge it into your summed data, then divide the total stockout days by the facilities.




# Start with a single variable. Make a data table called ‘det’ that includes just the Determine variable and the number of days stocked out.
# Sum the total days stocked out by district as you did in your graphs: det [stock_category==’days_out_of_stock’ ,sum(value), by=.(date, dps)]


#Question - do we want to aggregate this map by year? Do we want to subset to one year and do a map for each quarter? 
m1 = ggplot(det_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  facet_wrap(~year) +
  labs(title="Determine days out of stock by district, DRC")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


#--------------------------------------------------------------
#Make the graphs, and write to a .PDF
#--------------------------------------------------------------
outFile = paste0(saveDir, 'drug_test_kits.pdf')
pdf(outFile, height=5.5, width=7)

tt1
tt2
tt3

dev.off()


#The other thing was just to run a PDF loop with a time trend for every variable (50 page PDF). Make sure you aggregate
#to the level of variable/date/stock category 
#before you do that and facet by stock category. An extra credit alternative would be to add an if statement 
#that says if(!is.na(sex)) for a given variable and then embed 
#a ggplot where color = sex, so that the ones that have sex are stratified by sex and the ones that don’t have sex have a single line on each graph.
outFile = paste0(saveDir, 'drug_all_elements.pdf')
pdf(outFile, height=5.5, width=7)

for (var in unique(dt$element)){
  temp = dt[element==var, .(date, value, element, stock_category)]
  temp[, value:=sum(value), by=.(date, element, stock_category)]
  temp = unique(temp)
  
  plot = ggplot(temp, aes(x=date, y=value)) + 
    geom_point() + geom_line() + 
    theme_bw() + 
    facet_wrap(~stock_category, scales='free_y')+
    labs(title= var)
  print(plot) 
}
dev.off()

#Print all maps into one file 
outFile = paste0(saveDir, 'drug_maps.pdf')
pdf(outFile, height=5.5, width=7)

m1

dev.off()