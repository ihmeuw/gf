# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Explore supply chain data for PNLS, mainly on HIV test kits. 
# DATE: Last updated March 2019
# --------------------------------------------------------------------

#---------------------------------------------------------------------
# TO DO LIST 
#  For elements where stock category is NA, investigate what's going on. *DONE
#  Make sex-stratified graphs for elements that have them. *DONE
#  Add a variable for regimen - first, second. *DONE, NEEDS REVIEW
# Check for data entry errors - are there more than 30 days out of stock for a given facility in a given month? *DONE (simple version)
# Check for data inconsistencies in the 'value' variable - why are we getting such high stock-out days for map m1 in 2018?
#  Move on to mapping first line treatment regimen - 
#  1. Map mean test kits per facility. Try without a facet wrap, and then facet wrap by year. *DONE - facet wrapped by year. Code needs review. 
#  2. Reporting completeness - length(unique(facility)), by = date *DONE
#  3. Look at first line treatment, days out of stock. *DONE
#  4. Stratify the variables you can by sex. *DONE 
# 
# 
#---------------------------------------------------------------------

#Observations about this dataset: 
# There is no data for December 2017. 
# If stock category is NA, then it is sex-stratified. 
#We seem to just have data gaps for the same 3 districts in 2017? 

rm(list=ls())
library(data.table)
library(ggplot2)
library(maptools)
library(raster)
library(RColorBrewer)
library(rgdal)
library(rgeos)

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

rawDT = readRDS(paste0(dir, 'prepped/pnls_sets/pnls_drug_2017_01_01_2018_12_01.rds'))
dt = copy(rawDT) #Just doing this so I don't have to read in file over and over
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
date_frame = data.table(month = seq(1, 12, by=1), expected_days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
dt[, month:=month(date)]
dt = merge(dt, date_frame, all.x = TRUE, by = 'month')

#unique(dt[value >31 & stock_category == "number_of_days_stocked_out", .(value)]) #Caitlin do we want to clean any of these? 
dt[value>expected_days & stock_category == "number_of_days_stocked_out", impossible_so_days:=TRUE] #Create a boolean value to flag which NAs you've created. 
dt[value>expected_days & stock_category == "number_of_days_stocked_out", value:=NA] #Replace impossible days stocked out with NA

#Create a variable to delineate first-line and second-line regimens

#From treatment regimen PDF in DRC - 
# Initiation du TAR (*) :
#   -Régime de traitement :
#   TDF + 3TC + EFV
# -Alternatives :
#   TDF + 3TC + NVP
# AZT + 3TC + EFV
# AZT + 3TC + NVP
dt[element_id == "jJuipTLZK4o", regimen:=1] # TDF/3TC/EFV(300/300/600 mg) - 30 ces
dt[element_id =="aozs6mB8T8n", regimen:=2] #AZT+3TC+NVP
dt[element_id =="W7sym5eCc44", regimen:=2]  #"AZT/3TC/NVP(300/150/200 mg) - 60 ces" 
dt[element_id == "pzMcLYBCPYG", regimen:=2] #AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces
dt[element_id == "ANTg88cSB09", regimen:=2] #AZT+3TC+EFV

unique(dt[!is.na(regimen), .(regimen, element)][order(regimen)])
#Are there any other second-line regimens we can pull here? 

#Save a cleaned data set here so you can run quantile regression 
saveRDS(dt, paste0(dir, "prepped/pnls_drug.rds"))
#--------------------------------------------------------------
# Subset this cleaned data set into specialized data tables
#--------------------------------------------------------------

#Create an 'annual' dataset to address the reporting lag at the end of 2018. 
# Subset both years to only the first 9 months so they are comparable. 
{
  annual_dt = dt[(date< "2018-09-01" & year == 2018) | (date < '2017-09-01' & year == 2017)] 
  unique(annual_dt[, .(date, year)][order(date, year)]) #Visual inspection for the subset above
}
#Make a test kit data table 
{
  # element_id                                  element
  # 21: Gv1UQdMw5wL                           HIV 1+2, Determine Complete, Kit de 100 tests
  # 22: ctP0MNHiq3B                                  HIV 1+2, Uni-Gold HIV, Kit de 20 tests
  # 23: k3JmmwNHkmY                             HIV 1/2, Double Check Gold, Kit de 100 test
  
  test_kit_vars = c("Gv1UQdMw5wL", "ctP0MNHiq3B", "k3JmmwNHkmY")
  test_kits = dt[element_id%in%test_kit_vars]
  test_kits = test_kits[, .(element_id, date, element, value, stock_category)]
  test_kits[, value:=sum(value, na.rm = TRUE), by=c('element_id', 'element', 'date', 'stock_category')]
  test_kits = unique(test_kits)
}

#Make a data table to caculate facility weeks stocked out
{
  dt[ ,total_facilities:=length(unique(org_unit_id)), by=dps]
  dps_level = dt[, .(element_id, dps, value, stock_category, date, total_facilities)]
  dps_level[, value:=sum(value), by=c('total_facilities', 'element_id', 'dps', 'stock_category', 'date')]
  dps_level = unique(dps_level)
  
  dps_level_so = dps_level[stock_category=="number_of_days_stocked_out"]
  dps_level_so[, mean_fac_days_so:=value/total_facilities]
}

#Make a data table that just contains treatment drugs (?)

#Make a data table to calculate mean test kits per facility (count the number of test kits available at the district level, and divide by the number of facilities in the data)
#Aggregating at the year-level so it can be mapped easily to coordinates; this should be reviewed. 
{
  test_kit_vars = c("Gv1UQdMw5wL", "ctP0MNHiq3B", "k3JmmwNHkmY")
  facs_per_district = dt[element_id%in%test_kit_vars & stock_category == "available_usable_stock", .(facs=length(unique(org_unit_id))), by=dps] #Find the number of facs per district with available test-kit stock. 
  
  kits_per_facility = dt[element_id%in%test_kit_vars & stock_category == "available_usable_stock", .(element_id, value, dps, year, id, date)]
  kits_per_facility = kits_per_facility[(date< "2018-09-01" & year == 2018) | (date < '2017-09-01' & year == 2017)] #Subset to handle time lags. 
  kits_per_facility = kits_per_facility[, .(value=sum(value, na.rm = TRUE)), by = c('element_id', 'dps', 'year', 'id')] #Collapse here, because you want to get rid of the date-level. 
  kits_per_facility = merge(kits_per_facility, facs_per_district, by='dps', all.x = TRUE)
  kits_per_facility[, kits_per_fac:=round(value/facs, 2)]
  
  #Merge with coordinate system so it can be mapped 
  det_per_fac_map = merge(kits_per_facility[element_id == "Gv1UQdMw5wL"], coord_ann, by=c('id', 'year'), all.y=TRUE)
  uni_per_fac_map = merge(kits_per_facility[element_id == "ctP0MNHiq3B"], coord_ann, by=c('id', 'year'), all.y=TRUE)
  doub_per_fac_map = merge(kits_per_facility[element_id == "k3JmmwNHkmY"], coord_ann, by=c('id', 'year'), all.y=TRUE)
}

#o	For annual maps, I would recommend subsetting to Jan. – Aug. in 2018 given the data lag (dt[date < ‘2018-09-01’]). 
#So, when you compare days out of stock, make sure you’re comparing ‘apples to apples’ by subsetting both years to nine month periods (because obviously a year will have more days than 8 months). 
{
  #Caitlin - for all of these data sets, do we want to sum by date or sum by year? 
  det_annual_so = annual_dt[element_id == "Gv1UQdMw5wL" & stock_category == "number_of_days_stocked_out", .(element_id, stock_category, value, date, year, id)]
  det_annual_so = det_annual_so[, .(value=sum(value, na.rm = T)), by=c('id', 'element_id', 'stock_category', 'year')] #Do we want to sum by date? Or sum by year?
  uni_annual_so = annual_dt[element_id == "ctP0MNHiq3B" & stock_category == "number_of_days_stocked_out", .(element_id, stock_category, value, date, year, id)]
  uni_annual_so = uni_annual_so[, .(value=sum(value, na.rm = T)), by=c('id', 'element_id', 'stock_category', 'year')] #Do we want to sum by date? Or sum by year?
  doub_annual_so = annual_dt[element_id == "k3JmmwNHkmY" & stock_category == "number_of_days_stocked_out", .(element_id, stock_category, value, date, year, id)]
  doub_annual_so = doub_annual_so[, .(value=sum(value, na.rm = T)), by=c('id', 'element_id', 'stock_category', 'year')] #Do we want to sum by date? Or sum by year?
  
  # merge with coordinates
  det_so_map = merge(det_annual_so, coord_ann, by=c('id', 'year'), all.y=TRUE)
  uni_so_map = merge(uni_annual_so, coord_ann, by=c('id', 'year'), all.y = TRUE)
  doub_so_map = merge(doub_annual_so, coord_ann, by=c('id', 'year'), all.y = TRUE)
}

#Generate a data table on reporting completeness - both just by date, and by date/dps
# total facilities/art sites and whether they reported
{
  report_by_date = dt[ ,.(facilities_by_date=length(unique(org_unit_id))), by=date]
  
  report_by_dps = dt[, .(facilities_by_dps=length(unique(org_unit_id))), by=c('year', 'dps')]
  report_by_dps = merge(report_by_dps, dt, by=c('year', 'dps'))
  report_by_dps = unique(report_by_dps[, .(year, id, facilities_by_dps, dps)]) 
  
  report_map = merge(report_by_dps, coord_ann, by = c('id', 'year'), all.y = TRUE, allow.cartesian = TRUE)
  
}

#Measure stock-outs of first-line treatment regimens
{
  regimen = annual_dt[!is.na(regimen) & stock_category == 'number_of_days_stocked_out', 
                      .(value = sum(value, na.rm = TRUE)), by = c('element_id', 'year', 'dps', 'id', 'regimen')] #Review this variable creation above and make sure all drugs are included 
  #I'm only getting 3 of the 5 drugs here - do I not have stock-out data for all? 
  unique(dt[!is.na(regimen), .(element, element_id, stock_category)][order(element, stock_category)])
  
  azt_3tc_nvp1_map = merge(regimen[element_id == "pzMcLYBCPYG"], coord_ann, by=c('id', 'year'), all.y = TRUE) #AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces
  azt_3tc_nvp2_map = merge(regimen[element_id == "W7sym5eCc44"], coord_ann, by=c('id', 'year'), all.y = TRUE) #AZT/3TC/NVP(300/150/200 mg) - 60 ces
  tdf_3tc_efv_map = merge(regimen[element_id == "jJuipTLZK4o"], coord_ann, by=c('id', 'year'), all.y = TRUE) #TDF/3TC/EFV(300/300/600 mg) - 30 ces
  
}

#--------------------------------------------------------------
#Make some maps and charts
#--------------------------------------------------------------
# •	Graph national time trends for each test kit variable. For example, a graph would be facet wrapped by stock category with scales=’free_y’. It doesn’t make sense to use stock category
#as color (all on the same graph) because they are in different units – days out of stock in days, stock available is in… stuff. So, we want to facet with a continuous y axis.

tt1 = ggplot(test_kits[element_id == "Gv1UQdMw5wL"], aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  theme_bw() + 
  labs(title="Determine stock categories over time, with free_y scales")

tt2 = ggplot(test_kits[element_id == "k3JmmwNHkmY"] , aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  theme_bw() + 
  labs(title="Double-check gold stock categories over time, with free_y scales")

tt3 = ggplot(test_kits[element_id == "ctP0MNHiq3B"], aes(x=date, y=value)) + 
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

# Make a graph for reporting completeness, and then map reporting completeness by district. 
rep1 = ggplot(report_by_date, aes(x=date, y=facilities_by_date)) + 
  geom_point() + geom_line() + 
  theme_bw() + 
  labs(title="Facilities reporting over time", y = "Number of facilities", x = "Date")

rep2 = ggplot(report_map, aes(x=long, y=lat, group=group, fill=facilities_by_dps)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="Reporting completeness by year and district, DRC")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


# Start with a single variable. Make a data table called ‘det’ that includes just the Determine variable and the number of days stocked out.
# Sum the total days stocked out by district as you did in your graphs: det [stock_category==’days_out_of_stock’ ,sum(value), by=.(date, dps)]


#Question - do we want to aggregate this map by year? Do we want to subset to one year and do a map for each quarter? 
m1 = ggplot(det_so_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Determine days out of stock by district, DRC")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

m2 = ggplot(uni_so_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Uni-Gold days out of stock by district, DRC")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

m3 = ggplot(doub_so_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Double-Check days out of stock by district, DRC")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

#Map mean test kits per facility. Try without a facet wrap, and then facet wrap by year. 
m4 = ggplot(det_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Mean Determine test kits per facility, DRC")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

m5 = ggplot(uni_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Mean Uni-Gold test kits per facility, DRC")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

m6 = ggplot(doub_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Mean Double-Gold test kits per facility, DRC")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


#Map treatment stock-outs for first-line and second-line drugs 
#Caitlin- would it be helpful to aggregate to only first-line regimen and second-line regimen drugs? Maybe generate indicators for these stock-outs? 
treat1 = ggplot(azt_3tc_nvp1_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces days out of stock by district")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

treat2 = ggplot(azt_3tc_nvp2_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="AZT/3TC/NVP(300/150/200 mg) - 60 ces days out of stock by district")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

treat3 = ggplot(tdf_3tc_efv_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Greens'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="TDF/3TC/EFV(300/300/600 mg) - 30 ces days out of stock by district")+
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 


#--------------------------------------------------------------
#Make the graphs, and write to a .PDF
#--------------------------------------------------------------
outFile = paste0(saveDir, 'drug_test_kits.pdf')
pdf(outFile, height=5.5, width=7)

tt1
tt2
tt3

m1
m2
m3

m4
m5
m6

dev.off()


#The other thing was just to run a PDF loop with a time trend for every variable (50 page PDF). Make sure you aggregate
#to the level of variable/date/stock category 
#before you do that and facet by stock category. An extra credit alternative would be to add an if statement 
#that says if(!is.na(sex)) for a given variable and then embed 
#a ggplot where color = sex, so that the ones that have sex are stratified by sex and the ones that don’t have sex have a single line on each graph.
outFile = paste0(saveDir, 'drug_overview.pdf')
pdf(outFile, height=5.5, width=7)

rep1
rep2 

for (var in unique(dt$element)){
  temp = dt[element==var]
  
  #If you don't have sex, make a general plot.
  if (nrow(temp[is.na(sex)])!=0){ 
    temp = temp[, .(date, value, element, stock_category)]
    temp = temp[, .(value=sum(value, na.rm = TRUE)), by=.(date, element, stock_category)]
    
    plot = ggplot(temp, aes(x=date, y=value)) + 
      geom_point() + geom_line() + 
      theme_bw() + 
      facet_wrap(~stock_category, scales='free_y')+
      labs(title= var)
  } else { #If you do have sex, make a plot by sex, ignoring stock category because these values are NA where the element is stratified by sex.
    temp = temp[, .(date, value, element, sex)]
    temp = temp[, .(value=sum(value)), by=.(date, element, sex)]
    
    plot = ggplot(temp, aes(x=date, y=value, color=sex)) + 
      geom_point() + geom_line() + geom_line() +
      theme_bw() + 
      labs(title= var)
  }
  print(plot) 
}
dev.off()

#Print all maps into one file 
outFile = paste0(saveDir, 'drug_treatment.pdf')
pdf(outFile, height=5.5, width=7)

treat1
treat2
treat3


dev.off()