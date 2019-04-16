# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Explore supply chain data for PNLS, mainly on HIV test kits. 
# DATE: Last updated March 2019
# --------------------------------------------------------------------

#---------------------------------------------------------------------
# TO DO LIST 
#  Add available-usable stock to title where it refers to it in slides, 
# Split sex-stratified variables out into their own PDF. 
# For two indicators in test-kits PDFs, generate both 'total counts' and 'mean facility-days' maps
#     make these labels really clear! *DONE
# Generate a map that shows changes in stock-out days in a district over time, facet-wrapped by month, for only 2018 data. 
#   Take this same map and subset to the facility-level. *DONE
# 

# Not urgent - check online PNLS dashboard and make sure new first-line treatment exists. 
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

#Make a year variable - be careful with the gaps in the data. 
dt[, year:=year(date)]


# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coord = data.table(fortify(shapefile)) 
coord[, id:=as.numeric(id)]
coord_ann = rbind(coord, coord)
coord_ann[, year:=rep(2017:2018, each=nrow(coord))] #What years do you have data for? 

#Make a coordinate map for the months you have available in the data. 
dates_avail = unique(dt[, .(date)][order(date)])
coord_months = data.table()
for (i in dates_avail){
  print(i)
  temp = coord
  temp[, date:=i]
  coord_months = rbind(coord_months, temp)
}

#--------------------------------------------------------------
#Clean the data 
#--------------------------------------------------------------
dt = dt[, -c('subpop', 'maternity', 'case')]

dt[stock_category=='Nbr de jours RS', stock_category:='number_of_days_stocked_out']
dt[stock_category=="Stock disponible utilisable", stock_category:='available_usable_stock']
dt[stock_category=='Stock Initial', stock_category:='initial_stock']
dt[stock_category=="Entrée", stock_category:='input']
dt[stock_category=='Sortie', stock_category:='output']

#Check to make sure there aren't impossible reporting periods for stock-out days per month, and drop these values
date_frame = data.table(month = seq(1, 12, by=1), expected_days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
dt[, month:=month(date)]
dt = merge(dt, date_frame, all.x = TRUE, by = 'month')

#unique(dt[value >31 & stock_category == "number_of_days_stocked_out", .(value)]) #Caitlin do we want to clean any of these? 
dt[value>expected_days & stock_category == "number_of_days_stocked_out", impossible_so_days:=TRUE] #Create a boolean value to flag which NAs you've created. 
dt[value>expected_days & stock_category == "number_of_days_stocked_out", value:=NA] #Replace impossible days stocked out with NA

# Generate a variable 'any_stock_out' where the denominator is all stock_category 'number of days stocked out' 
#   (Including the 'impossible' NAs for now), and the numerator = 1 if value != 0 & value != NA. 
#   (for that given month, they had at least one day stocked out.)
#   Numerator = 0 if value == 0. Numerator == NA if value == NA. 
dt[stock_category == 'number_of_days_stocked_out', any_stock_out:=0]
dt[!is.na(any_stock_out) & value>0 & !is.na(value), any_stock_out:=1]
dt[is.na(value) & !is.na(any_stock_out), any_stock_out:=NA]

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

unique(dt[is.na(regimen), .(regimen, element)][order(regimen)])
#Are there any other second-line regimens we can pull here? 

#Save a cleaned data set here so you can run quantile regression 
saveRDS(dt, paste0(dir, "prepped/pnls_drug.rds"))
#--------------------------------------------------------------
# Subset this cleaned data set into specialized data tables
#--------------------------------------------------------------

#Make some nice color scales to use later on. 
colScale = scale_fill_gradient2(low="red", high="green", midpoint=0)
colScale2 = scale_fill_gradient2(low="green", high="red", midpoint=0)

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

#Measure stock-outs of first-line treatment regimens, both by total counts and by mean-facility-days. 
{
  #TOTAL COUNTS 
  regimen = annual_dt[!is.na(regimen) & stock_category == 'number_of_days_stocked_out', 
                      .(value = sum(value, na.rm = TRUE)), by = c('element_id', 'year', 'dps', 'id', 'regimen')] #Review this variable creation above and make sure all drugs are included 
  #I'm only getting 3 of the 5 drugs here - do I not have stock-out data for all? 
  unique(dt[!is.na(regimen), .(element, element_id, stock_category)][order(element, stock_category)])
  
  azt_3tc_nvp1_map = merge(regimen[element_id == "pzMcLYBCPYG"], coord_ann, by=c('id', 'year'), all.y = TRUE) #AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces
  azt_3tc_nvp2_map = merge(regimen[element_id == "W7sym5eCc44"], coord_ann, by=c('id', 'year'), all.y = TRUE) #AZT/3TC/NVP(300/150/200 mg) - 60 ces
  tdf_3tc_efv_map = merge(regimen[element_id == "jJuipTLZK4o"], coord_ann, by=c('id', 'year'), all.y = TRUE) #TDF/3TC/EFV(300/300/600 mg) - 30 ces
  
  #MEAN FACILITY-DAYS
  facs_per_district = dt[!is.na(regimen) & stock_category == "number_of_days_stocked_out", .(facs=length(unique(org_unit_id))), by=dps]
  
  treat_so_per_facility = annual_dt[!is.na(regimen) & stock_category == "number_of_days_stocked_out", .(element_id, element, value, dps, year, id, date)]
  treat_so_per_facility = treat_so_per_facility[, .(value=sum(value, na.rm = TRUE)), by = c('element_id', 'element', 'dps', 'year', 'id')] #Collapse here, because you want to get rid of the date-level. 
  treat_so_per_facility = merge(treat_so_per_facility, facs_per_district, by='dps', all.x = TRUE)
  treat_so_per_facility[, treat_per_fac:=round(value/facs, 2)]
  
  #Merge with coordinate system so it can be mapped 
  mean_so_map1 = merge(treat_so_per_facility[element_id=='pzMcLYBCPYG'], coord_ann, by=c('id', 'year'), all.y=TRUE) #"AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces"
  mean_so_map2 = merge(treat_so_per_facility[element_id=='W7sym5eCc44'], coord_ann, by=c('id', 'year'), all.y=TRUE) #For "AZT/3TC/NVP(300/150/200 mg) - 60 ces" 
  mean_so_map3 = merge(treat_so_per_facility[element_id=='jJuipTLZK4o'], coord_ann, by=c('id', 'year'), all.y=TRUE) #"TDF/3TC/EFV(300/300/600 mg) - 30 ces"

}
# Generate a variable 'monthly_so_rate' That represents the percentage of days in the month that a given facility was 
# Stocked out. *Note that you'll have to recalculate this based on the level of aggregation (facility, dps, etc.)
# Denominator represents total potential days in the month (Jan = 31, Feb = 28, etc.) and only gets created for 
# 'number_of_days_stocked_out'. 
# Numerator is the number of days stocked out. 
#Do this only for first line treatment drugs right now! 
{
  facs_per_district = dt[stock_category == "number_of_days_stocked_out" & !is.na(value) & element_id=='jJuipTLZK4o', .(facs=length(unique(org_unit_id))), by=c('dps', 'date')] #Exclude impossible day values here. 
  
  monthly_so_rate_dps = dt[stock_category == 'number_of_days_stocked_out' & element_id=='jJuipTLZK4o', .(id, value, date, expected_days, dps)]
  monthly_so_rate_dps = monthly_so_rate_dps[, .(value=sum(value, na.rm = TRUE)), by=c('id', 'date', 'expected_days', 'dps')]
  
  monthly_so_rate_dps = merge(monthly_so_rate_dps, facs_per_district, by=c('dps', 'date'))
  
  #Generate a variable at the dps level. 
  monthly_so_rate_dps[, expected_days_dps:=expected_days*facs]
  monthly_so_rate_dps[, monthly_so_rate:=round(value/expected_days_dps, 2)]
  
  monthly_so_rate_map = merge(monthly_so_rate_dps, coord_months, by=c('id', 'date'), all.y=TRUE) 
  
}
#Make a map that shows how the number of stock-out days in a district has changed over time. 
#Just do this for the first line drug combination for now. 
{
  #Use the monthly stock out rate graph that you've already made above. 
  #Do a very simple visual aid - create a variable 'change' that == 'increase' if the stock-out days in this month were 
  #higher than the stock-out days last month, and 'decrease' otherwise. 
  monthly_so_change = data.table()
  for (district in unique(monthly_so_rate_dps$dps)){
    # i = 1. If stock out days for i = 2 are higher than me, 'increase'. Otherwise 'decrease'. 
    temp = monthly_so_rate_dps[dps==district]
    if (nrow(temp)!=1){
      for (i in 2:nrow(temp)-1){
        temp$status[i+1] = ifelse(temp$monthly_so_rate[i]<temp$monthly_so_rate[i+1], "INCREASE", "DECREASE")
        temp$change[i+1] = temp$monthly_so_rate[i+1]-temp$monthly_so_rate[i]
      }
    }
    monthly_so_change= rbind(monthly_so_change, temp, fill=T)
  }
  
  monthly_so_change_map = merge(monthly_so_change, coord_months, by=c('id', 'date'), all.y=TRUE) 

}
{
  health_posts = dt[level%in%c("health_post")]
  facs_per_district_hp = health_posts[stock_category == "number_of_days_stocked_out" & !is.na(value) & element_id=='jJuipTLZK4o', .(facs=length(unique(org_unit_id))), by=c('dps', 'date')] #Exclude impossible day values here. 
  
  monthly_so_rate_dps_hp = health_posts[stock_category == 'number_of_days_stocked_out' & element_id=='jJuipTLZK4o', .(id, value, date, expected_days, dps)]
  monthly_so_rate_dps_hp = monthly_so_rate_dps_hp[, .(value=sum(value, na.rm = TRUE)), by=c('id', 'date', 'expected_days', 'dps')]
  
  monthly_so_rate_dps_hp = merge(monthly_so_rate_dps_hp, facs_per_district_hp, by=c('dps', 'date'))
  
  #Generate a variable at the dps level. 
  monthly_so_rate_dps_hp[, expected_days_dps:=expected_days*facs]
  monthly_so_rate_dps_hp[, monthly_so_rate:=round(value/expected_days_dps, 2)]
  
  monthly_so_rate_map_hp = merge(monthly_so_rate_dps_hp, coord_months, by=c('id', 'date'), all.y=TRUE) 
  
}
#Make a map that shows how the number of stock-out days in a district has changed over time. 
#Just do this for the first line drug combination for now. 
{
  #Use the monthly stock out rate graph that you've already made above. 
  #Do a very simple visual aid - create a variable 'change' that == 'increase' if the stock-out days in this month were 
  #higher than the stock-out days last month, and 'decrease' otherwise. 
  monthly_so_change_hp = data.table()
  for (district in unique(monthly_so_rate_dps_hp$dps)){
    # i = 1. If stock out days for i = 2 are higher than me, 'increase'. Otherwise 'decrease'. 
    temp = monthly_so_rate_dps_hp[dps==district]
    if (nrow(temp)!=1){
      for (i in 2:nrow(temp)-1){
        temp$status[i+1] = ifelse(temp$monthly_so_rate[i]<temp$monthly_so_rate[i+1], "INCREASE", "DECREASE")
        temp$change[i+1] = temp$monthly_so_rate[i+1]-temp$monthly_so_rate[i]
      }
    }
    monthly_so_change_hp= rbind(monthly_so_change_hp, temp, fill=T)
  }
  
  monthly_so_change_map_hp = merge(monthly_so_change_hp, coord_months, by=c('id', 'date'), all.y=TRUE) 
  
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
  theme(plot.title=element_text(vjust=-1), plot.subtitle=element_text(vjust=6)) 


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
  labs(title="Determine total days out of stock by district, DRC", subtitle="Annual maps restricted to Jan-Aug")

m2 = ggplot(uni_so_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Uni-Gold total days out of stock by district, DRC", subtitle="Annual maps restricted to Jan-Aug") 

m3 = ggplot(doub_so_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Double-Check total days out of stock by district, DRC", subtitle="Annual maps restricted to Jan-Aug")


#Map mean test kits per facility. Try without a facet wrap, and then facet wrap by year. 
m4 = ggplot(det_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Reds'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Mean Determine test kits per facility, by district", subtitle="Annual data restricted to Jan-Aug", 
        caption="*Denominator only includes facilities with 'available, usable stock' of test kits")

m5 = ggplot(uni_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Mean Uni-Gold test kits per facility, by district", subtitle="Annual data restricted to Jan-Aug", 
       caption="*Denominator only includes facilities with 'available, usable stock' of test kits")

m6 = ggplot(doub_per_fac_map, aes(x=long, y=lat, group=group, fill=kits_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="Mean Double-Check Gold test kits per facility, by district", subtitle="Annual data restricted to Jan-Aug", 
       caption="*Denominator only includes facilities with 'available, usable stock' of test kits")


#Map treatment stock-outs for first-line and second-line drugs 
#Caitlin- would it be helpful to aggregate to only first-line regimen and second-line regimen drugs? Maybe generate indicators for these stock-outs? 
treat1 = ggplot(azt_3tc_nvp1_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="'AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces' total days out of stock by district", subtitle="Annual data restricted to Jan-Aug")

treat2 = ggplot(azt_3tc_nvp2_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="'AZT/3TC/NVP(300/150/200 mg) - 60 ces' total days out of stock by district", subtitle="Annual data restricted to Jan-Aug")

treat3 = ggplot(tdf_3tc_efv_map, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Greens'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="'TDF/3TC/EFV(300/300/600 mg) - 30 ces' total days out of stock by district", subtitle="Annual data restricted to Jan-Aug")

treat4 = ggplot(mean_so_map1, aes(x=long, y=lat, group=group, fill=treat_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="'AZT/3TC/NVP 60/30/50 mg ces disp - 60 ces' mean facility-days out of stock by district", subtitle="Annual data restricted to Jan-Aug", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat5 = ggplot(mean_so_map2, aes(x=long, y=lat, group=group, fill=treat_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Blues'))) + 
  theme_void() +
  facet_wrap(~year, strip.position = "bottom") +
  labs(title="'AZT/3TC/NVP(300/150/200 mg) - 60 ces' mean facility-days out of stock by district", subtitle="Annual data restricted to Jan-Aug", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat6 = ggplot(mean_so_map3, aes(x=long, y=lat, group=group, fill=treat_per_fac)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Greens'))) + 
  theme_void() +
  facet_wrap(~year, strip.position="bottom") +
  labs(title="TDF/3TC/EFV mean facility-days out of stock by district", subtitle="Annual data restricted to Jan-Aug", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat7 = ggplot(monthly_so_rate_map[year(date)==2018], aes(x=long, y=lat, group=group, fill=monthly_so_rate)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  scale_fill_gradientn(colors=(brewer.pal(9, 'Purples'))) + 
  theme_void() +
  facet_wrap(~date, strip.position = "bottom") +
  labs(title="TDF/3TC/EFV stock-out rate per district by month", subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat8 = ggplot(monthly_so_change_map[year(date)==2018], aes(x=long, y=lat, group=group, fill=status)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  scale_fill_manual(breaks = c("DECREASE", "INCREASE"), 
                    values=c("green", "red"))+
  facet_wrap(~date) +
  labs(title="TDF/3TC/EFV absolute changes in stock-out rate for 2018", subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat9 = ggplot(monthly_so_change_map[year(date)==2018], aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  colScale2+
  facet_wrap(~date) +
  labs(title="TDF/3TC/EFV rate of change of stock-outs by district", subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

#Add one more map that shows absolute changes in stock at the level of each facility. 
treat10 = ggplot(monthly_so_change_map_hp[year(date)==2018], aes(x=long, y=lat, group=group, fill=status)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  scale_fill_manual(breaks = c("DECREASE", "INCREASE"), 
                    values=c("green", "red"))+
  facet_wrap(~date) +
  labs(title="TDF/3TC/EFV absolute changes in stock-out rate for 2018, health posts only", subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

treat11 = ggplot(monthly_so_change_map_hp[year(date)==2018], aes(x=long, y=lat, group=group, fill=change)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  colScale2+
  facet_wrap(~date) +
  labs(title="TDF/3TC/EFV rate of change of stock-outs by district, health posts only", subtitle="Data controlled for reporting", 
       caption = "*Denominator only includes facilities that reported data for the given treatment regimen")

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
treat4

treat2
treat5

treat3
treat6

treat7
treat8
treat9

treat10
treat11

dev.off()