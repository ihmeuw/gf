# ARV stockouts by facility - visualize the data 
# create data tables for distinct graphs
# Caitlin O'Brien-Carelli, modified by Emily Linebarger
# 12/14/2018
# ----------------------
# Set up R
library(raster)
library(ggplot2)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(maptools)

# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  'Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')

# working directory to aggregate
dt = readRDS(paste0(dir, 'pnls_drug.rds'))

#Read in the shapefile
shapefile = shapefile("J:/Project/Evaluation/GF/mapping/cod/gadm36_COD_shp/gadm36_COD_1.shp")
shapefile@data$NAME_1 = standardizeDPSNames(shapefile@data$NAME_1)
# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coord = data.table(fortify(shapefile)) 
coord[, id:=as.numeric(id)]
coord_ann = rbind(coord, coord)
coord_ann[, year:=rep(2017:2018, each=nrow(coord))] #What years do you have data for? 

#Set global variables here so you can re-run for any drug. 
drug_id = "Gv1UQdMw5wL"
drug_name = "Determine test kit"
full_date_range = "Jan 2017 - Dec 2018"
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


#---------------------------------
# reporting completeness data prep - graphs 1, 2

# total facilities/art sites and whether they reported
report = dt[ ,.(facilities=length(unique(org_unit_id))), by=date]
#Did they report for the specific drug you're targeting? 
tk = dt[element_id==drug_id, .(facs_reporting_on_drug=length(unique(org_unit_id))), by=date]
report = merge(report, tk, by='date', all.x=TRUE)

report[is.na(facs_reporting_on_drug), facs_reporting_on_drug:=0]
report[ , drug_reporting_ratio:=100*(facs_reporting_on_drug/facilities)]

# convert to numerics and round
report[, drug_reporting_ratio:=round(drug_reporting_ratio, 1)]

# shape long
report = melt(report, id.vars='date')
report[grep('ratio', variable), ratio:=TRUE]
report[!grep('ratio', variable), ratio:=FALSE]

# label the variables
report$variable = factor(report$variable, c( 'facilities',  'facs_reporting_on_drug','drug_reporting_ratio'), 
                         c('Total health facilities', paste0('Reported about ', drug_name, ' stock'),
                           paste0('% of facilities reporting on ', drug_name)))

#-----------------------------
# stock outs of ARVs - 3, 4, 5

drug_so = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & value>0 & !is.na(value), 
             .(facilities_stocked_out=as.numeric(length(unique(org_unit_id)))), by=date] #Count all facilities that reported stock out information for this drug, with greater than 0 days in the month. 
drug_so2 = dt[element_id==drug_id, .(facilities_reporting=as.numeric(length(unique(org_unit_id)))), by=date]
drug_so = merge(drug_so, drug_so2, by='date', all=T)
drug_so[is.na(facilities_reporting), facilities_reporting:=0]
drug_so[ ,ratio:=round(100*(facilities_stocked_out/facilities_reporting), 2)]

# calculate 50% of facilities to graph above a reporting threshold
n = dt[, length(unique(org_unit_id))/2 ]
drug_so_thresh = drug_so[facilities_reporting > n]

drug_so = melt(drug_so, id.vars='date')
drug_so$variable = factor(drug_so$variable, c('facilities_reporting', 'facilities_stocked_out', 'ratio'),
                      c(paste0('Facilities reporting on ', drug_name), paste0('Facilities with a stockout in ', drug_name), 
                        paste0('Percentage of facilities stocked out of ', drug_name)))

# run the same code on the threshold subset
drug_so_thresh = melt(drug_so_thresh, id.vars='date')
drug_so_thresh$variable = factor(drug_so_thresh$variable, c('facilities_reporting', 'facilities_stocked_out', 'ratio'),
                                 c(paste0('Facilities reporting on ', drug_name), paste0('Facilities with a stockout in ', drug_name), 
                                   paste0('Percentage of facilities stocked out of ', drug_name)))

#-----------------------------------
# ARV stockout weeks bar graphs - 6, 7

# Number of weeks of stockout by facility
so_days = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value),
             .(days=sum(value, na.rm=T)), by=.(year, org_unit_id)] #Do we want to have greater than 0 in this calculation? 
# Sanity check - are there any days greater than 365? This shouldn't be possible. 
range(so_days$days)
so_days = so_days[days >=1] #Caitlin this was just > 1 in your code? 
so_days = so_days[ ,.(facilities=length(unique(org_unit_id))), by=.(days, year)]
so_days[, weeks_stocked_out:=ceiling(days/7)]

# labels 
labels = so_days[ ,.(total=sum(facilities)), by=year]
l17 = paste0('2017 (n=', labels[year==2017]$total, ')')
l18 = paste0('2018 (n=', labels[year==2018]$total, ')') 
labels_vec = c(l17, l18)

so_days$year = factor(so_days$year, c(2017, 2018), 
                       labels_vec)

# same bar graph of stockouts, comparable time periods
so_days2 = dt[month(date)!=12 & element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value),
             .(days=sum(value, na.rm=T)), by=.(year, org_unit_id)] #Do we want to have greater than 0 in this calculation? 

# Sanity check - are there any days greater than 365? This shouldn't be possible. 
range(so_days2$days)
so_days2 = so_days2[days >=1] #Caitlin this was just > 1 in your code? 
so_days2 = so_days2[ ,.(facilities=length(unique(org_unit_id))), by=.(days, year)]
so_days2[, weeks_stocked_out:=ceiling(days/7)]

# labels 
labels = so_days2[ ,.(total=sum(facilities)), by=year]
l172 = paste0('2017 (n=', labels[year==2017]$total, ')')
l182 = paste0('2018 (n=', labels[year==2018]$total, ')') 
labels_vec2 = c(l172, l182)

so_days2$year = factor(so_days2$year, c(2017, 2018), 
                      labels_vec2)


#---------------------------------------
# ARV stockout maps - 8:12

# total facility-weeks of stock outs 
# exclude the months that are not in all years (e.g. december 2018)
stockout = dt[month(date)!=12 & element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), 
              .(days_stocked_out=sum(value, na.rm=T)), by=.(year, id)]
stockout[, weeks_stocked_out:=ceiling(days_stocked_out/7)]
so_map = merge(stockout, coord_ann, by=c('id', 'year'), all.y=TRUE)

# mean weeks stocked out 
# number of weeks of stockout divided by art sites reporting 
num_facilities = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), .(facilities=length(unique(org_unit_id))), by=.(year, id)]
num_facilities = merge(stockout, num_facilities)
num_facilities[ , mean_weeks:=round((weeks_stocked_out/facilities), 1)]
so_map_norm = merge(num_facilities, coord_ann, by=c('id', 'year'), all.y=TRUE)

# rates of change in facility-weeks per year
stockout[ , year2:=paste0('n', year)]
roc = dcast(data = stockout, id ~ year2, value.var=c('days_stocked_out', 'weeks_stocked_out'))
roc[ , change:=(weeks_stocked_out_n2018 - weeks_stocked_out_n2017)]
roc_map = merge(coord, roc, by='id')

# only districts with more stockouts in 2018 than 2017
roc_map_alt = merge(coord, roc, by='id')
roc_map_alt[change <=0, change:=NA]

# percentage of weeks stocked out
# stock = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), .(days_stocked_out=sum(value, na.rm=T)), by=.(year, id)]
# dt[art_site==TRUE & !is.na(arvs), reported:=TRUE]
# stock_add = dt[art_site==TRUE , .(total_weeks=sum(reported, na.rm=T)), by=.(year, id)]
# stock = merge(stock, stock_add, by=c('year', 'id'))
# stock[ , percent_out:=round(100*(weeks_out/total_weeks), 1)]
# stock = merge(stock, coord_ann, by=c('id', 'year'))


# #---------------------------------------
# # TEST KITS
# 
# # test kit stock out line graphs 13:15
# 
# # test kit stockouts
# test = dt[ , .(date=(unique(date)))]
# test2 = dt[!is.na(test_kits), .(reporting=length(unique(facility)), stockout=sum(test_kits, na.rm=T)), by=date]  
# test = merge(test, test2, by='date', all.x=T)
# 
# # calculate the percent of facilities stocked out in a given week
# test[ , ratio:=round(100*(stockout/reporting), 1)]
# test[ , reporting:=as.numeric(reporting)]
# test[ , stockout:=as.numeric(stockout)]
# 
# # label the variable
# test = melt(test, id.vars='date')
# test$variable = factor(test$variable, c('reporting', 'stockout', 'ratio'),
#                       c('Health facilities reporting', 'Facilities with a stockout of test kits', 
#                         'Percentage of facilities stocked out of test kits'))
# 
# # comparison of percent stocked out - test kits and arvs
# compare = test[variable == 'Percentage of facilities stocked out of test kits']
# compare_add = arv[variable == 'Percentage of ART sites stocked out of ARVs']
# compare = rbind(compare, compare_add)
# 
# 
# #-----------------------------------
# # TEST KIT STOCKOUT MAPS
# 
# #--------------------------
# # map of facility-weeks of stock outs 
# tk_stockout = dt[month!='2017-12-01', .(value=sum(test_kits, na.rm=T)), by=.(year, id)]
# 
# # merge with coordinates
# tk_map = merge(tk_stockout, coord_ann, by=c('id', 'year'), all.y=TRUE)
# 
# # number of weeks of stockout divided by facilities reporting 
# tk_sites = dt[!is.na(test_kits), .(tk_sites=length(unique(facility))), by=.(year, id)]
# tk_sites = merge(tk_stockout, tk_sites)
# tk_sites[ , mean_weeks:=round((value/tk_sites), 1)]
# tk_map_norm = merge(tk_sites, coord_ann, by=c('id', 'year'), all.y=TRUE)
# 
# # rates of change in facility-weeks per year
# tk_stockout[ , year2:=paste0('n', year)]
# tk_roc = dcast(tk_stockout, id ~ year2)
# tk_roc[ , change:=(n2018 - n2017)]
# tk_roc_map = merge(coord, tk_roc, by='id')
# 
# # only districts with more stockouts in 2018 than 2017
# tk_roc_map_alt = merge(coord, tk_roc, by='id')
# tk_roc_map_alt[change <=0, change:=NA]
# 
# # percentage of weeks stocked out
# tk_stock = dt[ , .(weeks_out=sum(test_kits, na.rm=T)), by=.(year, id)]
# dt[!is.na(test_kits), reported:=TRUE]
# tk_stock_add = dt[ , .(total_weeks=sum(reported, na.rm=T)), by=.(year, id)]
# tk_stock = merge(tk_stock, tk_stock_add, by=c('year', 'id'))
# tk_stock[ , percent_out:=round(100*(weeks_out/total_weeks), 1)]
# tk_stock = merge(tk_stock, coord_ann, by=c('id', 'year'))
# 
# #-------------------------------------------
# # scatter plots (facility level)
# 
# dt[ level=='HC II', level2:=2]
# dt[ level=='HC III', level2:=3]
# dt[ level=='HC IV', level2:=4]
# dt[ level=='Hospital', level2:=5]
# 
# scatter = dt[ ,.(arvs=sum(arvs, na.rm=T), test_kits=sum(test_kits, na.rm=T)), by=.(facility, level2, art_site)]
# scatter2 = dt[month!='2017-10-01' & month!='2017-11-01' & month!='2017-12-01',.(arvs=sum(arvs, na.rm=T), 
#                                       test_kits=sum(test_kits, na.rm=T)), by=.(facility, level2, year, art_site)]
# 
# #---------------------------------------
# # finale maps - categorical arv stockouts 
# 
# final = dt[art_site==TRUE,.(arvs=sum(arvs, na.rm=T)) , by=.(facility, year, id) ]
# final = final[ ,.(facilities=length(unique(facility))), by=.(arvs, year, id)]
# final[ ,months:=(arvs/4)]
# final[months==0, category:='no_stock_out']
# final[0 < months & months <= 1, category:='one_week_2_mos']
# final[1 < months & months <= 2, category:='two_4_mos']
# final[2 < months, category:='four_months']
# final = final[ ,.(value=sum(facilities)), by=.(year, id, variable=category)]
# final = dcast(final, year+id ~ variable)
# 
# final[is.na(no_stock_out), no_stock_out:=0]
# final[is.na(one_week_2_mos), one_week_2_mos:=0]
# final[is.na(two_4_mos), two_4_mos:=0]
# final[is.na(four_months), four_months:=0]
# 
# final = merge(final, coord_ann, by=c('id', 'year'), all.y=TRUE)
# final = melt(final, id.vars=c('year', 'id', 'long', 'lat', 'order', 'hole',
#                               'piece', 'group'))
# 
# final$variable = factor(final$variable, c('no_stock_out', 'one_week_2_mos',
#                                           'two_4_mos', 'four_months'),
#                                            c('No stock outs reported',
#                                           '1 week - 1 month ', '1+ - 2 months ', '2+ months'))


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

#Look at proportion of days out of stock. Do you get different totals when you sum 2018 over the whole year versus first 9 months? 

#First, only do 9 months. 
test1 = annual_dt[element_id=='jJuipTLZK4o' & stock_category == "number_of_days_stocked_out", .(facs=length(unique(org_unit_id))), by=dps]

test2 = annual_dt[element_id=='jJuipTLZK4o' & stock_category == "number_of_days_stocked_out", .(value=sum(value, na.rm = TRUE)), by = c('element_id', 'element', 'dps', 'year', 'id')] #Collapse here, because you want to get rid of the date-level. 
test2 = merge(test1, test2, by='dps')
test2[, treat_per_fac:=round(value/facs, 2)]

#Now, do the whole year. 
test3 = dt[element_id=='jJuipTLZK4o' & stock_category == "number_of_days_stocked_out", .(facs=length(unique(org_unit_id))), by=dps]

test4 = dt[element_id=='jJuipTLZK4o' & stock_category == "number_of_days_stocked_out", .(value=sum(value, na.rm = TRUE)), by = c('element_id', 'element', 'dps', 'year', 'id')] #Collapse here, because you want to get rid of the date-level. 
test4 = merge(test3, test4, by='dps')
test4[, treat_per_fac:=round(value/facs, 2)]


# ------------------------------------------------------
# color palettes

two = c('#91bfdb', '#bd0026')
ratio_colors = brewer.pal(8, 'Spectral')
results_colors = brewer.pal(6, 'Blues')
sup_colors = brewer.pal(6, 'Reds')
ladies = brewer.pal(11, 'RdYlBu')
gents = brewer.pal(9, 'Purples')

graph_colors = c('#bd0026', '#fecc5c', '#74c476','#3182bd', '#8856a7')
tri_sex = c('#bd0026', '#74c476', '#3182bd')
wrap_colors = c('#3182bd', '#fecc5c', '#bd0026', '#74c476', '#8856a7', '#f768a1')
sex_colors = c('#bd0026', '#3182bd', '#74c476', '#8856a7') # colors by sex plus one for facilities
single_red = '#bd0026'


