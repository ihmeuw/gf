# ARV stockouts by facility - visualize the data 
# create data tables for distinct graphs
# Caitlin O'Brien-Carelli, modified by Emily Linebarger
# 12/14/2018

#Read in data set, and source the setup. 
rm(list=ls())

# -----------------------------------------------
# Set up R
#------------------------------------------------
library(data.table)
library(raster)
library(ggplot2)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(maptools)

setwd("C:/Users/elineb/Documents/gf")

j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j,  'Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/') #Home directory
saveDir = paste0(j, 'Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pnls/')
codeDir = "./outcome_measurement/all/cod/dhis/dhis_analysis/pnls_supplychain/"

#---------------------------------------------------------
#Set global variables here so you can re-run for any drug. 
# This can also be set up as a for-loop. 
#---------------------------------------------------------

#Run just for test kits right now. 
target_drugs = c("Gv1UQdMw5wL", "ctP0MNHiq3B", "k3JmmwNHkmY")
drug_names = c("Determine test kit", "Uni-Gold test kit", "Double-check test kit")
full_date_range = "Jan 2017 - Dec 2018" #Imported into the graphs automatically - update as you get more data. 
partial_date_range = "Same time period: January - September" #Date range of 'AnnualDT' below - to have comparable time periods. Update as you get more data. 

#-------------------------------------------------
# Read in data 
# ------------------------------------------------
source('./core/standardizeDPSNames.R')

preppedDT = readRDS(paste0(dir, 'pnls_drug.rds'))
dt = copy(preppedDT)

#Read in the shapefile
shapefile = shapefile("J:/Project/Evaluation/GF/mapping/cod/gadm36_COD_shp/gadm36_COD_1.shp")
shapefile@data$NAME_1 = standardizeDPSNames(shapefile@data$NAME_1)

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

# Add in a clustered level variable to make scatter plots with later. THIS NEEDS TO BE VERIFIED. 
dt[ level%in%c('health_post', 'dispensary'), level2:=2]
dt[ level%in%c('health_center', 'reference_health_center', 'medical_center', 'clinic', 'polyclinic'), level2:=3]
dt[ level%in%c('general_reference_hospital', 'hospital_center', 'hospital', 'secondary_hospital', 'medical_surgical_center'), level2:=4]

# ------------------------------------------------------
# Prep some color palettes
#-------------------------------------------------------

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

colScale = scale_fill_gradient2(low="red", high="green", midpoint=0)
colScale2 = scale_fill_gradient2(low="green", high="red", midpoint=0)

#-------------------------------------
#MAKE DRUG-SPECIFIC GRAPHS 
#-------------------------------------
for (i in 1:length(target_drugs)){
  drug_id = target_drugs[i] #Pull the element ID
  drug_name = drug_names[i] #Pull a clean name for the drug to make pretty labels
  
  print(drug_id)
  print(drug_name)
  #---------------------------------------------------------
  # Make specialized 'base' data tables.  
  # These are labeled in Snake Case to distinguish from data tables 
  # created below. 
  #---------------------------------------------------------
  FacsReportingByDate = dt[, .(facilities=length(unique(org_unit_id))), by=date]
  FacsReportingDrugByDate = dt[element_id==drug_id, .(facs_reporting_on_drug=as.numeric(length(unique(org_unit_id)))), by=date]
  FacsReportingByDPS = dt[, .(facilities_by_dps=length(unique(org_unit_id))), by=c('year', 'dps')]
  
  StockoutsForTargetDrug = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & value>0 & !is.na(value)]
  StockoutsForTargetDrug[, facs_stocked_out:=as.numeric(length(unique(org_unit_id))), by=date]
  
  #Create an 'annual' dataset to address the reporting lag at the end of 2018. 
  # Subset both years to only the first 9 months so they are comparable. 
  AnnualDT = dt[(date< "2018-09-01" & year == 2018) | (date < '2017-09-01' & year == 2017)] 
  unique(AnnualDT[, .(date, year)][order(date, year)]) #Visual inspection for the subset above
  
  #---------------------------------------------------
  # reporting completeness - graphs 1, 2
  #---------------------------------------------------
  
  #Did they report for the specific drug you're targeting? 
  report = merge(FacsReportingByDate, FacsReportingDrugByDate, by='date', all.x=TRUE)
  
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
  
  #Generate a data table on reporting completeness - both just by date, and by date/dps
  report_by_dps = merge(FacsReportingByDPS, dt[element_id==drug_id], by=c('year', 'dps'))
  report_by_dps = unique(report_by_dps[, .(year, id, facilities_by_dps, dps)]) 
    
  report_map = merge(report_by_dps, coord_ann, by = c('id', 'year'), all.y = TRUE, allow.cartesian = TRUE)
  
  #-------------------------------------------------------------------------------------------------------------
  # stock outs for target drug and stock-outs where number of facilities is over a certain threshold- 3, 4, 5
  #--------------------------------------------------------------------------------------------------------------
  drug_so = StockoutsForTargetDrug[, .(facilities_stocked_out=as.numeric(length(unique(org_unit_id)))), by=date] #Count all facilities that reported stock out information for this drug, with greater than 0 days in the month. 
  drug_so = merge(drug_so, FacsReportingDrugByDate, by='date', all.x=T)
  drug_so[is.na(facs_reporting_on_drug), facs_reporting_on_drug:=0]
  drug_so[ ,ratio:=round(100*(facilities_stocked_out/facs_reporting_on_drug), 2)]
  
  # calculate 50% of facilities to graph above a reporting threshold
  n = dt[, length(unique(org_unit_id))/4 ]
  drug_so_thresh = drug_so[facs_reporting_on_drug > n]
  
  drug_so = melt(drug_so, id.vars='date')
  drug_so$variable = factor(drug_so$variable, c('facs_reporting_on_drug', 'facilities_stocked_out', 'ratio'),
                        c(paste0('Facilities reporting on ', drug_name), paste0('Facilities with a stockout in ', drug_name), 
                          paste0('Percentage of facilities stocked out of ', drug_name)))
  
  # run the same code on the threshold subset
  drug_so_thresh = melt(drug_so_thresh, id.vars='date')
  drug_so_thresh$variable = factor(drug_so_thresh$variable, c('facs_reporting_on_drug', 'facilities_stocked_out', 'ratio'),
                                   c(paste0('Facilities reporting on ', drug_name), paste0('Facilities with a stockout in ', drug_name), 
                                     paste0('Percentage of facilities stocked out of ', drug_name)))
  
  #-----------------------------------
  # Stockot weeks by facility - 6, 7
  #-----------------------------------
  
  # Number of weeks of stockout by facility
  so_days = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value),
               .(days=sum(value, na.rm=T)), by=.(year, org_unit_id)] #Do we want to have greater than 0 in this calculation? 
  # Sanity check - are there any days greater than 365? This shouldn't be possible. 
  range(so_days$days)
  so_days = so_days[days >=1] #Caitlin this was just > 1 in your code? Only keep where there was one or more days of stockouts. 
  so_days = so_days[ ,.(facilities=length(unique(org_unit_id))), by=.(days, year)] #EMILY IS THIS WHAT YOU WANT HERE? 
  
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
  
  #-----------------------------------
  # percentage of weeks stocked out
  #-----------------------------------
  #Weeks facilities were stocked out / Total weeks in which facilities reported. 
  stock = dt[element_id==drug_id & stock_category=="number_of_days_stocked_out", .(total_days=sum(expected_days, na.rm=T)), by=c('year', 'id')]#Count all facilities that reported stock out information for this drug, with greater than 0 days in the month. 
  stock_add = dt[element_id==drug_id & stock_category=="number_of_days_stocked_out", .(days_out=sum(value, na.rm=T)), by=c('year', 'id')]
  stock = merge(stock, stock_add, by=c('year', 'id'), all=T)
  stock[, percent_out:=round(100*(days_out/total_days), 1)]
  
  unique(stock[percent_out==100, .(id, year, total_days, days_out)]) #Verifying that there are some cases with 100% of days stocked-out. 
  stock[is.na(percent_out)]
  
  stock = merge(stock, coord_ann, by=c('id', 'year'), all.y=T)
  
  #---------------------------------------
  # Stockout maps- 8:12
  #---------------------------------------
  
  # total facility-weeks of stock outs 
  # exclude the months that are not in all years (e.g. december 2018)
  stockout = dt[month(date)!=12 & element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), 
                .(days_stocked_out=sum(value, na.rm=T)), by=.(year, id)] #Get the total number of days stocked out for each DPS. 
  stockout[, weeks_stocked_out:=ceiling(days_stocked_out/7)]
  so_map = merge(stockout, coord_ann, by=c('id', 'year'), all.y=TRUE)
  
  # mean weeks stocked out 
  # number of weeks of stockout divided by art sites reporting 
  num_facilities = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out' & !is.na(value), .(facilities=length(unique(org_unit_id))), by=.(year, id)]
  num_facilities = merge(stockout, num_facilities)
  num_facilities[ , mean_weeks:=round((weeks_stocked_out/facilities), 1)]
  num_facilities[ , mean_days:=round((days_stocked_out/facilities), 1)]
  so_map_norm = merge(num_facilities, coord_ann, by=c('id', 'year'), all.y=TRUE)
  
  # rates of change in facility-weeks per year
  stockout[ , year2:=paste0('n', year)]
  roc = dcast(data = stockout, id ~ year2, value.var=c('days_stocked_out', 'weeks_stocked_out'))
  roc[ , change:=(days_stocked_out_n2018 - days_stocked_out_n2017)]
  roc_map = merge(coord, roc, by='id')
  
  # only districts with more stockouts in 2018 than 2017
  roc_map_alt = merge(coord, roc, by='id')
  roc_map_alt[change <=0, change:=NA]
  
  #---------------------------------------------------
  # Mean test-kits per facility 
  #---------------------------------------------------
  facs_per_district = dt[element_id==drug_id & stock_category == "available_usable_stock", .(facs=length(unique(org_unit_id))), by=c('dps','year')] #Find the number of facs per district with available test-kit stock. 
  
  kits_per_facility = dt[element_id==drug_id & stock_category == "available_usable_stock", .(element_id, value, dps, year, id, date)]
  kits_per_facility = kits_per_facility[(date< "2018-09-01" & year == 2018) | (date < '2017-09-01' & year == 2017)] #Subset to handle time lags. 
  kits_per_facility = kits_per_facility[, .(value=sum(value, na.rm = TRUE)), by = c('element_id', 'dps', 'year', 'id')] #Collapse here, because you want to get rid of the date-level. 
  kits_per_facility = merge(kits_per_facility, facs_per_district, by=c('dps', 'year'), all.x = TRUE)
  kits_per_facility[, kits_per_fac:=round(value/facs, 2)]
  
  #Merge with coordinate system so it can be mapped 
  kits_per_fac_map = merge(kits_per_facility, coord_ann, by=c('id', 'year'), all.y=TRUE)
  
  # # scatter plots (facility level)
  dt[ level%in%c('health_post', 'dispensary'), level2:=2]
  dt[ level%in%c('health_center', 'reference_health_center', 'medical_center', 'clinic', 'polyclinic'), level2:=3]
  dt[ level%in%c('general_reference_hospital', 'hospital_center', 'hospital', 'secondary_hospital', 'medical_surgical_center'), level2:=4]
  
  #See stockouts clustered by level, full time series. 
  scatter = dt[element_id==drug_id & stock_category=='number_of_days_stocked_out', .(days_out=sum(value, na.rm=T)), by=.(org_unit_id, level2)]
  scatter[, weeks_out:=days_out/7]
  #See stockouts clustered by level, subset to only Jan-Sep. 
  scatter2 = AnnualDT[element_id==drug_id & stock_category=='number_of_days_stocked_out', .(days_out=sum(value, na.rm=T)), by=.(org_unit_id, level2, year)]
  scatter2[, weeks_out:=days_out/7]
  
  #---------------------------------------
  # finale maps - categorical arv stockouts

  final = dt[element_id==drug_id & stock_category=="number_of_days_stocked_out",.(days_out=sum(value, na.rm=T)) , by=.(org_unit_id, year, id) ]
  final = final[ ,.(org_unit_id=length(unique(org_unit_id))), by=.(days_out, year, id)]
  final[ ,months:=(days_out/30)]
  final[months==0, category:='no_stock_out']
  final[0 < months & months <= 1, category:='one_week_2_mos']
  final[1 < months & months <= 2, category:='two_4_mos']
  final[2 < months, category:='four_months']
  final = final[ ,.(value=sum(org_unit_id)), by=.(year, id, variable=category)]
  final = dcast(final, year+id ~ variable)

  final[is.na(no_stock_out), no_stock_out:=0]
  final[is.na(one_week_2_mos), one_week_2_mos:=0]
  final[is.na(two_4_mos), two_4_mos:=0]
  final[is.na(four_months), four_months:=0]

  final = merge(final, coord_ann, by=c('id', 'year'), all.y=TRUE)
  final = melt(final, id.vars=c('year', 'id', 'long', 'lat', 'order', 'hole',
                                'piece', 'group'))

  final$variable = factor(final$variable, c('no_stock_out', 'one_week_2_mos',
                                            'two_4_mos', 'four_months'),
                                             c('No stock outs reported',
                                            '1 week - 1 month ', '1+ - 2 months ', '2+ months'))
  
  
  
  #---------------------------------------------------
  # Data tables for additional analyses
  #---------------------------------------------------
  # Generate a variable 'monthly_so_rate' That represents the percentage of days in the month that a given facility was 
  # Stocked out. *Note that you'll have to recalculate this based on the level of aggregation (facility, dps, etc.)
  # Denominator represents total potential days in the month (Jan = 31, Feb = 28, etc.) and only gets created for 
  # 'number_of_days_stocked_out'. 
  # Numerator is the number of days stocked out. 
  #Do this only for first line treatment drugs right now! 
  {
    facs_per_district = dt[stock_category == "number_of_days_stocked_out" & !is.na(value) & element_id==drug_id, .(facs=length(unique(org_unit_id))), by=c('dps', 'date')] #Exclude impossible day values here. 
    
    monthly_so_rate_dps = dt[stock_category == 'number_of_days_stocked_out' &!is.na(value) & element_id==drug_id, .(id, value, date, expected_days, dps)]
    monthly_so_rate_dps = monthly_so_rate_dps[, .(value=sum(value, na.rm = TRUE)), by=c('id', 'date', 'expected_days', 'dps')]
    
    dups = monthly_so_rate_dps[duplicated(monthly_so_rate_dps, by=c('id', 'date'))]
    dups = merge(dups, monthly_so_rate_dps, by=c('id', 'date'))
    stopifnot(nrow(dups)==0)
    
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
    facs_per_district_hp = health_posts[stock_category == "number_of_days_stocked_out" & !is.na(value) & element_id==drug_id, .(facs=length(unique(org_unit_id))), by=c('dps', 'date')] #Exclude impossible day values here. 
    
    monthly_so_rate_dps_hp = health_posts[stock_category == 'number_of_days_stocked_out' & element_id==drug_id, .(id, value, date, expected_days, dps)]
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
  
  #--------------------------------------------
  # Break apart the categorical maps even further, using the structure of the 'final' data table created above. 
  #Disaggregate everything with more than 2 months stocked out. 
  {
    final2 = dt[element_id==drug_id & stock_category=="number_of_days_stocked_out",.(days_out=sum(value, na.rm=T)) , by=.(org_unit_id, year, id) ]
    final2 = final2[ ,.(org_unit_id=length(unique(org_unit_id))), by=.(days_out, year, id)]
    final2[ ,months:=(days_out/30)]
    
    final2[months==0, category:='no_stock_out']
    final2[0 < months & months <= 2, category:='zero_two_months']
    final2[2 < months & months <= 4, category:='two_four_months']
    final2[4 < months & months <=6, category:='four_six_months']
    final2[6 < months & months <=8, category:='six_eight_months']
    final2[8 < months & months <=10, category:='eight_ten_months']
    final2[months>10, category:='ten_year_months']
    
    final2 = final2[ ,.(value=sum(org_unit_id)), by=.(year, id, variable=category)]
    final2 = dcast(final2, year+id ~ variable)
    
    for (var in c('no_stock_out', 'zero_two_months', 'two_four_months', 'four_six_months', 'six_eight_months', 'eight_ten_months',
                  'ten_year_months')){
      if (var%in%names(final2)){
        final2[is.na(get(var)), (var):=0]
      }
    }
    
    final2 = merge(final2, coord_ann, by=c('id', 'year'), all.y=TRUE)
    final2 = melt(final2, id.vars=c('year', 'id', 'long', 'lat', 'order', 'hole',
                                  'piece', 'group'))
    
    final2$variable = factor(final2$variable,  c('no_stock_out', 'zero_two_months', 'two_four_months', 'four_six_months', 
                                                 'six_eight_months', 'eight_ten_months',
                                                 'ten_year_months'),
                            c('No stock outs reported', '0-2 mo', '2-4 mo', '4-6 mo', '6-8 mo', '8-10 mo', '10-12 mo'))
    
    
    
  }
  
  #-------------------------------------------------------
  # Source the visualization code 
  #-------------------------------------------------------
  source(paste0(codeDir, "3_pnls_supplychain_visualize.R"))
}
