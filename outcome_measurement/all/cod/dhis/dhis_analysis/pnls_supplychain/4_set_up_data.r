#----------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Set up data for PNLS analysis 
# DATE: Updated May 2019
#-----------------------------------------

if (use_sigl_only){
  dt = readRDS("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/sigl/sigl_for_pnls_supplychain.rds")
  dt[, year:=year(date)]
} else { #Use the PNLS/SIGL combined data
  dt = readRDS(paste0(dir, 'pnls_drug.rds'))
}

#Only limit to 2018 for now. 
dt = dt[year<=2018]

#Read in the shapefile
shapefile = shapefile("J:/Project/Evaluation/GF/mapping/cod/gadm36_COD_shp/gadm36_COD_1.shp")
shapefile@data$NAME_1 = standardizeDPSNames(shapefile@data$NAME_1)
shapefile@data$dps = shapefile@data$NAME_1

#Pull in the ID variable for SIGL data only
if (use_sigl_only){
  shape_names = data.table(id = seq(0, 25, by=1), dps=shapefile@data$NAME_1) #This matches the data when you fortify the shapefile below
  dt[!dps%in%shapefile@data$dps] #Check that merge will work correctly - this data table should have 0 rows. 
  dt = merge(dt, shape_names, by='dps', all.x = TRUE)
}

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

# Add in a clustered level variable to make scatter plots with later. 
#Just label health post, health center, and hospital, and call everything else 'other'. 
dt$level = factor(dt$level, c('health_center', 'reference_health_center', 'general_reference_hospital', 
                              'hospital_center', 'medical_center', 'clinic', 'hospital', 'secondary_hospital', 
                              'polyclinic', 'health_post', 'dispensary', 'medical_surgical_center'), 
                  c('Health center', 'Reference health center', 'General reference hospital', 
                    'Hospital center', 'Medical center', 'Clinic', 'Hospital', 'Secondary hospital', 
                    'Polyclinic', 'Health post', 'Dispensary', 'Medical surgical center'))
dt[, level2:=level]
dt[!level%in%c('Health post', 'Health center', 'Hospital'), level2:='Other']


#Do some other prep if you've got SIGL data only - this mimics prep for both datasets together, and code should probably be integrated. EKL 6/18/19
if (use_sigl_only){
  #Check to make sure there aren't impossible reporting periods for stock-out days per month, and drop these values
  date_frame = data.table(month = seq(1, 12, by=1), expected_days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  dt[, month:=month(date)]
  dt = merge(dt, date_frame, all.x = TRUE, by = 'month')
  dt[value>expected_days & stock_category == "number_of_days_stocked_out", impossible_so_days:=TRUE] #Create a boolean value to flag which NAs you've created. 
  dt[value>expected_days & stock_category == "number_of_days_stocked_out", value:=NA] #Replace impossible days stocked out with NA
  
  
  dt[stock_category == 'number_of_days_stocked_out', any_stock_out:=0]
  dt[!is.na(any_stock_out) & value>0 & !is.na(value), any_stock_out:=1]
  dt[is.na(value) & !is.na(any_stock_out), any_stock_out:=NA]
  
  #If you've replaced the value with NA, also make expected days NA. 
  dt[is.na(value) & impossible_so_days==TRUE, expected_days:=NA]
}
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

scale_2018 = scale_fill_gradient2(low = "white", high = "orangered")
scale_2017 = scale_fill_gradient2(low = "white", high = "mediumorchid1")