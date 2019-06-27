# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Explore supply chain data for PNLS, mainly on HIV test kits. 
# DATE: Last updated June 2019
# --------------------------------------------------------------------

#---------------------------------------------------------------------
# TO DO LIST 
#  Add available-usable stock to title where it refers to it in slides, 
# Split sex-stratified variables out into their own PDF. 
# 

# Not urgent - check online PNLS dashboard and make sure new first-line treatment exists. 
#---------------------------------------------------------------------

#Observations about this dataset: 
# There is no data for December 2017. 
# If stock category is NA, then it is sex-stratified. 
#We seem to just have data gaps for the same 3 districts in 2017? 

#Set up R, and read in data. 
#Read in data set, and source the setup. 

#-------------------------------------------------
# Read in PNLS and SIGL data, and merge 
# ------------------------------------------------
pnls = readRDS(paste0(dir, 'pnls_sets/pnls_drug_2017_01_01_2019_02_01.rds'))
pnls = pnls[, -c('subpop', 'maternity', 'case')]
setnames(pnls, 'element', 'element_eng') #For this data, the translation isn't as important because we don't need to do subpop extraction. Just grepping on drug names. 

pnls[stock_category=='Nbr de jours RS', stock_category:='number_of_days_stocked_out']
pnls[stock_category=="Stock disponible utilisable", stock_category:='available_usable_stock']
pnls[stock_category=='Stock Initial', stock_category:='initial_stock']
pnls[stock_category=="Entrée", stock_category:='input']
pnls[stock_category=='Sortie', stock_category:='output']

sigl = readRDS(paste0(dir, 'sigl/sigl_prepped.rds'))

#Pull relevant SIGL test kit variables. (Determine, HIV 1+2, and Double-check days out of stock and available usable stock)
# unique(sigl[grepl("determine", element_eng, ignore.case=T), .(element_eng, element_id)])
#unique(sigl[grepl("double check", element_eng, ignore.case=T), .(element_eng, element_id)])

sigl_element_ids = c("T4gD1QoPYzK", "VwDtaCYTugc", "Swrsj0VOZ74", "o7edQT2Wizp")
sigl = sigl[element_id%in%sigl_element_ids]
unique(sigl[, .(element_eng, element_id)]) #Visual check to make sure this is what you want. NEED TO VERIFY WE DON'T HAVE ANY UNIGOLD IN THE DATASET EMILY 6/18/19

#Drop irrelevant variables before appending, and generate relevant ones. 
sigl = sigl[, -c('category_id', 'category', 'opening_date', 'element', 'country', 'data_set', 'data_set_id', 'year')]
pnls = pnls[, -c('sex', 'age', 'tb', 'pnls_set')]

setnames(pnls, "facility_level", "level")

sigl[grepl("Days out of stock", element_eng), stock_category:="number_of_days_stocked_out"]
sigl[grepl("available usable", element_eng), stock_category:="available_usable_stock"]
stopifnot(nrow(sigl[is.na(stock_category)])==0)

#After this edit, What names are different between the two files? 
names(sigl)[!names(sigl)%in%names(pnls)]
names(pnls)[!names(pnls)%in%names(sigl)]

#Are the element IDs the same between the two datasets? 
unique(pnls[grepl("determine|double", element_eng, ignore.case=T), .(element_eng, element_id)])
unique(sigl[, .(element_eng, element_id)])

#Correct SIGL element IDs to be the same as PNLS IDs
sigl[element_id=="Swrsj0VOZ74", element_id:="k3JmmwNHkmY"] #Double check, days out of stock 
sigl[element_id=="o7edQT2Wizp", element_id:="k3JmmwNHkmY"] #Double check, available usable stock
sigl[element_id=="VwDtaCYTugc", element_id:="Gv1UQdMw5wL"] #Determine, days out of stock
sigl[element_id=="T4gD1QoPYzK", element_id:="Gv1UQdMw5wL"] #Determine, available usable stock 

sigl[element_id=="k3JmmwNHkmY", element_eng:="HIV 1/2, Double Check Gold 100 Test Kit"]
sigl[element_id=="Gv1UQdMw5wL", element_eng:="HIV 1 + 2, Determine Complete, 100 test kit"]

#Tag the two datasets so it's clear which observation came from which. 
pnls[, source:="PNLS"]
sigl[, source:="SIGL"]

#Make sure there isn't any overlap in facility reporting between two datasets 
pnls_facs = unique(pnls$org_unit_id)
sigl_facs = unique(sigl$org_unit_id)

overlap = pnls_facs[pnls_facs%in%sigl_facs]
sigl_only = sigl_facs[!sigl_facs%in%pnls_facs]
pnls_only = pnls_facs[!pnls_facs%in%sigl_facs]

print(paste0("There are ", length(overlap), " overlapping facilities, ", length(pnls_only), " reporting only to PNLS, and ", length(sigl_only), " reporting only to SIGL."))

#Do this check at the month-level, to get a better idea if the monthly stockout data agrees between the two sources. 

#Compare available, usable stock and stockouts for overlapping facilities for Determine (first-line testing)
overlap_dt = rbind(pnls, sigl, use.names=T, fill=T)
overlap_dt[, concat:=paste0(date, "_", org_unit_id)]

date_frame = data.table(month = seq(1, 12, by=1), expected_days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
overlap_dt[, month:=month(date)]
overlap_dt = merge(overlap_dt, date_frame, all.x = TRUE, by = 'month')

overlap_dt[value>expected_days & stock_category == "number_of_days_stocked_out", value:=NA] #Replace impossible days stocked out with NA


#What facilities are reporting to both PNLS and SIGL by month for Determine available usable stock? 
overlap_avail = overlap_dt[stock_category=="available_usable_stock" & element_id=="Gv1UQdMw5wL", .(org_unit_id, source, date)]
overlap_avail = overlap_avail[, .(facs=.N), by=c('date', 'org_unit_id')]
stopifnot(overlap_avail$facs%in%c(1, 2)) #There's a problem if you have values other than these! 
overlap_avail = overlap_avail[facs>1] #When you have more than 1 here, it means you had reporting from both sources. 
overlap_avail[, concat:=paste0(date, "_", org_unit_id)]

avail_determine = overlap_dt[concat%in%overlap_avail$concat & stock_category=="available_usable_stock" & element_id=="Gv1UQdMw5wL", .(value=sum(value, na.rm=T)), by=c('date', 'source')]
ggplot(avail_determine, aes(x=date, y=value, color=source))+
  geom_line() + 
  theme_bw() + 
  labs(title="Comparison of 'available, usable stock' for Determine\nin availping PNLS and SIGL facilities", x='Date', y='Available, Usable Stock')
ggsave("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pnls/supplychain_final_graphs/pnls_sigl_determine_avail.png")

#What facilities are reporting to both PNLS and SIGL by month for Determine available usable stock? 
overlap_so = overlap_dt[stock_category=="number_of_days_stocked_out" & element_id=="Gv1UQdMw5wL", .(org_unit_id, source, date)]
overlap_so = overlap_so[, .(facs=.N), by=c('date', 'org_unit_id')]
stopifnot(overlap_so$facs%in%c(1, 2)) #There's a problem if you have values other than these! 
overlap_so = overlap_so[facs>1] #When you have more than 1 here, it means you had reporting from both sources. 
overlap_so[, concat:=paste0(date, "_", org_unit_id)]
          
so_determine = overlap_dt[concat%in%overlap_so$concat & stock_category=="number_of_days_stocked_out" & element_id=="Gv1UQdMw5wL", .(value=sum(value, na.rm=T)), by=c('date', 'source')]
ggplot(so_determine, aes(x=date, y=value, color=source))+
  geom_line() + 
  theme_bw() + 
  labs(title="Comparison of number of days stocked out for Determine\nin overlapping PNLS and SIGL facilities", x='Date', y='Stock-out days')
ggsave("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pnls/supplychain_final_graphs/pnls_sigl_determine_so.png")

#Save the SIGL dataset at this point in case you want to use it later 
saveRDS(sigl, "J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/sigl/sigl_for_pnls_supplychain.rds")
#Subset SIGL to only its unique facilities - we might want to do a more nuanced merge than this, around reporting completeness. 
sigl_subset = sigl[org_unit_id%in%sigl_only]

dt = rbind(pnls, sigl_subset, use.names=T)

#-------------------------------------------------
# Read in shapefile 
# ------------------------------------------------
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

#Make it possible to merge the data with the shape file
shape_names = data.table(id = seq(0, 25, by=1), NAME_1=shapefile@data$NAME_1) #This matches the data when you fortify the shapefile below

dt[, NAME_1:=standardizeDPSNames(dps)]
dt[!NAME_1%in%shapefile@data$NAME_1] #Check that merge will work correctly - this data table should have 0 rows. 

dt = merge(dt, shape_names, by='NAME_1', all.x = TRUE)

#Make a year variable - be careful with the gaps in the data. 
dt[, year:=year(date)]

#--------------------------------------------------------------
#Clean the data 
#--------------------------------------------------------------
#Fix DPS names 
dt[, dps:=standardizeDPSNames(dps)]

#Check to make sure there aren't impossible reporting periods for stock-out days per month, and drop these values
date_frame = data.table(month = seq(1, 12, by=1), expected_days = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
dt[, month:=month(date)]
dt = merge(dt, date_frame, all.x = TRUE, by = 'month')

#unique(dt[value >31 & stock_category == "number_of_days_stocked_out", .(value)]) #Caitlin do we want to clean any of these? 
dt[value>expected_days & stock_category == "number_of_days_stocked_out", impossible_so_days:=TRUE] #Create a boolean value to flag which NAs you've created. 
print(paste0(round((nrow(dt[source=="SIGL" & impossible_so_days==TRUE])/nrow(dt[source=="SIGL"]))*100, 2), "% of SIGL stock-out data has impossible values (SO days greater than days in month)."))
print(paste0(round((nrow(dt[source=="PNLS" & impossible_so_days==TRUE])/nrow(dt[source=="PNLS"]))*100, 2), "% of PNLS stock-out data has impossible values (SO days greater than days in month)."))

dt[value>expected_days & stock_category == "number_of_days_stocked_out", value:=NA] #Replace impossible days stocked out with NA

# Generate a variable 'any_stock_out' where the denominator is all stock_category 'number of days stocked out' 
#   (Including the 'impossible' NAs for now), and the numerator = 1 if value != 0 & value != NA. 
#   (for that given month, they had at least one day stocked out.)
#   Numerator = 0 if value == 0. Numerator == NA if value == NA. 
dt[stock_category == 'number_of_days_stocked_out', any_stock_out:=0]
dt[!is.na(any_stock_out) & value>0 & !is.na(value), any_stock_out:=1]
dt[is.na(value) & !is.na(any_stock_out), any_stock_out:=NA]

#If you've replaced the value with NA, also make expected days NA. 
dt[is.na(value) & impossible_so_days==TRUE, expected_days:=NA]

#Check if there are any observations where available stock is greater than 0, but facility reported a stockout for the whole month. 
available_stock = unique(dt[stock_category=="available_usable_stock" & value>0 & !is.na(value), .(date, org_unit_id, element_id)])
available_stock[, concat:=paste0(org_unit_id, "_", element_id, "_", date)]
dt[, concat:=paste0(org_unit_id, "_", element_id, "_", date)]
dt[concat%in%available_stock$concat & stock_category=="number_of_days_stocked_out" & value>0 & !is.na(value), so_error:=TRUE]

print(paste0(round((nrow(dt[so_error==TRUE])/nrow(dt))*100, 2), "% of data have coding errors for stockouts, where available-usable stock was >0 but a stock-out was reported."))

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

unique(dt[is.na(regimen), .(regimen, element_eng)][order(regimen)])
#Are there any other second-line regimens we can pull here? 

#Save a cleaned data set here so you can run quantile regression 
saveRDS(dt, paste0(dir, "pnls_final/pnls_drug.rds"))

# 
# 
# 
# #---------------------------
# # drop out the 12 facilities that never reported 
# 
# missing = full_data[ , .(check=all(is.na(arvs)), check_t=all(is.na(test_kits))), by=facility]
# missing = missing[check==TRUE & check_t==TRUE]
# full_data = full_data[!facility %in% missing$facility]
# 
# #------------------------------------
# # merge in the regions
# regions = fread(paste0(j, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
# regions = regions[ ,.(region = region10_name, district = dist112_name)]
# regions = regions[!duplicated(district)]
# full_data = merge(full_data, regions, by='district', all.x=T)
# 
# # check that every district has a region associated with it
# full_data[is.na(region)]
# 
# #-------------------------------------
# # save the output
# 
# # get the minimum and maximum year and add to the file name for export
# min_year = full_data[ , min(year)]
# max_year = full_data[ , max(year)]
# 
# # save as a data table
# saveRDS(full_data, paste0(OutDir, 'arv_stockouts_', min_year, '_', max_year, '.rds'))
# 
# #----------------------------
# 
# 
