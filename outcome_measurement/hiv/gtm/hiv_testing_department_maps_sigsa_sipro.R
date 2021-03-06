#----------------------------------------
# Audrey Batzel
# 7/08/2019
# map combined sigsa/sigpro data
#----------------------------------------

#-----------------------
# Install packages 
# ----------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(RColorBrewer)
library(gridExtra)
library(openxlsx)
# ----------------------

#----------------------------------------
# Set up directories 
#----------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')
mapping_dir = paste0(j, '/Project/Evaluation/GF/mapping/gtm/')

# input
inFile = paste0(dir, 'prepped/combined_sigsa_sipro_corrected_for_graphing.rds')
gtm_adm1 = paste0(mapping_dir,'GTM_adm1.shp')

# output files
# out1 = paste0(dir, 'visualizations/maps_annual_hiv_testing_by_dept.pdf')
outFile = paste0(dir, 'visualizations/maps_annual_hiv_testing_by_dept_pop.pdf')

# read in combined data file:
dt = readRDS(inFile)
shape = shapefile(gtm_adm1)

# color palettes
ratio_colors <- rev(brewer.pal(8, 'Spectral'))
results_colors <- brewer.pal(6, 'Blues')
sup_colors <- brewer.pal(6, 'Reds')
rdywbe <- brewer.pal(11, 'RdYlBu')
purples <- brewer.pal(9, 'Purples') 
#----------------------------------------

#----------------------------------------
# geomatching between shapefile and data
#----------------------------------------
shape@data$NAME_1 = tolower(shape@data$NAME_1)
shape@data = as.data.table(shape@data)

shape@data[ NAME_1 == 'quich�', NAME_1 := "quich�"]
shape@data[ NAME_1 == 'solol�', NAME_1 := "solol�"]
shape@data[ NAME_1 == 'pet�n', NAME_1 := "pet�n"]
shape@data[ NAME_1 == 'sacatep�quez', NAME_1 := "sacatep�quez"]
shape@data[ NAME_1 == 'totonicap�n', NAME_1 := "totonicap�n"]
shape@data[ NAME_1 == 'suchitep�quez', NAME_1 := "suchitep�quez"]

dt[ match_dept == 'quetzaltenango', match_dept := "quezaltenango"]

# check that names are matched
data_dept =unique(dt$match_dept)
data_dept =data_dept[order(data_dept)]
map_dept =unique(shape@data$NAME_1)
data_dept[!data_dept %in% map_dept]
map_dept[!map_dept %in% data_dept]
#----------------------------------------

#----------------------------------------
# set up data for mapping
#----------------------------------------
# fortify to get coordinates
coords = as.data.table(fortify(shape, region='NAME_1'))

# create a year variable
dt[, year := year(date)]
# subset data
dt = dt[,.(match_dept, year, date, hiv_test_comp, hiv_positive, hiv_confirmatoryTest_comp, hiv_confirmatoryTest_positive, pop, trans, pregnant, pueblo, linguistic_community, nationality, sr)]

# identify kvps 
dt[ , msm := ifelse(grepl(pop, pattern = 'msm'),TRUE,FALSE)]
dt[ , csw := ifelse(grepl(pop, pattern = 'csw'),TRUE,FALSE)]
dt[ , prisoner := ifelse(grepl(pop, pattern = 'prisoner'),TRUE,FALSE)]
dt[ , migrant := ifelse(grepl(pop, pattern = 'migrant'),TRUE,FALSE)]
dt[ , military := ifelse(grepl(pop, pattern = 'military'),TRUE,FALSE)]
dt[, pop := NULL]

# make table of results by pop
dt_table = dt[year %in% 2015:2017, .(match_dept, year, date, hiv_test_comp, hiv_positive, hiv_confirmatoryTest_comp, hiv_confirmatoryTest_positive, trans, pregnant, msm, csw, prisoner, migrant)]
dt_table = melt.data.table(dt_table, id.vars = c('match_dept', 'year', 'date', 'hiv_test_comp', 'hiv_positive', 'hiv_confirmatoryTest_comp', 'hiv_confirmatoryTest_positive'), variable.name = 'pop') 

dt_table_output = data.table(pop = character(), `Tests completed`=numeric(), `Tests positive`= numeric()) 

for (p in unique(dt_table$pop)) {
  add = dt_table[pop == p & value == TRUE, ]
  add = add[, .(`Tests completed` = sum(hiv_test_comp, na.rm = TRUE),
               `Tests positive` = sum(hiv_positive, na.rm = TRUE)), 
            by = 'pop']
  dt_table_output = rbind(dt_table_output, add)
}
dt_table_output_add = data.table(pop = 'All patients', `Tests completed`= sum(dt_table$hiv_test_comp, na.rm = TRUE),
                                 `Tests positive`= sum(dt_table$hiv_positive, na.rm = TRUE)) 

dt_table_output = rbind(dt_table_output, dt_table_output_add)

dt_table_output[, `Test positivity rate` := (`Tests positive` / `Tests completed`)*100]
setnames(dt_table_output, 'pop', 'Population')

# write.xlsx(dt_table_output, paste0(dir, 'prepped/table_of_key_pop_testing_values.xlsx'))

# sum data by dept and year
id_vars = c('match_dept', 'year')
dt = dt[ match_dept %in% coords$id, ]
dt = dt[ year %in% 2015:2017, ]

all_patients = dt[ ,.(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
                      hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
                   by=id_vars]

all_patients[, pos_rate := ((hiv_positive / hiv_test_comp) *100)]
all_patients_long = melt.data.table(all_patients, id.vars = id_vars)

sum_by_kvp = function( var ){
  dt2 = dt[ get(var) == TRUE, .(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
                                 hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
             by=c(id_vars)] 
  dt2[, pos_rate := ((hiv_positive / hiv_test_comp) *100)]
  dt2_long = melt.data.table(dt2, id.vars = id_vars)
}
preg_long = sum_by_kvp('pregnant')
migr_long = sum_by_kvp('migrant')
trans_long = sum_by_kvp('trans')
pris_long = sum_by_kvp('prisoner')
msm_long = sum_by_kvp('msm')
csw_long = sum_by_kvp('csw')
mili_long = sum_by_kvp('military')
#----------------------------------------

#----------------------------------------
# maps by year and variable
#----------------------------------------
all = data.table( expand.grid( year = unique(dt$year), match_dept = unique(dt$match_dept), variable = c('hiv_test_comp', 'hiv_positive', 'pos_rate') ))

list_of_dts = list("All people" = all_patients_long, "Transgender people" = trans_long, "Migrants" = migr_long, "Prisoners" = pris_long,
                   "MSM" = msm_long, "CSWs" = csw_long, "Pregnant Women" = preg_long, "Military" = mili_long)

pdf(outFile, height = 12, width = 12) 
for (x in 1:length(list_of_dts)){
  title = names(list_of_dts[x])
  
  dt = list_of_dts[[x]]
  dt = merge(dt, all, all = TRUE)
  
  graphData = merge(dt, coords, by.x = 'match_dept', by.y = 'id', allow.cartesian = TRUE)
  
  map1 = ggplot() + geom_polygon(data=graphData[variable == 'hiv_test_comp'], aes(x=long, y=lat, group=group, fill=value)) + 
    coord_equal() + 
    geom_path(data=graphData, aes(x=long, y=lat, group=group), color="darkgray", size = 0.2) +
    facet_grid(~year) +
    scale_fill_gradientn(colors=results_colors, na.value = "grey70") + theme_void() +
    theme(plot.title = element_text(size = 18, vjust = 7), legend.title=element_text(size=16), legend.text=element_text(size=10), strip.text=element_text(size=14)) +
    labs(title= paste0("Annual HIV tests completed: ", title), fill=paste0('Number of tests completed')) 
  
  map2 = ggplot() + geom_polygon(data=graphData[variable == 'hiv_positive'], aes(x=long, y=lat, group=group, fill=value)) + 
    coord_equal() + 
    geom_path(data=graphData, aes(x=long, y=lat, group=group), color="darkgray", size = 0.2) +
    facet_grid(~year) +
    scale_fill_gradientn(colors=sup_colors, na.value = "grey70") + theme_void() +
    theme(plot.title = element_text(size = 18, vjust = 7), legend.title=element_text(size=16), legend.text=element_text(size=10), strip.text=element_text(size=14)) +
    labs(title= paste0("Annual number of HIV tests positive: ", title), fill=paste0('Number of tests positive')) 
  
  map3 = ggplot() + geom_polygon(data=graphData[variable == 'pos_rate'], aes(x=long, y=lat, group=group, fill=value)) + 
    coord_equal() + 
    geom_path(data=graphData, aes(x=long, y=lat, group=group), color="darkgray", size = 0.2) +
    facet_grid(~year) +
    scale_fill_gradientn(colors=ratio_colors, na.value = "grey70") + theme_void() +
    theme(plot.title = element_text(size = 18, vjust = 7), legend.title=element_text(size=16), legend.text=element_text(size=10), strip.text=element_text(size=14)) +
    labs(title= paste0("Annual percentage of HIV tests positive: ", title), fill=paste0('Percentage of tests positive')) 

  # create better spacing for grid.arrange of maps
  margin = theme(plot.margin = unit(c(.25,.25,.25,.25), "cm"))
  #print all together:
  grid.arrange(grobs = lapply( list(map1, map2, map3), "+", margin), ncol = 1, nrow = 3)
}  
dev.off()
#----------------------------------------


sd_cols = c('hiv_test_comp', 'hiv_positive')
dept_year_results = dt[, lapply(.SD, sum, na.rm = TRUE), .SDcols = sd_cols, by = .(match_dept, year)]
dept_year_results[, test_positivity := (hiv_positive/hiv_test_comp)*100]
dept_year_results = melt.data.table(dept_year_results, id.vars = c('year', 'match_dept'))
dept_year_results = dcast.data.table(dept_year_results, match_dept ~ variable + year, value.var = 'value')

natl_year_results = trans_wide[, lapply(.SD, sum, na.rm = TRUE), .SDcols = sd_cols, by = .(year)]
natl_year_results[, test_positivity := (hiv_positive/hiv_test_comp)*100]
