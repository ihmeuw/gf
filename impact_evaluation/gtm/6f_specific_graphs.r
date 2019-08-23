#--------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Make specific graphs for Guatemala model 
# deliverables 
# DATE: August 2019 
#--------------------------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/gtm/set_up_r.r')

#File paths 
save_loc = "J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/september_terg_presentation/"

# load model results
load(outputFile5a)
data1=copy(data)
urFits1 = copy(urFits)
load(outputFile5b)
data2=copy(data)
urFits2 = copy(urFits)

# load nodeTable for graphing
nodeTable1 = fread(nodeTableFile1)
nodeTable2 = fread(nodeTableFile2)

# ensure there are no extra variables introducted from nodeTable
nodeTable1 = nodeTable1[variable %in% names(data1)]
nodeTable2 = nodeTable2[variable %in% names(data2)]

# compute averages (approximation of standard error, would be better as Monte Carlo simulation)
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
urFits1[, se_ratio.std:=se.std/est.std]
urFits1[, se_ratio:=se/est]
urFit1 = urFits1[, lapply(.SD, mean, na.rm=TRUE), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit1[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit1[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
urFits2[, se_ratio.std:=se.std/est.std]
urFits2[, se_ratio:=se/est]
urFit2 = urFits2[, lapply(.SD, mean, na.rm=TRUE), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit2[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit2[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
# -----------------------------------------------

# load up area shape file, and create basic plot. 
library(rgdal)
library(geosphere)
shp <- readOGR("J:/Project/Evaluation/GF/mapping/gtm/GTM_adm1.shp")
shp_data = data.table(fortify(shp)) 
shp_data[, id:=as.numeric(id)+1]# NEED TO VERIFY THIS WITH GUILLERMO 


#----------------------
#For each DPS, find it's center. 
departments = unique(shp_data$id)
departments = departments[!is.na(departments)]
all_centers = data.table()
for (department in departments){
  centers = shp_data[id==department, .(long, lat)]
  center = as.data.table(centroid(centers))
  center[, id:=department]
  all_centers = rbind(all_centers, center)
}

# Generate a labels dataset
labels = unique(shp_data[, .(id)])
labels[, label:= as.character(id)]
labels = merge(labels, all_centers, by=c('id'))
#----------------------

#For helping to make sure IDs are mapping correctly. 
id_labeled_map = ggplot(shp_data, aes(x=long, y=lat, group=group)) + 
  geom_polygon(color="black", fill="gray89") + 
  theme_void() + 
  geom_label_repel(data = labels, aes(label = label, x = lon, y = lat, group = label), inherit.aes=FALSE, size=3)

#Create mapping data set. 
setnames(urFits1, 'department', 'id')
urFits1 = urFits1[rhs!='date' & op=="~"]
map_data1 = merge(urFits1, shp_data, by='id', all=T, allow.cartesian=T)

#Create a general function. 
# Parameters: 
# Data - what data file you'd like to use. Data should be merged with shapefile data before this step. 
# lhsVar - which lhs var to isolate to. 
# rhsVar - which rhs var to isolate to. 
# title, subtitle, and caption - all arguments to be passed to "labs" for graph
# colScaleMin - starting point of the color scale. 
# colScaleMax - ending point of the color scale. 
shape_plot = function(data, lhsVar, rhsVar, title="", subtitle="", caption="", colScaleMin=-3, colScaleMax=3.5){
  data = map_data1[lhs==lhsVar & rhs==rhsVar]
  if (any(data$est.std<colScaleMin, na.rm=T) | any(data$est.std>colScaleMax, na.rm=T)) stop("colScaleMin and colScaleMax will drop some values in data - expand your parameters.")
  p = ggplot(data = data, aes(x = long, y = lat, group = group, fill=est.std)) +
    geom_polygon(colour="black") + 
    theme_void(base_size=16) + 
    scale_fill_viridis(direction=-1, breaks=seq(colScaleMin, colScaleMax, by=0.5), labels=as.character(seq(colScaleMin, colScaleMax, by=0.5)), limits=c(colScaleMin, colScaleMax)) +  
    labs(title=title, subtitle=subtitle, caption=caption, fill="Estimate")
  return(p)
} 

#Create some plots with this function. 
#Cases Notified ~ Additional cases via ACF 
p1 = shape_plot(map_data1, rhsVar="Additional_Cases_Detected_via_ACF_out", lhsVar="Cases_Notified_out_cumulative", 
               title="Correlation between additional cases detected via ACF and cases notified", colScaleMin=-1, colScaleMax=1)

p2 = shape_plot(map_data1, rhsVar="gf_tbhiv_cumulative", lhsVar="TB_Patients_Tested_for_HIV_act_cumulative", 
                title="Correlation between GF TB/HIV expenditure and TB patients tested for HIV", colScaleMin=-0.5, colScaleMax=2)

p3 = shape_plot(map_data1, rhsVar="ghe_tb_cumulative", lhsVar="TB_Patients_Tested_for_HIV_act_cumulative", 
                title="Correlation between GHE TB expenditure and TB patients tested for HIV", colScaleMin=-0.5, colScaleMax=2)

p4 = shape_plot(map_data1, rhsVar="gf_mdrtb_cumulative", lhsVar="Number_of_Cases_Screened_for_MDR_act_cumulative", 
                title="Correlation between GF MDR-TB expenditure and GeneXpert testing", colScaleMin=-0.5, colScaleMax=2)

p5 = shape_plot(map_data1, rhsVar="ghe_tb_cumulative", lhsVar="Number_of_Cases_Screened_for_MDR_act_cumulative", 
                title="Correlation between GHE TB expenditure and GeneXpert testing", colScaleMin=-0.5, colScaleMax=2)

p6 = shape_plot(map_data1, rhsVar="gf_tb_cumulative", lhsVar="Cases_Notified_in_Prisons_out", 
                title="Correlation between GHE TB expenditure and cases notified in prisons", colScaleMin=-0.5)


# Save all of these plots 
ggsave(paste0(save_loc, "acf_subnational.png"), p1, height=10, width=10)
ggsave(paste0(save_loc, "gf_tbhiv_testing_subnational.png"), p2, height=10, width=10)
ggsave(paste0(save_loc, "ghe_tbhiv_testing_subnational.png"), p3, height=10, width=10)
ggsave(paste0(save_loc, "gf_genexpert_testing_subnational.png"), p4, height=10, width=10)
ggsave(paste0(save_loc, "ghe_genexpert_testing_subnational.png"), p5, height=10, width=10)
ggsave(paste0(save_loc, "gf_prison_case_notifications_subnational.png"), p6, height=10, width=10)



#-------------------------------------------------------------
# RUN SIMPLE LINEAR REGRESSIONS BETWEEN 
# SECOND LINE DRUGS, GENEXPERT TESTING, AND GF SPENDING ON THESE 
# MODULES SPECIFICALLY FOR EFFICIENCY ANALYSIS 
#--------------------------------------------------------------

expenditures = readRDS(expendituresFile)

#Create date variable (collapsed to semester level)
expenditures[, year:=year(start_date)]
expenditures[, quarter:=quarter(start_date)]
expenditures$semester = NULL
expenditures[quarter%in%c(1, 2), semester:=0.0]
expenditures[quarter%in%c(3, 4), semester:=0.5]
expenditures[, date:=year+semester]

#Collapse, and fix names
expenditures = expenditures[loc_name=='gtm', .(expenditure=sum(expenditure)), by=c("date", "gf_module", "gf_intervention", "code", "disease", "grant_disease")] #Will subset by disease in code section below. 
setnames(expenditures, old=c("gf_module","gf_intervention"), new=c("module", "intervention"))


#Run data transformations on this data - backcast if needed, cumulatively sum, redistribute to 











