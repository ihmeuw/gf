#--------------------------------------------------
# AUTHOR: Francisco RC, Emily Linebarger 
# PURPOSE: Make specific graphs for Guatemala model 
# deliverables 
# DATE: August 2019 
#--------------------------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/sen/set_up_r.r')

#File paths 
save_loc = "J:/Project/Evaluation/GF/impact_evaluation/sen/visualizations/maps/"

# load model results
load(outputFile5a)
load(outputFile4a)

#-------------------------------------------------
# seperate out each data set into regions


data[region=='DAKAR', id:=1]  
data[region=='DIOURBEL', id:=2] 
data[region=='FATICK', id:=3] 
data[region=='KAFFRINE', id:=4] 
data[region=='KAOLACK', id:=5] 
data[region=='KEDOUGOU', id:=6] 
data[region=='KOLDA', id:=7] 
data[region=='LOUGA', id:=8]
data[region=='MATAM', id:=9] 
data[region=='SEDHIOU', id:=11] 
data[region=='ST-LOUIS', id:=10]
data[region=='TAMBACOUNDA', id:=12]
data[region=='THIES', id:=13]
data[region=='ZIGUINCHOR', id:=14]

urFits[region=='DAKAR', id:=1]  
urFits[region=='DIOURBEL', id:=2] 
urFits[region=='FATICK', id:=3] 
urFits[region=='KAFFRINE', id:=4] 
urFits[region=='KAOLACK', id:=5] 
urFits[region=='KEDOUGOU', id:=6] 
urFits[region=='KOLDA', id:=7] 
urFits[region=='LOUGA', id:=8]
urFits[region=='MATAM', id:=9] 
urFits[region=='SEDHIOU', id:=11] 
urFits[region=='ST-LOUIS', id:=10]
urFits[region=='TAMBACOUNDA', id:=12]
urFits[region=='THIES', id:=13]
urFits[region=='ZIGUINCHOR', id:=14]

#-------------------------------------------------

data1=copy(data)
urFits1 = copy(urFits)


# load nodeTable for graphing
nodeTable1 = fread(nodeTableFile1)

# ensure there are no extra variables introducted from nodeTable
nodeTable1 = nodeTable1[variable %in% names(data1)]

# compute averages (approximation of standard error, would be better as Monte Carlo simulation)
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
urFits1[, se_ratio.std:=se.std/est.std]
urFits1[, se_ratio:=se/est]
urFit1 = urFits1[, lapply(.SD, mean, na.rm=TRUE), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit1[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit1[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
# -----------------------------------------------


# load up area shape file, and create basic plot. 
library(rgdal)
library(geosphere)
shp <- readOGR("J:/Project/Evaluation/GF/mapping/sen/shapefiles/gadm36_SEN_1.shp")
shp_data = data.table(fortify(shp)) 
shp_data[, id:=as.numeric(id)+1]


#----------------------
#For each DPS, find it's center. 
regions = unique(shp_data$id)
regions = regions[!is.na(regions)]
all_centers = data.table()
for (region in regions){
  centers = shp_data[id==region, .(long, lat)]
  center = as.data.table(centroid(centers))
  center[, id:=region]
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
urFits1 = urFits1[rhs!='date' & op=="~"]
map_data1 = merge(urFits1, shp_data, by='id', all=T, allow.cartesian=T)
#data1_merge = data1[date>=2014, .(com_vad_touss=sum(com_vad_touss)), by=c('id')]
#data1_merge[com_vad_touss==0, com_vad_touss:=NA] #In these cases, we know there wasn't ACF in these departments and this change is a remnant of the model cleaning code. 
#map_data2 = merge(data1_merge, shp_data, by='id', all=T, allow.cartesian=T)

#Create a general function. 
# Parameters: 
# Data - what data file you'd like to use. Data should be merged with shapefile data before this step. 
# lhsVar - which lhs var to isolate to. 
# rhsVar - which rhs var to isolate to. 
# title, subtitle, and caption - all arguments to be passed to "labs" for graph
# colScaleMin - starting point of the color scale. 
# colScaleMax - ending point of the color scale. 
shape_plot = function(data, lhsVar, rhsVar, title="", subtitle="", caption="", colScaleMin=-3, colScaleMax=3.5, colScaleBreaks=0.5){
  data = map_data1[lhs==lhsVar & rhs==rhsVar]
  if (any(data$est<colScaleMin, na.rm=T) | any(data$est>colScaleMax, na.rm=T)) stop("colScaleMin and colScaleMax will drop some values in data - expand your parameters.")
  p = ggplot(data = data, aes(x = long, y = lat, group = group, fill=est)) +
    geom_polygon(colour="black") +
    theme_void(base_size=18) +
    scale_fill_viridis(direction=-1, breaks=seq(colScaleMin, colScaleMax, by=colScaleBreaks), labels=as.character(seq(colScaleMin, colScaleMax, by=colScaleBreaks)), limits=c(colScaleMin, colScaleMax)) +
    labs(title=title, subtitle=subtitle, caption=caption, fill="Estimate")
  return(p)
}

#Create some plots with this function.
#Cases Notified ~ Additional cases via ACF
p1 = shape_plot(map_data1, rhsVar="com_vad_touss_cumulative", lhsVar="lead_gueris_taux",
                title="Correlation between Home Visits and Treatment Success Rate", colScaleMin=-0.028, colScaleMax=0.0128, colScaleBreaks = 0.01)

# summary(map_data1[lhs=="lead_gueris_taux" & rhs=="com_vad_touss_cumulative"])

p2 = shape_plot(map_data1, rhsVar="com_enf_ref_cumulative", lhsVar="lead_tpm_chimio_enf_cumulative",
                title="Correlation between Child Contacts referred and Preventive Medicine for Children", colScaleMin=-200, colScaleMax=400, colScaleBreaks = 200)

p3 = shape_plot(map_data1, rhsVar="gf_tb_cumulative", lhsVar="Cases_Notified_in_Prisons_out_cumulative",
                title="Correlation between GF TB expenditure and cases notified in prisons", colScaleMin=-0.5, colScaleMax=2)

p4 = shape_plot(map_data1, lhsVar="Treatment_Success_Rate_imp", rhsVar="Cases_Started_on_Treatment_out_cumulative", 
                title="Estimated correlation between cases started on treatment\nand treatment success rate", colScaleMin = 0, colScaleMax=0.002, colScaleBreaks=0.001)

p5 = shape_plot(map_data1, lhsVar="Case_Notification_Rate_imp_log", rhsVar="Cases_Notified_out_cumulative", 
                title="Estimated correlation between cases notified and case notification rate", colScaleMin = 0, colScaleMax=0.008, colScaleBreaks=0.002)

p6 = shape_plot(map_data1, lhsVar="Children_in_Contact_with_TB_Started_IPT_out_cumulative", rhsVar="Children_less5_referred_out_cumulative", 
                title="Estimated correlation between children referred for TB screening\nand children started IPT", colScaleMin=0, colScaleMax=4.5, colScaleBreaks=1)

p7 = shape_plot(map_data1, lhsVar="Proportion_of_MDR_Cases_Treated_out_log", rhsVar = "MDR_Cases_Started_Treatment_out_cumulative", 
                colScaleMin=0, colScaleMax=0.03, colScaleBreaks=0.01, 
                title="Estimated correlation between MDR cases started on treatment\nand proportion of MDR cases treated")

p8 = shape_plot(map_data1, lhsVar="Proportion_of_HIV_TB_Cases_Treated_out_log", rhsVar="HIV_TB_Cases_Notified_out_cumulative", 
                colScaleMin=-0.04, colScaleMax=0.2, colScaleBreaks=0.05, 
                title="Estimated correlation between HIV/TB cases notified\nand proportion of HIV/TB cases treated")

p9 = shape_plot(map_data1, lhsVar="HIV_TB_Treatment_Success_Rate_imp", rhsVar="HIV_TB_Cases_Notified_out_cumulative", 
                colScaleMin=-0.005, colScaleMax=0.05, colScaleBreaks=0.01, 
                title="Estimated correlation between HIV/TB cases notified\nand HIV/TB treatment success rate")

p10 = shape_plot(map_data1, lhsVar="Proportion_of_Cases_in_Prisons_Treated_out_log", rhsVar="Cases_Notified_in_Prisons_out_cumulative", 
                 colScaleMin=-0.01, colScaleMax=0.2, colScaleBreaks=0.05, 
                 title="Estimated correlation between cases notified in prisons\nand proportion of cases in prisons treated")

pdf(paste0(save_loc, "outputs_outcomes_graphs.pdf"), height=10, width=10)
p4
p5
p6
p7
p8
p9
p10
dev.off() 

# Save all of these plots
# ggsave(paste0(save_loc, "acf_subnational.png"), p1, height=10, width=10)
# ggsave(paste0(save_loc, "gf_tbhiv_testing_subnational.png"), p2, height=10, width=10)
# ggsave(paste0(save_loc, "ghe_tbhiv_testing_subnational.png"), p3, height=10, width=10)
ggsave(paste0(save_loc, "gf_genexpert_testing_subnational.png"), p1, height=10, width=10)
ggsave(paste0(save_loc, "ghe_genexpert_testing_subnational.png"), p2, height=10, width=10)
ggsave(paste0(save_loc, "gf_prison_case_notifications_subnational.png"), p3, height=10, width=10)

ggsave(paste0(save_loc, "cases_started_tx_tx_success_rate.png"), p4, height=10, width=10)
ggsave(paste0(save_loc, "hivtb_cases_started_tx_tx_success_rate.png"), p9, height=10, width=10)


# Create a few plots using just the raw data - specifically active case finding. 

base_data = ggplot(map_data2, aes(x = long, y = lat, group = group, fill=Additional_Cases_Detected_via_ACF_out)) +
  geom_polygon(colour="black") + 
  theme_void(base_size=16) + 
  scale_fill_viridis(direction=-1) +  
  labs(title="Prioritized departments for Active Case Finding", fill="Add'l Cases Detected")
ggsave(paste0(save_loc, "acf_subnational.png"), base_data, height=10, width=10)

