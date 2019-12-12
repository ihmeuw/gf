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
  if (any(data$est.std<colScaleMin, na.rm=T) | any(data$est.std>colScaleMax, na.rm=T)) stop("colScaleMin and colScaleMax will drop some values in data - expand your parameters.")
  p = ggplot(data = data, aes(x = long, y = lat, group = group, fill=est.std)) +
    geom_polygon(colour="black") +
    theme_void(base_size=18) +
    scale_fill_viridis(direction=-1, breaks=seq(colScaleMin, colScaleMax, by=colScaleBreaks), labels=as.character(seq(colScaleMin, colScaleMax, by=colScaleBreaks)), limits=c(colScaleMin, colScaleMax)) +
    labs(title=title, subtitle=subtitle, caption=caption, fill="Estimate")
  return(p)
}

#Create some plots with this function.
p1 = shape_plot(map_data1, rhsVar="com_vad_touss_cumulative", lhsVar="lead_gueris_taux",
                title="Std. coeff. between No. home visits \n and TB Tx Success Rate", colScaleMin=-400, colScaleMax=100, colScaleBreaks = 100)

# summary(map_data1[rhs=="com_vad_touss_cumulative" & lhs=="lead_gueris_taux"]$est.std)
p2 = shape_plot(map_data1, rhsVar="tb_vih_arv_cumulative", lhsVar="lead_gueris_taux",
                title="Std. coeff. between No. TB-HIV on ARV \n and TB Tx Success Rate", colScaleMin=-15, colScaleMax=6, colScaleBreaks = 4)

p3 = shape_plot(map_data1, rhsVar="tb_tfc_cumulative", lhsVar="lead_gueris_taux",
                title="Std. coeff. between No. Tx First-line \n and TB Tx Success Rate", colScaleMin=-30, colScaleMax=30, colScaleBreaks = 12)

p4 = shape_plot(map_data1, rhsVar="com_enf_ref_cumulative", lhsVar="lead_tpm_chimio_enf_cumulative",
                title="Std. coeff. between No. child ref \n and No. child on IPT", colScaleMin=.9, colScaleMax=1, colScaleBreaks = 0.02)

p5 = shape_plot(map_data1, rhsVar="dx_count_cumulative", lhsVar="mdr_success_cumulative",
                title="Std. coeff. between No. MDR-TB cases dx \n and No. MDR-TB Cured", colScaleMin=0, colScaleMax=1, colScaleBreaks = 0.2)

p6 = shape_plot(map_data1, rhsVar="patients_prop_genexpert_cumulative",lhsVar="dx_count_cumulative", 
                title="Std. coeff. between No. GeneXpert tests \n and No. MDR-TB cases dx", colScaleMin = -1, colScaleMax=1, colScaleBreaks=0.4)
 
p7 = shape_plot(map_data1,rhsVar="tot_genexpert_cumulative",  lhsVar="dx_count_cumulative", 
                 title="Std. coeff. between TB dx by GeneXpert \n and No. MDR-TB cases dx", colScaleMin = -0.5, colScaleMax=2, colScaleBreaks=0.5)
 
p8 = shape_plot(map_data1, rhsVar="com_vad_touss_cumulative", lhsVar="com_enf_ref_cumulative",  
               title="Std. coeff. between No. home visits \n and No. child ref", colScaleMin=.9, colScaleMax=1, colScaleBreaks=0.02)
 
p9 = shape_plot(map_data1, rhsVar = "com_vad_touss_cumulative", lhsVar="com_nom_touss_cumulative",  
                 colScaleMin=.98, colScaleMax=1, colScaleBreaks=0.004, 
                 title="Std. coeff. between No. home visits \n and People with cough referred")
 
p10 = shape_plot(map_data1, rhsVar="com_vad_touss_cumulative", lhsVar="tb_cas_id_cumulative",  
                 colScaleMin=-23, colScaleMax=20, colScaleBreaks=8, 
                 title="Std. coeff. between No. home visits \n and TB Cases ID in other ways")

p11 = shape_plot(map_data1, rhsVar="patients_prop_genexpert_cumulative", lhsVar="tot_genexpert_cumulative",  
                 colScaleMin=-.5, colScaleMax=.3, colScaleBreaks=0.16, 
                 title="Std. coeff. between No. GeneXpert tests \n and No. TB dx by GeneXpert")

p12 = shape_plot(map_data1, rhsVar="com_radio_cumulative", lhsVar="tb_cas_id_cumulative",  
                 colScaleMin=-1.3, colScaleMax=9, colScaleBreaks=2, 
                 title="Std. coeff. between No. radio bcasts \n and No. TB dx in oth ways")

p13 = shape_plot(map_data1, rhsVar="com_mobsoc_cumulative", lhsVar="tb_cas_id_cumulative",  
                 colScaleMin=-29, colScaleMax=21, colScaleBreaks=8, 
                 title="Std. coeff. between No. social mobil \n and No. TB dx by GeneXpert")
 

#
pdf(paste0(save_loc, "outputs_outcomes_graphs_standardized.pdf"), height=10, width=12)
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)
print(p9)
print(p10)
print(p11)
print(p12)
print(p13)
dev.off() 

# Save all of these plots
# ggsave(paste0(save_loc, "acf_subnational.png"), p1, height=10, width=10)
# ggsave(paste0(save_loc, "gf_tbhiv_testing_subnational.png"), p2, height=10, width=10)
# ggsave(paste0(save_loc, "ghe_tbhiv_testing_subnational.png"), p3, height=10, width=10)
#ggsave(paste0(save_loc, "gf_genexpert_testing_subnational.png"), p1, height=10, width=10)
#ggsave(paste0(save_loc, "ghe_genexpert_testing_subnational.png"), p2, height=10, width=10)
#ggsave(paste0(save_loc, "gf_prison_case_notifications_subnational.png"), p3, height=10, width=10)

#ggsave(paste0(save_loc, "cases_started_tx_tx_success_rate.png"), p4, height=10, width=10)
#ggsave(paste0(save_loc, "hivtb_cases_started_tx_tx_success_rate.png"), p9, height=10, width=10)


# Create a few plots using just the raw data - specifically active case finding. 

#base_data = ggplot(map_data2, aes(x = long, y = lat, group = group, fill=Additional_Cases_Detected_via_ACF_out)) +
#  geom_polygon(colour="black") + 
#  theme_void(base_size=16) + 
#  scale_fill_viridis(direction=-1) +  
#  labs(title="Prioritized departments for Active Case Finding", fill="Add'l Cases Detected")
#ggsave(paste0(save_loc, "acf_subnational.png"), base_data, height=10, width=10)

