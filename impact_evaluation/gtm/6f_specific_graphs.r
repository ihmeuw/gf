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
load(outputFile4a)

#-------------------------------------------------
# Make sure that department IDs match - 
# IDs in raw data came from this key 
# https://www.mineduc.gob.gt/DIGEESP/documents/adecuacionesCurriculares/Documentos%20de%20Apoyo/Códigos%20Departamentos-Municipios-Idiomas.pdf
# EL 8/26/19 


data[department==1, id:=7] #Guatemala 
data[department==2, id:=5] # El Progreso
data[department==3, id:=16] # Sacatepequez
data[department==4, id:=3] # Chimaltenango
data[department==5, id:=6] # Escuintla
data[department==6, id:=18] # Santa Rosa
data[department==7, id:=19] # Solola
data[department==8, id:=21] # Totonicapan
data[department==9, id:=13] # Quetzaltenango
data[department==10, id:=20] # Suchitepequez
data[department==11, id:=15] # Retalhuleu
data[department==12, id:=17] # San Marcos
data[department==13, id:=8] # Huehuetenango
data[department==14, id:=14] # El Quiche
data[department==15, id:=2] # Baja Verapaz
data[department==16, id:=1] # Alta Verapaz
data[department==17, id:=12] # El Peten
data[department==18, id:=9] # Izabal
data[department==19, id:=22] # Zacapa
data[department==20, id:=4] # Chiquimula
data[department==21, id:=10] # Jalapa
data[department==22, id:=11] # Jutiapa


urFits[department==1, id:=7] #Guatemala 
urFits[department==2, id:=5] # El Progreso
urFits[department==3, id:=16] # Sacatepequez
urFits[department==4, id:=3] # Chimaltenango
urFits[department==5, id:=6] # Escuintla
urFits[department==6, id:=18] # Santa Rosa
urFits[department==7, id:=19] # Solola
urFits[department==8, id:=21] # Totonicapan
urFits[department==9, id:=13] # Quetzaltenango
urFits[department==10, id:=20] # Suchitepequez
urFits[department==11, id:=15] # Retalhuleu
urFits[department==12, id:=17] # San Marcos
urFits[department==13, id:=8] # Huehuetenango
urFits[department==14, id:=14] # El Quiche
urFits[department==15, id:=2] # Baja Verapaz
urFits[department==16, id:=1] # Alta Verapaz
urFits[department==17, id:=12] # El Peten
urFits[department==18, id:=9] # Izabal
urFits[department==19, id:=22] # Zacapa
urFits[department==20, id:=4] # Chiquimula
urFits[department==21, id:=10] # Jalapa
urFits[department==22, id:=11] # Jutiapa

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
urFits1 = urFits1[rhs!='date' & op=="~"]
map_data1 = merge(urFits1, shp_data, by='id', all=T, allow.cartesian=T)
data1_merge = data1[date>=2017, .(Additional_Cases_Detected_via_ACF_out=sum(Additional_Cases_Detected_via_ACF_out)), by=c('id')]
data1_merge[Additional_Cases_Detected_via_ACF_out==0, Additional_Cases_Detected_via_ACF_out:=NA] #In these cases, we know there wasn't ACF in these departments and this change is a remnant of the model cleaning code. 
map_data2 = merge(data1_merge, shp_data, by='id', all=T, allow.cartesian=T)

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
    theme_void(base_size=18) +
    scale_fill_viridis(direction=-1, breaks=seq(colScaleMin, colScaleMax, by=0.5), labels=as.character(seq(colScaleMin, colScaleMax, by=0.5)), limits=c(colScaleMin, colScaleMax)) +
    labs(title=title, subtitle=subtitle, caption=caption, fill="Estimate")
  return(p)
}

#Create some plots with this function.
#Cases Notified ~ Additional cases via ACF
p4 = shape_plot(map_data1, rhsVar="gf_mdrtb_cumulative", lhsVar="Number_of_Cases_Screened_for_MDR_act_cumulative",
                title="Correlation between GF MDR-TB expenditure and GeneXpert testing", colScaleMin=-0.5, colScaleMax=2)

p5 = shape_plot(map_data1, rhsVar="ghe_tb_cumulative", lhsVar="Number_of_Cases_Screened_for_MDR_act_cumulative",
                title="Correlation between GHE TB expenditure and GeneXpert testing", colScaleMin=-0.5, colScaleMax=2)

p6 = shape_plot(map_data1, rhsVar="gf_tb_cumulative", lhsVar="Cases_Notified_in_Prisons_out_cumulative",
                title="Correlation between GF TB expenditure and cases notified in prisons", colScaleMin=-0.5, colScaleMax=2)



# Save all of these plots
# ggsave(paste0(save_loc, "acf_subnational.png"), p1, height=10, width=10)
# ggsave(paste0(save_loc, "gf_tbhiv_testing_subnational.png"), p2, height=10, width=10)
# ggsave(paste0(save_loc, "ghe_tbhiv_testing_subnational.png"), p3, height=10, width=10)
ggsave(paste0(save_loc, "gf_genexpert_testing_subnational.png"), p4, height=10, width=10)
ggsave(paste0(save_loc, "ghe_genexpert_testing_subnational.png"), p5, height=10, width=10)
ggsave(paste0(save_loc, "gf_prison_case_notifications_subnational.png"), p6, height=10, width=10)




# Create a few plots using just the raw data - specifically active case finding. 

p7 = ggplot(map_data2, aes(x = long, y = lat, group = group, fill=Additional_Cases_Detected_via_ACF_out)) +
  geom_polygon(colour="black") + 
  theme_void(base_size=16) + 
  scale_fill_viridis(direction=-1) +  
  labs(title="Prioritized departments for Active Case Finding", fill="Add'l Cases Detected")
ggsave(paste0(save_loc, "acf_subnational.png"), p7, height=10, width=10)

