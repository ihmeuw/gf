#-----------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Review test positivity rates for pregnant women in PMTCT data 
# DATE: August 26, 2019 
#-----------------------------------------------------------------------

rm(list=ls())

# Install packages
library(data.table)
library(raster)
library(ggplot2)
library(ggrepel)
library(geosphere)
library(gridExtra)
library(knitr)
library(tibble)
library(dplyr)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(rmarkdown)
library(maptools)
repo_root = "C:/Users/elineb/Documents/gf/" #Set to the root of your repository 
setwd(repo_root)
source('./core/standardizeDPSNames.R')

#Set up directories 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/pnls_final/') #Home directory
saveDir = paste0(j, 'Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pnls/')
codeDir = paste0(repo_root, "outcome_measurement/all/cod/dhis/dhis_analysis/pnls_pmtct/")

source(paste0(codeDir, "pmtct_set_up_data.r"))

#--------------------------------------------------
# ANALYSIS-SPECIFIC DATA CHANGES 
#--------------------------------------------------

#Only keep 'Tested' and "Total plw received in facility', and only PLW 
dt = dt[element_id%in%c("DXz4Zxd4fKq" , "uXM8GDCJbGK") & subpop=='plw']

#Round numbers after quantile regression 
dt = dt[, value:=round(value)]

#Only keep 2017 and 2018 
dt = dt[year%in%c(2017, 2018)]

#Check to drop dates where the total # of women tested is greater than the total # of women received in the facility. 
dt_wide = dcast(dt, dps+health_zone+org_unit_id+date~element_eng, value.var='value', fun.aggregate = sum)
setnames(dt_wide, c('Tested', 'Total received in the facility'), c('tested', 'total'))
dt_wide[tested>total, error:=TRUE]
print(paste0((nrow(dt_wide[error==T])/nrow(dt_wide))*100, "% of data reports a higher # of women tested than women received in the facility by DPS, health zone, facility, and date. These values will be dropped from data."))

dt_wide[is.na(tested) | is.na(total), error2:=TRUE]
print(paste0((nrow(dt_wide[error2==T])/nrow(dt_wide))*100, "% of data reports either 'tested' or 'total' as NA. These values will be dropped from data."))

drop = unique(dt_wide[error==T | error2==T, .(dps, health_zone, org_unit_id, date)])
dt = dt[!(dps%in%drop$dps & health_zone%in%drop$health_zone & org_unit_id%in%drop$org_unit_id & date%in%drop$date)]

#Only keep Global Fund-funded areas 
gf_only = dt[, .(value=sum(value, na.rm=T)), by=c('date', 'year', 'id', 'age', 'subpop', 'funder', 'dps', 'element_eng', 'element_id')]
gf_only[funder=="PEPFAR", value:=NA]

#Easy visual to grab element id. 
unique(gf_only[, .(element_id, element_eng)])

#--------------------------------------------------
# GENERATE GRAPHS 
#--------------------------------------------------
# Graphs to make: 
#   * Only use GF data to generate these! 
#   1. 3 diagnostic plots of key variables, summed to DPS level, with DPS as color. Spaghetti plot. 
#   2. 3 line plots of key variables - tested, total, and ratio, summed to national level. 
#   3. 3 maps, 2018 only, of these 3 variables, making PEPFAR numbers NA 
# 
# 
# 

#Add a general reporting completeness plot? 
reporting = dt[, .(date, org_unit_id)]
reporting[, total_facs:=length(unique(org_unit_id))]
reporting[, facs_by_month:=length(unique(org_unit_id)), by='date']

reporting = unique(reporting[, .(date, total_facs, facs_by_month)])
reporting[, pct:=(facs_by_month/total_facs)*100]

completeness = ggplot(reporting, aes(x=date, y=pct)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(title="Reporting completeness", x="Date", y="Percentage of facilities reporting")

#Diagnostic plots 
# Tested 
dt1 = gf_only[element_id=="DXz4Zxd4fKq", .(value=sum(value, na.rm=T)), by=c('date', 'dps')]
p1 = ggplot(dt1, aes(x=date, y=value, color=dps)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(title="Diagnostic plot for 'Tested' by DPS", x="Date", y="Tested", caption="Note: Represents only Global Fund-supported health zones")

#Total 
dt2 = gf_only[element_id=="uXM8GDCJbGK", .(value=sum(value, na.rm=T)), by=c('date', 'dps')]
p2 = ggplot(dt2, aes(x=date, y=value, color=dps)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  labs(title="Diagnostic plot for 'Total received in the facility' by DPS", x="Date", y="Total received in facility", caption="Note: Represents only Global Fund-supported health zones")

#pct 
dt3 = gf_only[, .(value=sum(value, na.rm=T)), by=c('date', 'dps', 'element_eng')]
dt3 = dcast(dt3, date+dps~element_eng)
setnames(dt3, c('Tested', 'Total received in the facility'), c('tested', 'total'))

dt3[tested>total, error:=TRUE]
stopifnot(nrow(dt3[error==TRUE])==0) #You should have resolved all of these cases above, but just make sure before running. 

dt3[, pct:=((tested/total)*100)]
p3 = ggplot(dt3, aes(x=date, y=pct, color=dps)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  scale_y_continuous(breaks=seq(0, 100, by=10), labels=as.character(seq(0, 100, by=10)), limits=c(0, 100)) + 
  labs(title="Diagnostic plot for percentage of pregnant and lactating women\ntested for HIV by DPS", x="Date", y="Percentage (%)", caption="Note: Represents only Global Fund-supported health zones")

#National time trends 
# Tested 
dt4 = gf_only[element_id=="DXz4Zxd4fKq", .(value=sum(value, na.rm=T)), by=c('date')]
n_dt4 = dt4[, sum(value, na.rm=T)]
p4 = ggplot(dt4, aes(x=date, y=value)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(title="National time trend of pregnant and lactating women tested", x="Date", y="Tested",
       subtitle=paste0("n = ", n_dt4),
       caption="Note: Represents only Global Fund-supported health zones")

#Total 
dt5 = gf_only[element_id=="uXM8GDCJbGK", .(value=sum(value, na.rm=T)), by=c('date')]
n_dt5 = dt5[ ,sum(value, na.rm=T)] # total ladies 
p5 = ggplot(dt5, aes(x=date, y=value)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(title="National time trend of pregnant and lactating women received in a facility", 
       subtitle = paste0("n = ", n_dt5),
       x="Date", y="Received", caption="Note: Represents only Global Fund-supported health zones")

#pct 
dt6 = gf_only[, .(value=sum(value, na.rm=T)), by=c('date', 'element_eng')]
dt6 = dcast(dt6, date~element_eng)
setnames(dt6, c('Tested', 'Total received in the facility'), c('tested', 'total'))
n_dt6 = dt6[, sum(total)]

dt6[, pct:=((tested/total)*100)]
p6 = ggplot(dt6, aes(x=date, y=pct)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(title="Percentage of pregnant and lactating women tested for HIV", x="Date", y="Percent (%)",
       subtitle = paste0("n = ", n_dt6),
       caption="Note: Represents only Global Fund-supported health zones")

p7 = ggplot(dt6, aes(x=date, y=pct)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(text = element_text(size=18)) +
  scale_y_continuous(breaks=seq(0, 100, by=10), labels=as.character(seq(0, 100, by=10)), limits=c(0, 100)) + 
  labs(title="Percentage of pregnant and lactating women\n tested for HIV in GF districts", x="Date", y="Percent (%)",
       subtitle = paste0("n = ", n_dt6),
       caption="*Y-axis scaled from 0:100\nNote: Represents only Global Fund-supported health zones")

#Maps 
# Tested 
gf_only_map = gf_only[year==2018, .(value=sum(value, na.rm=T)), by=c('id', 'dps', 'element_id', 'element_eng')] #Only do 2018 for now! 
gf_only_map = merge(gf_only_map, coord, by=c('id'), all=T, allow.cartesian=T)

#Make PEPFAR districts gray - turn to NA
gf_only_map[dps%in%c('haut-katanga', "lualaba"), value:=NA]
n_gf_only = gf_only_map[, sum(value, na.rm=T)]

#Tested 
p8 = ggplot(gf_only_map[element_id=="DXz4Zxd4fKq"], aes(x = long, y = lat, group = group, fill=value)) +
  geom_polygon(colour="black") +
  theme_void() +
  theme(text = element_text(size=18)) +
  scale_fill_gradient2(low = "white", high = "darkorchid3") + 
  coord_fixed(ratio=1) + 
  labs(title="Pregnant and lactating women tested for HIV\nby DPS in 2018", fill="Tested", 
       subtitle=paste0("n = ", n_gf_only), 
       caption="Note: Represents only Global Fund-supported health zones") 

p9 = ggplot(gf_only_map[element_id=="uXM8GDCJbGK"], aes(x = long, y = lat, group = group, fill=value)) +
  geom_polygon(colour="black") +
  theme_void() +
  theme(text = element_text(size=18)) +
  scale_fill_gradient2(low = "white", high = "dodgerblue3") + 
  coord_fixed(ratio=1) + 
  labs(title="Pregnant and lactating women received\nby DPS in 2018", fill="Received", 
       subtitle=paste0("n = ", n_gf_only), 
       caption="Note: Represents only Global Fund-supported health zones") 

gf_map_wide = gf_only[year==2018, .(value=sum(value, na.rm=T)), by=c('id', 'element_eng', 'dps')]
gf_map_wide = dcast(gf_map_wide, id+dps~element_eng, value.var='value')
setnames(gf_map_wide, c('Tested', 'Total received in the facility'), c('tested', 'total'))
n_gf_wide = gf_map_wide[, sum(total)]
gf_map_wide[, pct:=((tested/total)*100)]
gf_map_wide = merge(gf_map_wide, coord, by='id', all=T, allow.cartesian=T)

#make the first letter of each DPS capital, as well as the second word when split by "-".
gf_map_wide[, dps:=paste0(toupper(substr(dps, 1, 1)), substr(dps, 2, length(dps)))]
gf_map_wide[, dps1:=tstrsplit(dps, "-", keep=1)]
gf_map_wide[, dps2:=tstrsplit(dps, "-", keep=2)]
gf_map_wide[!is.na(dps2), dps2:=paste0(toupper(substr(dps2, 1, 1)), substr(dps2, 2, length(dps2)))]
gf_map_wide[!is.na(dps2), dps:=paste0(dps1, " ", dps2)]

#Add on centroid labels
#---------------------------------------------
districts = unique(gf_map_wide$dps)
districts = districts[!is.na(districts)]
all_centers = data.table()
for (district in districts){
  centers = gf_map_wide[dps==district, .(long, lat)]
  center = as.data.table(centroid(centers))
  center[, dps:=district]
  all_centers = rbind(all_centers, center)
}

# Generate a labels dataset
labels = unique(gf_map_wide[, .(id, dps, pct)])
labels[, label:= paste0(dps, ": ", format(round(pct, digits=1), nsmall=1), "%")]
labels = merge(labels, all_centers, by=c('dps'))
#-----------------------------------------------

#Remove Lualaba and Haut-katanga from labels dataset because they aren't funded by the Global Fund. 
labels = labels[!(dps=="Haut Katanga" | dps=="Lualaba")]

p10 = ggplot(gf_map_wide, aes(x = long, y = lat, group = group, fill=pct)) +
  geom_polygon(colour="black") +
  theme_void() +
  scale_fill_gradient2(low = "white", high = "firebrick3") + 
  theme(text = element_text(size=18)) +
  coord_fixed(ratio=1) + 
  labs(title="Percentage of pregnant and lactating women tested for HIV in 2018", fill="Percent (%)", 
       subtitle=paste0("n = ", n_gf_wide), 
       caption="Note: Represents only Global Fund-supported health zones") +
  geom_label_repel(data = labels, aes(label = label, x = lon, y = lat, group = label), inherit.aes=FALSE, size=3)

# Save this graph for DRC final report 
ggsave("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pmtct/plw_2018_final_report.png", p10, height=8, width=11)

# One breakdown by age grouping - testing. 
dt11 = gf_only[element_id=="DXz4Zxd4fKq", .(value=sum(value, na.rm=T)), by=c('date', 'age')]
n_dt11 = dt11[, sum(value, na.rm=T)]

p11 = ggplot(dt11, aes(x=date, y=value, color=age)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(title="HIV Testing among pregnant and lactating women by age", x="Date", y="Tested", 
       subtitle=paste0("n = ", n_dt11), 
       color="Age group", caption="Note: Represents only Global Fund-supported health zones")

# National time trend of PEPFAR ratio
pepfar_only = dt[funder=="PEPFAR", .(value=sum(value, na.rm=T)), by=c('date', 'element_eng', 'id')]
pepfar_wide = dcast(pepfar_only, date+id~element_eng, value.var='value')
setnames(pepfar_wide, c('Tested', 'Total received in the facility'), c('tested', 'total'))
pepfar_wide[, pct:=((tested/total)*100)]

pepfar_wide2 = dt[funder=="PEPFAR", .(value=sum(value, na.rm=T)), by=c('date', 'element_eng')]
pepfar_wide2 = dcast(pepfar_wide2, date~element_eng, value.var='value')
setnames(pepfar_wide2, c('Tested', 'Total received in the facility'), c('tested', 'total'))
n_pep_total = pepfar_wide2[, sum(total)]
pepfar_wide2[, pct:=((tested/total)*100)]

p12 = ggplot(pepfar_wide2, aes(x=date, y=pct)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(title="Percentage of pregnant and lactating women tested for HIV\nin PEPFAR districts", x="Date",
       subtitle=paste0("n = ", n_pep_total), 
       y="Percent (%)", caption="Note: Represents only PEPFAR-supported health zones")


p13 = ggplot(pepfar_wide2, aes(x=date, y=pct)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(text = element_text(size=18)) +
  scale_y_continuous(breaks=seq(0, 100, by=10), labels=as.character(seq(0, 100, by=10)), limits=c(0, 100)) + 
  labs(title="Percentage of pregnant and lactating women\ntested for HIV in PEPFAR districts", x="Date",
       y="Percent (%)", subtitle=paste0("n = ", n_pep_total), 
       caption="*Y-axis scaled from 0:100\n*Note: Represents only PEPFAR-supported health zones")

# One testing counts, colored by level. 
dt14 = dt[element_id=="DXz4Zxd4fKq" & year%in%c(2017, 2018), .(value=sum(value, na.rm=T)), by=c('date', 'level', 'funder')]
dt14[funder=="PEPFAR", value:=NA]
dt14 = dt14[, .(value=sum(value, na.rm=T)), by=c('date', 'level')]
n_dt14 = dt14[, sum(value, na.rm=T)]
p14 = ggplot(dt14, aes(x=date, y=value, color=level)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(title="Pregnant and lactating women tested by facility level", x="Date", 
       y="Tested", color="Facility level", 
       subtitle=paste0("n = ", n_dt14),
       caption="Note: Represents only PEPFAR-supported health zones")


# One map comparing PEPFAR to GF ratio in 2018 
coord_funder = rbind(coord, coord)
coord_funder[, funder:=rep(c('PEPFAR', 'The Global Fund'), each=nrow(coord))]

dt15 = dt[year==2018, .(value=sum(value, na.rm=T)), by=c('funder', 'element_eng', 'id')]
dt15 = dcast(dt15, funder+id~element_eng, value.var='value')
setnames(dt15, c('Tested', 'Total received in the facility'), c('tested', 'total'))
n_dt15 = dt15[, sum(total)]
dt15[, pct:=((tested/total)*100)]

dt15 = merge(dt15, coord_funder, by=c('id', 'funder'), all=T, allow.cartesian=T)
p15 = ggplot(dt15, aes(x = long, y = lat, group = group, fill=pct)) +
  geom_polygon(colour="black") +
  theme_void() +
  scale_fill_gradient2(low = "white", high = "darkturquoise") + 
  theme(text = element_text(size=18)) +
  facet_wrap(~funder) + 
  coord_fixed(ratio=1) + 
  labs(title="Percentage of pregnant and lactating women tested for HIV\nin 2018 by funder", fill="Percent (%)", 
       subtitle=paste0("n = ", n_dt15)) 

# Run a few analyses on early/late reporters. 
early = dt[date < '2018-01-01', unique(org_unit_id)]

dt_early = dt[org_unit_id%in%early & date<"2018-01-01"]
dt_early[, report_time:='early']
dt_late = dt[!org_unit_id%in%early & date >="2018-01-01"]
dt_late[, report_time:='late']

report_timing = rbind(dt_early, dt_late)

# What type of facilities were reporting early vs. late? 
dt16 = report_timing[year==2017 | year==2018, .(num_facs=length(unique(org_unit_id))), by=c('report_time', 'date', 'level')]
dt16 = dt16[!is.na(level)]
dt16[, total_facs:=sum(num_facs), by=c('report_time', 'date')]
dt16[, pct:=(num_facs/total_facs)*100]
p16 = ggplot() + geom_bar(aes(x=date, y=pct, fill=level), data=dt16, stat="identity") + 
  scale_fill_viridis_d(direction=-1) + 
  labs(title="Facility composition by reporting pre- or post-January 2018", x="Date",  
       y="Percent (%)", fill="Facility type", 
       caption="*Data restricted to only early reporting facilities before Jan. 2018,\nand only late-reporting facilities after") + 
  theme_bw(base_size=18) 
  

# What was the pct of tests/total served by early vs. late? 
dt[org_unit_id%in%early, report_time:='early']
dt[!org_unit_id%in%early, report_time:='late']

dt17 = dt[, .(value=sum(value, na.rm=T)), by=c('date', 'report_time', 'element_eng')]
dt17 = dcast(dt17, date+report_time~element_eng, value.var='value')
setnames(dt17, c('Tested', 'Total received in the facility'), c('tested', 'total'))
dt17[, pct:=((tested/total)*100)]

p17 = ggplot(dt17, aes(x=date, y=pct, color=report_time)) + 
  geom_point() + 
  geom_line() + 
  theme_bw() + 
  theme(text = element_text(size=18)) +
  labs(title="Testing percentage, by early/late reporting facilities", x="Date", y="Percent (%)", 
       color="Report time", caption="*Early reporting defined as reporting prior to Jan. 2018") 

#--------------------------------------------------
# SAVE GRAPHS 
#--------------------------------------------------
pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pmtct/visuals.pdf", height=10, width=14)
completeness
p1
p2
p3
p4
p5
p6
p7
p8
p9
p10
p11 
p12  
p13
grid.arrange(p7, p13, nrow=1)
p14
p15
p16
p17
dev.off()

ggsave("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/outputs/pmtct/plw_tested_2018_labeled.png", p10, height=10, width=14)
