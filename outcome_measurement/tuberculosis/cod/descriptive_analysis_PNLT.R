# ----------------------------------------------
# Audrey Batzel
# 9/2?/2018
# First decriptive analysis of PNLT data
# ----------------------------------------------
## Set up R / install packages 
# ----------------------------------------------
rm(list=ls())
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(maptools)
library(RColorBrewer)
library(ggrepel)
library(zoo)
setwd('C:/local/gf/')
source('./core/standardizeDPSnames_function.R')
# ----------------------------------------------


# ----------------------------------------------
## set up the directories to read/output files: 
# ----------------------------------------------
# Directories
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j/')

output_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLT_data/')
data_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/")
shape_dir <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/gadm36_COD_shp/")

# input files
pnlt_data_dep <- "PNLT_prepped_data.csv"
pnlt_data_case_outcomes17 <- "PNLT_case_outcomes_2017.csv"
pnlt_data_case_outcomes18 <- "PNLT_case_outcomes_2018.csv"
pnlt_outcomes_18_tb_hiv <- "PNLT_case_outcomes_2018_TBHIV.csv"
pnlt_case_screening_17 <- "PNLT_case_screening_2017.csv"
dps_shapefile <-  "gadm36_COD_1.shp"

# output files
compare_dps_graphs <- "PNLT case outcomes - incident cases - by dps.pdf"
compare_dps_graphs_faceted <- "PNLT_case_outcomes_by_dps_(faceted).pdf"
ts_completed_treatment_natl <- "case_outcomes_data/TS - Completed course of treatment (incident cases) national.pdf"
map_case_outcomes_completed_treatment = "case_outcomes_data/Map - Completed course of treatment (incident cases).pdf"
map_comp_trt_with_GF_marked = "case_outcomes_data/Map - Completed course of treatment (incident cases) with GF province marked.pdf"
# ----------------------------------------------


# ----------------------------------------------
## load data / prep continued (move to prep code)
# ----------------------------------------------
drcShape <- shapefile(paste0(shape_dir, dps_shapefile)) # shapefile with all DPS (use a different one for health zones)

# ~~~~~~~~~~
dt17 <- read.csv(paste0(data_dir, pnlt_data_case_outcomes17), stringsAsFactors = FALSE)
dt17 <- as.data.table(dt17)

incid_cases_patterns <- c("NC", "RECH")
dt17[, incid_case := ifelse (grepl(paste(incid_cases_patterns, collapse="|"), dt17$TB_type), "yes", "no")]

  # UPDATE: Can't match 2018 and 2017 up this way unfortunately-----------
  # dt18 <- read.csv(paste0(data_dir, pnlt_data_case_outcomes18))
  # dt18 <- as.data.table(dt18)
  # 
  # dt18[, incid_case:= ifelse (grepl("CAS INCID", dt18$sheet), "yes", "no")]
  # 
  # dt <-  rbindlist(list(dt17, dt18), use.names=TRUE, fill= TRUE)
  # ----------------------------------------------------------------------

# Incident cases by DPS, year, quarter
dt_incid <- dt17[incid_case == "yes", ]
dt_incid[, X:=NULL]
dt_incid <- dt_incid[, lapply(.SD, sum, na.rm=TRUE), by= c("dps", "data_year", "file_year", "quarter"), .SDcols= 2:10 ] 
  # by year and quarter and dps, totals for incident cases (across different sheets)

    # CALCULATIONS FOR COUNTRY REPORTS 11/15
      dt_trt2 <- dt_incid[, lapply(.SD, sum), by= c("data_year", "file_year", "quarter"), .SDcols= c("tot_cas_reg", "healed", "trt_complete") ]
      dt_trt[, quarter_trt_comp := (healed+trt_complete)/(tot_cas_reg)]
      dt_trt[, quarter_trt_comp := round(quarter_trt_comp, digits=4)]
      dt_trt[, quarter_trt_comp := quarter_trt_comp * 100]

# DRC national level quarterly numbers
dt_natl <- dt_incid[, lapply(.SD, sum, na.rm=TRUE), by= c("data_year", "file_year", "quarter"), .SDcols= 5:13 ]
  # by year and quarter, across all DPS  

# DPS level 2017 totals
# combine kongo-central-est and kongo-central-ouest into kongo-central
dt_annual <- dt_incid[ dps %in% c("kongo-central-est", "kongo-central-ouest"), dps:= "kongo-central"]
# sum by dps over quarters to get an annual value for each variable
all_vars <- colnames(dt_annual)
id_vars <- c("dps", "data_year", "file_year", "quarter")
vars_to_analyze <- all_vars[!all_vars %in% id_vars]
dt_annual  <- dt_annual[, lapply(.SD, sum, na.rm=TRUE), by = "dps", .SDcols = vars_to_analyze]

# copy which data table you want to use into dt 
dt <- copy(dt_natl)
dt <- copy(dt_incid)
dt <- copy(dt_annual)
# ----------------------------------------------

# create percent case_eval column:
dt[, percent_cases_eval := cas_eval/tot_cas_reg]
dt[, full_trt_course := healed + trt_complete]
dt[, pct_comp_full_trt_course := full_trt_course/tot_cas_reg] #using this rather than over the # of cases evaluated 
dt[, pct_died := died/tot_cas_reg]

# ~~~~~~~~~~
# dt_tbhiv <- read.csv(paste0(data_dir, pnlt_outcomes_18_tb_hiv))
# 
# dt <- rbindlist(list(dt_tbhiv, dt18), use.names = TRUE, fill = TRUE)
# dt[grepl("CAS INCID", dt$sheet), case_type:= "incident_case"]
# dt[grepl("TB VIH", dt$sheet), case_type:= "tb_hiv_case"]
# dt[grepl("ANT INCO", dt$sheet), case_type:= "not_incident_case"]
# ~~~~~~~~~~
# # Compare incident case numbers and year totals (with GBD/WHO estimates)
# dt_dep17 <- read.csv(paste0(data_dir, pnlt_case_screening_17))
# dt_dep17 <- as.data.table(dt_dep17)
# dt_dep17 <- dt_dep17[, .(dps, tot_incCase, file_year, quarter)]
# check <- dt[file_year==2017,]
# total_incident_cases_2017 <- dt_dep17[, sum(tot_incCase)]
# # Compare incident cases in T1 2017 DEP sheet with case outcomes in T1 2018 eval sheet
# dt_dep17 <- dt_dep17[quarter=="T1",]
#   
# dt18 <- read.csv(paste0(data_dir, pnlt_data_case_outcomes18))
# dt18 <- as.data.table(dt18)
# 
# dt18[, incid_case:= ifelse (grepl("CAS INCID", dt18$sheet), "yes", "no")]
# dt18 <- dt18[incid_case=="yes",]
#       # sum(dt18$tot_cas_reg)
#       # [1] 21496
#       # sum(dt_dep17$tot_incCase)
#       # [1] 34992
# dt18 <- dt18[, .(dps, tot_cas_reg, healed, trt_complete, died, trt_failed, lost_to_followup, cas_not_eval, cas_eval)]
# 
# dt <- merge(dt18, dt_dep17, by=c("dps"))
# setnames(dt, 'tot_cas_reg', 'cases_registered_EVAL')
# setnames(dt, 'tot_incCas', 'incident_cases_DEP')
# 
# g <- ggplot(dt, aes(cases_registered_EVAL, incident_cases_DEP, label=dps) ) + theme_bw() + 
#   geom_point() + coord_fixed() + geom_abline() + 
#   geom_text(aes(label=dps), hjust=0, vjust=0) +
#   xlim(0, 2000) + ylim(0, 2000) 
# 
# print(g)
# ~~~~~~~~~~
# ----------------------------------------------


# ----------------------------------------------
## variable set up
# ----------------------------------------------
# id_vars <- c('dps', 'sheet', 'tb_type', 'data_year', 'file_year', 'quarter')
# tb_type <- unique(dt$tb_type)
# dps_names <- unique(dt$dps)
# ----------------------------------------------


# ----------------------------------------------
## MOVE TO PREP CODE:
# ----------------------------------------------
# change class of vars
dt$tb_type <- as.character(dt$tb_type)
dt$dps <- as.character(dt$dps)
# rename cols
# setnames(dt, "TB_type", "tb_type")

# set quarters:
dt$quarter <- gsub("T", "", dt$quarter)
dt$quarter <- as.numeric(dt$quarter)

# create date variable:
dt[ file_year=="2017" & quarter=="1", date:= "2017-01-01"]
dt[ file_year=="2017" & quarter=="2", date:= "2017-04-01"]
dt[ file_year=="2017" & quarter=="3", date:= "2017-07-01"]
dt[ file_year=="2017" & quarter=="4", date:= "2017-10-01"]
dt[ file_year=="2018" & quarter=="1", date:= "2018-01-01"]

dt$date <- as.Date(dt$date)

# # melt data
# melted_dt <- melt.data.table(dt, id.vars=id_vars, variable.name = "outcome")
# melted_dt$outcome <- as.character(melted_dt$outcome)
# melted_dt <- melted_dt[ outcome != "cas_not_eval"]
# melted_dt <- melted_dt[ outcome != "ca_eval"]

dps_names <- unique(dt$dps)
dps_group1 <- dps_names[1:7]
dps_group2 <- dps_names[8:14]
dps_group3 <- dps_names[15:21]
dps_group4 <- dps_names[22:27]


dps_group1 <- c("kwilu", "kongo-central-ouest", "haut-lomami", "sud-kivu", "tshuapa" )
dps_group2 <- c("kinshasa", "maniema", "lomami", "tshopo")
dps_group3 <- c("kongo-central-est", "kasai-central", "haut-katanga", "sud-ubangi", "nord-ubangi" )
dps_group4 <- c("nord-kivu", "haut-uele", "mongala", "kwango" ) 
dps_group5 <- c("equateur", "ituri") 
dps_group6 <- c("kasai", "kasai-oriental","sankuru", "bas-uele") 
dps_group7 <- c("lualaba", "tanganyika", "mai-ndombe")


dt[dps %in% dps_group1, dps_group:=1]
dt[dps %in% dps_group2, dps_group:=2]
dt[dps %in% dps_group3, dps_group:=3]
dt[dps %in% dps_group4, dps_group:=4]
dt[dps %in% dps_group5, dps_group:=5]
dt[dps %in% dps_group6, dps_group:=6]
dt[dps %in% dps_group7, dps_group:=7]
# ----------------------------------------------


# ----------------------------------------------
## graphing function / loop through dps and tb_type and make graphs
# ----------------------------------------------
# # Graphs by "trend"
# pdf(file=paste0(output_dir, "cases_registered_over_time.pdf"), height = 9, width = 11)
# for (d in 1:7){
#     g <- ggplot(dt, aes(date, tot_cas_reg, color=dps) ) + theme_bw() + 
#       geom_point() + 
#       geom_line() + xlab("Date") + ylab("Percent") +
#       ggtitle(paste0("Registered TB cases over time")) +
#       scale_y_continuous(labels= scales:: comma)
#     print(g)
# }
# dev.off()
# ----------------------------------------------

# ----------------------------------------------
# Case outcomes for incident cases graphs
makeGraph <- function(variable, title, min_y, max_y){
  g <- ggplot(dt, aes(date, get(variable), color=dps, ymin=min_y, ymax=max_y) ) + theme_bw() + 
    geom_point() + 
    geom_line() + xlab("Date") + ylab("Percent") +
    ggtitle(paste0("2017: ", title)) +
    scale_y_continuous(labels=function(x)x*100)
}

makeGraphPercent <- function(variable, title, min_y, max_y){
  g <- ggplot(dt, aes(date, get(variable), ymin=min_y, ymax=max_y) ) + theme_bw() + 
    geom_point() + 
    geom_line() + xlab("Date") + ylab("Percent") +
    ggtitle(paste0("2017 DRC: ", title)) +
    scale_y_continuous(labels=function(x)x*100)
}

makeGraphNumber <- function(variable,title){
  g <- ggplot(dt, aes(date, get(variable)) ) + theme_bw() + 
    geom_point() + 
    geom_line() + xlab("Date") + ylab("Number") +
    ggtitle(paste0("2017 DRC: ", title)) +
    scale_y_continuous(labels = scales::comma)
}

pdf(file=paste0(output_dir, "case_outcomes_data/", compare_dps_graphs), height = 9, width = 11)
print(makeGraph("pct_comp_full_trt_course", "Percent of registered incident TB cases that recorded a completed course of treatment", .6, 1.0))
print(makeGraph("pct_died", "Percent of deaths out of incident TB cases registered", 0, .1))
print(makeGraph("percent_cases_eval", "Percent of incident TB cases registered that were evaluated", 0, .1))
dev.off()

pdf(file=paste0(output_dir, "case_outcomes_data/PNLT case outcomes - incident cases - national.pdf"), height = 9, width = 11)
print(makeGraphNumber("tot_cas_reg", "Number of registered incident TB cases"))
print(makeGraphPercent("percent_cases_eval", "Percent of incident TB cases registered that were evaluated", 0, .1))
print(makeGraphNumber("full_trt_course", "Number of registered incident TB cases that recorded a completed course \nof treatment"))
print(makeGraphNumber("trt_failed", "Number of registered incident TB cases that recorded a treatment failure"))
print(makeGraphPercent("pct_comp_full_trt_course", "Percent of registered incident TB cases that recorded a completed \ncourse of treatment", .6, 1.0))
print(makeGraphNumber("died", "Number of registered incident TB cases that died"))
print(makeGraphPercent("pct_died", "Percent of deaths out of incident TB cases registered", 0, .1))
dev.off()

dt[, yr_qtr := paste0(file_year, "-", quarter)]
dt[, yr_qtr := as.yearqtr(yr_qtr)]

pdf(paste0(output_dir, ts_completed_treatment_natl), height = 9, width = 11)
  g <- ggplot(dt, aes(yr_qtr, pct_comp_full_trt_course, ymin=0.60, ymax=1.0) ) + theme_bw() + 
    geom_point(color="navyblue") + 
    geom_line(color="navyblue") + xlab("Date") + ylab("Percent") +
    ggtitle("2017 DRC: Percent of registered incident TB cases that recorded a completed \ncourse of treatment") +
    scale_y_continuous(labels=function(x)x*100) + 
    geom_hline(yintercept=0.9, linetype="dashed", size = 1, alpha = 0.5) +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=16), plot.title = element_text(size=20), legend.position = "none", plot.caption = element_text(size=14)) +
    scale_x_yearqtr(limits = c(min(dt$yr_qtr), max(dt$yr_qtr)), format = "%Y-Q%q") +
    labs(caption= "Note: Dotted line represents WHO target of 90%.")
  g
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# TB HIV cases graphs
dps_to_remove <- dt[case_type=="tb_hiv_case" & is.na(pct_comp_full_trt_course), dps]
dt$X <- NULL
id_vars <- c(id_vars, "case_type", "date")
dt_melt <- melt(dt, id.vars = id_vars, variable.name="outcome")

pdf(file=paste0(output_dir, "PNLT_tbhiv_treatment_completed.pdf"), height = 8, width = 20)

g <- ggplot(dt_melt[outcome=="pct_comp_full_trt_course" & case_type != "not_incident_case" & !dps %in% dps_to_remove,], aes(dps, value, fill=case_type)) + theme_bw() + 
  geom_point() + 
  geom_bar(position="dodge", stat="identity") + xlab("Date") + ylab("Percent") +
  ggtitle("Percent of registered TB/HIV cases that recorded a completed course of treatment for TB by case_type, 1st quarter 2018")+
  scale_y_continuous(labels=function(x)x*100)

print(g)
dev.off()


g <- ggplot(dt[case_type == "tb_hiv_case"], aes(date, pct_comp_full_trt_course, color=dps, ymin=0, ymax=1) ) + theme_bw() + 
  geom_point() + 
  geom_line() + xlab("Date") + ylab("Percent") +
  ggtitle("Percent of registered TB/HIV cases that recorded a completed course of treatment for TB")+
  scale_y_continuous(labels=function(x)x*100)


# makeGraph <- function(data_table, dps){
#   g <- ggplot(data_table[dps == dps,], aes(quarter, value, color=outcome) ) + theme_bw() + 
#     geom_point() + 
#     geom_line() + 
#     ggtitle(dps) +
#     facet_wrap(~ tb_type, scales="free_y")
#   return(g)
# }
# ----------------------------------------------

# ----------------------------------------------
# Initial descriptive analysis graphs

for (i in tb_type){
  pdf(paste0(output_dir, i, "_graph.pdf"), height = 9, width = 11)
  
  for (j in dps_names){
    
    g <- ggplot(melted_dt[dps == j & tb_type == i,], aes(quarter, value, color=outcome, ymin=0) ) + theme_bw() + 
      geom_point() + 
      geom_line() + 
      ggtitle(paste0("dps: ", j, " and tb type: ", i)) +
      facet_wrap(~ outcome, scales="free_y") +
      guides(color=FALSE)
    
    print(g)
  }
  
  dev.off()
}
# ----------------------------------------------

# ----------------------------------------------
# Map case outcomes data percent successful treatment by DPS
# ----------------------------------------------
coordinates = as.data.table(fortify(drcShape, region='NAME_1'))
coordinates$id <- standardizeDPSnames(coordinates$id)
# this name doesn't save properly so you may have to re copy
coordinates$id <- gsub("ã???equateur", "equateur", coordinates$id)

graphData <- merge(coordinates, dt, by.x='id', by.y='dps', all=TRUE, allow.cartesian=TRUE)
graphData$pct_comp_full_trt_course <- graphData$pct_comp_full_trt_course *100

max = max(graphData$pct_comp_full_trt_course, na.rm=TRUE)
min = min(graphData$pct_comp_full_trt_course, na.rm=TRUE)
med = max/2

# make color palette 
colors = brewer.pal(9, 'RdYlGn')
# make labels centered for each DPS 
names = data.table(coordinates(drcShape))
setnames(names, c('long', 'lat'))
names[ , dps:=unique(drcShape@data$NAME_1)]
names$dps <- standardizeDPSnames(names$dps)
# this name doesn't save properly so you may have to re copy
names$dps <- gsub("ã???equateur", "equateur", names$dps)

names$long <- round(names$long, digits=3)
names$lat <- round(names$lat, digits=3)
# merge with data to get the values to include in the labels
names <- merge(names, dt, by="dps")

# capitalization function
capitalize <- function(in_vector, character_to_split) {
  out_vector = c()
  for (i in 1:length(in_vector)){
    s <- strsplit(in_vector[i], character_to_split)[[1]]
    s <- paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
    out_vector[i] = s
  }
  return (out_vector)
}

names$dps <- as.character(names$dps)
names$dps <- capitalize(names$dps, "-")

pdf(paste0(output_dir, map_case_outcomes_completed_treatment), height=10, width=15)
m1 <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=pct_comp_full_trt_course)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradientn(colors=colors, 
                       na.value = "grey70", space = "Lab", 
                       breaks=round(seq(min, max, by=((max-min)/4))), 
                       limits=c(min,max)) +
  labs(title="2017 DRC: Percent of registered incident TB cases that rec orded a completed \ncourse of treatment (NOTE: WHO target is 90%)", 
       fill='Percent') +
  geom_label_repel(data = names, aes(label = paste0(dps, ":\n" , round((pct_comp_full_trt_course*100), 2), "%\n (n= ", tot_cas_reg, ")"), x = long, y = lat, group = dps), inherit.aes=FALSE, size=3) +
  theme(plot.title = element_text(size = 20), legend.title = element_text(size=16))
m1
dev.off()

# remake the same graph, labelling GF vs non-GF provinces
all_dps <- unique(dt$dps)
gf_dps <-  c("kwango","kwilu","mai-ndombe","kongo-central", "equateur","mongala","nord-ubangi","sud-ubangi","tshuapa",        
            "kinshasa","maniema","nord-kivu","sud-kivu","haut-uele", "bas-uele","ituri", "tshopo") 

# need to re run this code so names match up to gf_dps
# make labels centered for each DPS 
names = data.table(coordinates(drcShape))
setnames(names, c('long', 'lat'))
names[ , dps:=unique(drcShape@data$NAME_1)]
names$dps <- standardizeDPSnames(names$dps)
# this name doesn't save properly so you may have to re copy
names$dps <- gsub("ã???equateur", "equateur", names$dps)

names$long <- round(names$long, digits=3)
names$lat <- round(names$lat, digits=3)
# merge with data to get the values to include in the labels
names <- merge(names, dt, by="dps")

pdf(paste0(output_dir, map_comp_trt_with_GF_marked), height=10, width=15)
map <- ggplot() + 
  geom_polygon(data=graphData, aes(x=long, y=lat, group=group, fill=pct_comp_full_trt_course)) + 
  coord_equal() +
  geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
  theme_void() +  
  scale_fill_gradientn(colors=colors, 
                       na.value = "grey70", space = "Lab", 
                       breaks=round(seq(min, max, by=((max-min)/4))), 
                       limits=c(min,max)) +
  labs(title="2017 DRC: Percent of registered incident TB cases that recorded a completed \ncourse of treatment with GF provinces starred", 
       fill='Percent', caption = "(NOTE: WHO target is 90%)") +
  geom_point(data = names[dps %in% gf_dps,], aes(x=long, y=lat), alpha=0.9, shape=8, inherit.aes=FALSE, size=5) +
  theme(plot.title = element_text(size = 20), legend.title = element_text(size=18), legend.text = element_text(size=14), plot.caption = element_text(size=18))
map
dev.off()

