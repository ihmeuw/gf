# ----------------------------------------------
# Audrey Batzel
# 8/7/2018
# Map visualizations of PNLT data
# ----------------------------------------------
## Set up R / install packages 
# ----------------------------------------------
rm(list=ls())
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(maptools)
# ----------------------------------------------


# ----------------------------------------------
## set up the directories to read/export files: 
# ----------------------------------------------
# Directories
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j/')

export_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLT_data/')
data_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/")

# input files
pnlt_data_dep <- "PNLT_prepped_data.csv"
pnlt_data_case_outcomes17 <- "PNLT_case_outcomes_2017.csv"
pnlt_data_case_outcomes18 <- "PNLT_case_outcomes_2018.csv"
pnlt_outcomes_18_tb_hiv <- "PNLT_case_outcomes_2018_TBHIV.csv"
pnlt_case_screening_17 <- "PNLT_case_screening_2017.csv"

# output files
compare_dps_graphs <- "PNLT_case_outcomes_by_dps.pdf"
compare_dps_graphs_faceted <- "PNLT_case_outcomes_by_dps_(faceted).pdf"
# ----------------------------------------------


# ----------------------------------------------
## load data / prep continued (move to prep code)
# ----------------------------------------------
# ~~~~~~~~~~
dt17 <- read.csv(paste0(data_dir, pnlt_data_case_outcomes17))
dt17 <- as.data.table(dt17)

incid_cases_patterns <- c("NC", "RECH")
dt17[, incid_case := ifelse (grepl(paste(incid_cases_patterns, collapse="|"), dt17$TB_type), "yes", "no")]

dt18 <- read.csv(paste0(data_dir, pnlt_data_case_outcomes18))
dt18 <- as.data.table(dt18)

dt18[, incid_case:= ifelse (grepl("CAS INCID", dt18$sheet), "yes", "no")]

dt <-  rbindlist(list(dt17, dt18), use.names=TRUE, fill= TRUE)
dt[, X:=NULL]

dt_incid <- dt[incid_case == "yes", ]
dt_incid <- dt_incid[, lapply(.SD, sum, na.rm=TRUE), by= c("dps", "data_year", "file_year", "quarter"), .SDcols= 2:10 ]

dt <- copy(dt_incid)
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
# set quarters:
dt$quarter <- gsub("T", "", dt$quarter)
dt$quarter <- as.numeric(dt$quarter)

# rename cols
# setnames(dt, "TB_type", "tb_type")

# change class of vars
dt$tb_type <- as.character(dt$tb_type)
dt$dps <- as.character(dt$dps)

# create date variable:
dt[ file_year=="2017" & quarter=="1", date:= "2017-01-01"]
dt[ file_year=="2017" & quarter=="2", date:= "2017-04-01"]
dt[ file_year=="2017" & quarter=="3", date:= "2017-07-01"]
dt[ file_year=="2017" & quarter=="4", date:= "2017-10-01"]
dt[ file_year=="2018" & quarter=="1", date:= "2018-01-01"]

dt$date <- as.Date(dt$date)

# create percent case_eval column:
dt[, percent_cases_eval := cas_eval/tot_cas_reg]
dt[, full_trt_course := healed + trt_complete]
dt[, pct_comp_full_trt_course := full_trt_course/tot_cas_reg] #using this rather than over the # of cases evaluated 
dt[, pct_died := died/tot_cas_reg]

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


dps_group1 <- c("kwilu", "kongo-central-ouest", "haut???lomami", "sud???kivu", "tshuapa" )
dps_group2 <- c("kinshasa", "maniema", "lomami", "tshopo")
dps_group3 <- c("kongo-central-est", "kasai???central", "haut???katanga", "sud-ubangi", "nord-ubangi" )
dps_group4 <- c("nord???kivu", "haut???uele", "mongala", "kwango" ) 
dps_group5 <- c("equateur", "ituri") 
dps_group6 <- c("kasai", "kasai???oriental","sankuru", "bas???uele") 
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
# make a graph for each DPS
pdf(file=paste0(export_dir, "PNLT_percent_trt_comp_by_trend.pdf"), height = 9, width = 11)
  
for (d in 1:7){
    g <- ggplot(dt[dps_group==d, ], aes(date, pct_comp_full_trt_course, color=dps, ymin=.6, ymax=1) ) + theme_bw() + 
      geom_point() + 
      geom_line() + xlab("Date") + ylab("Percent") +
      ggtitle(paste0(d, ": Percent of registered TB cases that recorded a completed course of treatment")) +
      scale_y_continuous(labels=function(x)x*100)
    print(g)
}

dev.off()


pdf(file=paste0(export_dir, compare_dps_graphs), height = 9, width = 11)

print(makeGraph("pct_comp_full_trt_course", "Percent of registered TB cases that recorded a completed course of treatment", .6, 1.0))
print(makeGraph("pct_died", "Percent of deaths out of TB cases registered", 0, .1))

dev.off()


makeGraph <- function(variable, title, min_y, max_y){
  g <- ggplot(dt, aes(date, get(variable), color=dps, ymin=min_y, ymax=max_y) ) + theme_bw() + 
    geom_point() + 
    geom_line() + xlab("Date") + ylab("Percent") +
    ggtitle(title)+
    scale_y_continuous(labels=function(x)x*100)
}


dps_to_remove <- dt[case_type=="tb_hiv_case" & is.na(pct_comp_full_trt_course), dps]
dt$X <- NULL
id_vars <- c(id_vars, "case_type", "date")
dt_melt <- melt(dt, id.vars = id_vars, variable.name="outcome")

pdf(file=paste0(export_dir, "PNLT_tbhiv_treatment_completed.pdf"), height = 8, width = 20)

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

for (i in tb_type){
  pdf(paste0(export_dir, i, "_graph.pdf"), height = 9, width = 11)
  
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



