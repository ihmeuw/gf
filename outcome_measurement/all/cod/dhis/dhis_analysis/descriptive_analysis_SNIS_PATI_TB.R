# ----------------------------------------------
# Audrey Batzel
#
# 1/29/19
# Descriptive analysis of PATI TB prepped data and comparison with PNLT Q1 2018
setwd('C:/local/gf/')
# ----------------------------------------------

# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
# --------------------

# ----------------------------------------------
# Overview - Files and Directories
# ----------------------------------------------
# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
snis_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')
pnlt_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/')
pnlp_dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLP/post_imputation/')
out_dir = 'J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/SNIS_PATI_TB/'

# input files
pati_cases <- paste0(snis_dir, "pati_tb/tb_pati_new_tb_cases_relapses_by_age_sex.rds")
pati_registered <- paste0(snis_dir, "pati_tb/tb_pati_cases_registered.rds")
pati_results <- paste0(snis_dir, "pati_tb/tb_pati_case_results.rds")
pnlt_case_outcomes <- paste0(pnlt_dir, 'PNLT_case_outcomes_2018.csv')
pnlt_case_screening <- paste0(pnlt_dir,"PNLT_case_screening_2018.csv")
pnlp_hz_level = paste0(pnlp_dir, "archive/imputedData_run2_agg_hz.rds")
pnls_data = paste0(snis_dir, "pnls_sets/pnls_clean_all_sets.rds")
# ----------------------------------------------

# ----------------------------------------------
# Read in data
# ----------------------------------------------
snis_cases <- readRDS(pati_cases)
snis_reg <- readRDS(pati_registered)
snis_res <- readRDS(pati_results)
pnlt_res <- read.csv(pnlt_case_outcomes, stringsAsFactors = FALSE) %>% as.data.table()
pnlt_reg <- read.csv(pnlt_case_screening)
pnlp_hz <- readRDS(pnlp_hz_level)
pnls <- readRDS(pnls_data)
# ----------------------------------------------

# ----------------------------------------------
# set up for comparison of case results(case outcomes) in PNLT and SNIS
# ----------------------------------------------
# first, aggregate snis_res to dps level
sd_cols <- colnames(snis_res)[grepl("TB_", colnames(snis_res))]
snis_res_dps <- snis_res[, lapply(.SD, sum, na.rm=TRUE), by = .(dps, element, element_eng, quarter, date), .SDcols = sd_cols]
# subset to just Q1 2018
snis_res_dps <- snis_res_dps[date == "2018-01-01", ]
snis_res_dps$year <- year(snis_res_dps$date)
# rename vars
names(snis_res_dps) <- c("dps", "element", "element_eng", "quarter", "date", "died", "trt_failed", "healed", "cas_not_eval", "lost_to_followup", "trt_complete", "year")
snis_res_dps$element <- as.character(snis_res_dps$element)
snis_res_dps$element <- trimws(snis_res_dps$element)
snis_res_dps[element== "TB_Enfants 0-14 ans", element_eng:= "children_0to14yrs"]
snis_res_dps[element== "TB_Patients co infectés TB/VIH+ (Np+Rech)", element_eng:= "tbhiv_patients_new_relapse"]
snis_res_dps[element== "TB_Cas de la TB Extra pulmonaire", element_eng:= "tep_case"]
snis_res_dps[element== "TB_Patients  suivis par la communauté", element_eng:= "patients_followed_by_community"]
snis_res_dps[element== "TB_Cas confirmés bactériologiquement (Nouveaux et rechutes)", element_eng:= "tpp_new_relapse"]
snis_res_dps[element== "TB_Cas diagnostiqués cliniquement (Nouveaux et rechutes)", element_eng:= "tpc_new_relapse"]
snis_res_dps[element== "TB_Déjà traité Hors Rechutes", element_eng:= "case_alreadyTreated_notRelapse"]

pnlt_res$source = "PNLT"
snis_res_dps$source = "SNIS_pati"

# exclude some vars to make the data sets match up
# exclude children_0to14yrs and tbhiv_patients_new_relapse for now because those are not in the current PNLT set
snis_res_dps_subset <- snis_res_dps[element_eng %in% c("children_0to14yrs", "tbhiv_patients_new_relapse", "patients_followed_by_community", "case_alreadyTreated_notRelapse")]
snis_res_dps <- snis_res_dps[!element_eng %in% c("children_0to14yrs", "tbhiv_patients_new_relapse", "patients_followed_by_community", "case_alreadyTreated_notRelapse")]

pnlt_res_not_incident_cases <- pnlt_res[ sheet== "EVAL HR ET ANT INCO T1", ]
pnlt_res <- pnlt_res[ sheet!= "EVAL HR ET ANT INCO T1", ]
pnlt_res <- pnlt_res[grep(dps, pattern="kongo-central"), dps:= "kongo-central"]
setnames(pnlt_res, "data_year", "year")

drop_vars <- c("sheet", "X", "file_year", "element", "element_eng", "date")
pnlt_res<- pnlt_res[, !names(pnlt_res) %in% drop_vars, with = FALSE ]
snis_res_dps<- snis_res_dps[, !names(snis_res_dps) %in% drop_vars, with = FALSE ]
snis_res_dps[quarter=="Q1", quarter:= "1"]
snis_res_dps$quarter = as.numeric(snis_res_dps$quarter)

id_vars = c('dps', 'year', 'quarter', 'source')

sd_cols <- colnames(pnlt_res)[!colnames(pnlt_res) %in% id_vars]
pnlt_res <- pnlt_res[, lapply(.SD, sum, na.rm=TRUE), by = id_vars, .SDcols = sd_cols]

sd_cols <- colnames(snis_res_dps)[!colnames(snis_res_dps) %in% id_vars]
snis_res_dps <- snis_res_dps[, lapply(.SD, sum, na.rm=TRUE), by = id_vars, .SDcols = sd_cols]

snis_res_dps[, cas_eval := died + trt_failed + healed + lost_to_followup + trt_complete ]

pnlt_res <- melt.data.table(pnlt_res, id.vars = id_vars, variable.name = "indicator",  variable.factor = FALSE)
snis_res_dps <- melt.data.table(snis_res_dps, id.vars = id_vars, variable.name = "indicator", variable.factor = FALSE)
snis_res_dps$quarter = as.numeric(snis_res_dps$quarter)
# ----------------------------------------------

# ----------------------------------------------
# compare case results(case outcomes) in PNLT and SNIS
# ----------------------------------------------
# both pnlt_res and snis_res_dps are now at the dps level and only Q1
# merge
id_vars = c('dps', 'year', 'quarter', 'indicator')
dt <- rbindlist(list(snis_res_dps, pnlt_res), use.names = TRUE, fill = TRUE)
dt <- dcast.data.table(dt, dps + year + quarter + indicator ~ source, value.var = 'value')
dt <- dt[ indicator != "tot_cas_reg",]

# graph
  g <- ggplot(dt, aes(x=PNLT, y=SNIS_pati)) + 
    geom_point() + geom_abline() +
    ggtitle(paste0("Comparison between PNLT and SNIS_PATI among incident cases")) +
    theme_bw() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
          legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
    facet_wrap( ~indicator, scales = "free") +
    scale_y_continuous( label= scales :: comma )
  print(g)
  
  g <- ggplot(dt[indicator == "cas_eval",], aes(x=PNLT, y=SNIS_pati)) + 
    geom_point() + geom_abline() +
    ggtitle(paste0("Comparison between PNLT and SNIS_PATI - Q1 2018; cases evaluated")) +
    theme_bw() +
    theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
          legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
    scale_y_continuous( label= scales :: comma, limits = c(0, 2000))+
    xlim(0, 2000) + labs(caption= "Each point represents a DPS \n Note: Kinshasa omitted; Q1 2018 is our only timepoint of overlap in the data sources")
  print(g)
# dev.off()
# ----------------------------------------------

# ----------------------------------------------
# number of facilities reporting national and dps, time series graph
# ----------------------------------------------
num_fac_over_time <- snis_res[, unique(org_unit_id), by = c("date", "dps")]
num_fac_over_time <- num_fac_over_time[, .N, by= c("date", "dps")] 
num_fac_over_time$data = "snis_pati_case_results"
num_fac_over_time[, date := as.Date(date)]

num_fac_over_time2 <- snis_reg[, unique(org_unit_id), by = c("date", "dps")]
num_fac_over_time2 <- num_fac_over_time2[, .N, by= c("date", "dps")]
num_fac_over_time2$data = "snis_pati_cases_registered"
num_fac_over_time2[, date := as.Date(date)]

num_fac_over_time_natl <- rbindlist(list(num_fac_over_time, num_fac_over_time2), fill= TRUE, use.names = TRUE)
# num_fac_over_time_natl <- dcast.data.table(num_fac_over_time_natl, date ~ data, value.var = "N")
# basically shows we only want to use 2018-Q1 to 2018-Q3 for now
num_fac_over_time_natl[, date := as.Date(date)]

g <- ggplot(num_fac_over_time_natl, aes(x=date, y=N, color = data)) +
  geom_point() + geom_line() +
  ggtitle(paste0("Number of facilities reporting over time, nationally")) +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16),
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  scale_y_continuous( label= scales :: comma ) +
  labs(caption= "Source: DHIS2 PATI TB; downloaded January 2019", y= "Number of facilities") +
  guides(color = guide_legend(title = "Data sheet:"))
print(g)

pdf(paste0(out_dir, "ts_facilities_reporting_natl.pdf"), height = 9, width = 13)
print(g)
dev.off()

num_fac_over_time[ , i := .GRP, by = "dps"]
num_fac_over_time[ i %in% 1:5, group := 1]
num_fac_over_time[ i %in% 6:10, group := 2]
num_fac_over_time[ i %in% 11:15, group := 3]
num_fac_over_time[ i %in% 16:20, group := 4]
num_fac_over_time[ i %in% 21:26, group := 5]
num_fac_over_time2[ , i := .GRP, by = "dps"]
num_fac_over_time2[ i %in% 1:5, group := 1]
num_fac_over_time2[ i %in% 6:10, group := 2]
num_fac_over_time2[ i %in% 11:15, group := 3]
num_fac_over_time2[ i %in% 16:20, group := 4]
num_fac_over_time2[ i %in% 21:26, group := 5]

pdf(paste0(out_dir, "ts_facilities_reporting_DPS.pdf"), height = 9, width = 13)
for (x in unique(num_fac_over_time$group)) {
g <- ggplot(num_fac_over_time[group == x, ], aes(x=date, y=N, color = dps)) + 
  geom_point() + geom_line() +
  ggtitle(paste0("Number of facilities reporting over time, by dps")) +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  scale_y_continuous( label= scales :: comma ) +
  labs(caption= "Source: SNIS PATI case results; downloaded January 2019", y = "Number of facilities")
print(g)
}

for (x in unique(num_fac_over_time2$group)) {
g <- ggplot(num_fac_over_time2[group == x, ], aes(x=date, y=N, color = dps)) + 
  geom_point() + geom_line() +
  ggtitle(paste0("Number of facilities reporting over time, by dps")) +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  scale_y_continuous( label= scales :: comma ) +
  labs(caption= "Source: SNIS PATI cases registered; downloaded January 2019", y = "Number of facilities" )
print(g)
}
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# number of facilities over time by health zone and date
  # and by dps and date
# ----------------------------------------------
dt4 <- snis_reg[, unique(org_unit_id), by = c("date", "dps")]
dt4 <- dt4[, .N, by= c("date", "dps")]
dt4$data = "snis_pati_cases_registered"

dt2 <- snis_res[, unique(org_unit_id), by = c("date", "dps")]
dt2 <- dt2[, .N, by= c("date", "dps")]
dt2$data = "snis_pati_case_results"

num_fac_over_time_dps <- rbindlist(list(dt2, dt4), fill= TRUE, use.names = TRUE)
num_fac_over_time_dps <- dcast.data.table(num_fac_over_time_dps, date + dps ~ data, value.var = "N")

dt1 <- snis_res[, unique(org_unit_id), by = c("date", "dps", "health_zone")]
dt1 <- dt1[, .N, by= c("date", "dps", "health_zone")]
dt1$data = "snis_pati_case_results"

dt3 <- snis_reg[, unique(org_unit_id), by = c("date", "dps", "health_zone")]
dt3 <- dt3[, .N, by= c("date", "dps", "health_zone")]
dt3$data = "snis_pati_cases_registered"

num_fac_over_time_hz <- rbindlist(list(dt1, dt3), fill= TRUE, use.names = TRUE)
num_fac_over_time_hz <- dcast.data.table(num_fac_over_time_hz, date + dps + health_zone ~ data, value.var = "N")
num_fac_over_time_hz <- num_fac_over_time_hz[date >= "2018-01-01"]
# ----------------------------------------------

# ----------------------------------------------
# number of health zones over time by DPS and date
# ----------------------------------------------
dt1 <- snis_res[, unique(health_zone), by = c("date", "dps")]
dt1 <- dt1[, .N, by= c("date", "dps")]
dt1$data = "snis_pati_case_results"

dt2 <- snis_reg[, unique(health_zone), by = c("date", "dps")]
dt2 <- dt2[, .N, by= c("date", "dps")]
dt2$data = "snis_pati_cases_registered"

# pnlp num of hzs to compare to
dt3 <- pnlp_hz[, unique(health_zone), by = c("date", "dps")]
dt3 <- dt3[, .N, by= c("date", "dps")]
setnames(dt3, "N", "pnlp")
dt3 <- dt3[ date == "2017-12-01"]
dt3 <- dt3[dps != "0",]

num_hz_over_time <- rbindlist(list(dt1, dt2), fill= TRUE, use.names = TRUE)
num_hz_over_time <- dcast.data.table(num_hz_over_time, date + dps ~ data, value.var = "N")
num_hz_over_time <- num_hz_over_time[date >= "2018-01-01"]
num_hz_over_time <- merge(num_hz_over_time, dt3, by ="dps")
# ----------------------------------------------

# ----------------------------------------------
# make a series of exploratory graphs to show natl level descriptive analysis
# ----------------------------------------------
snis_reg_natl <-  snis_reg[, .(value = sum(value, na.rm = TRUE)), by = .(element, element_eng, quarter, date, category)]
snis_reg_natl$element <- as.character(snis_reg_natl$element)

# pdf("J:/Project/Evaluation/GF/outcome_measurement/cod/visualizations/SNIS_PATI_TB/descriptive_analysis_pati_tb_cases_reg.pdf", height = 9, width = 11)
# for (e in unique(snis_reg_natl$element)) {
#   g <- ggplot(snis_reg_natl[element == e, ], aes(x=date, y=value, color = category)) +
#     geom_point() + geom_line() +
#     ggtitle(paste0(e, "\n", snis_reg_natl[element == e, unique(element_eng)])) +
#     theme_bw() +
#     theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16),
#           legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
#     scale_y_continuous( label= scales :: comma )
#   print(g)
# }
# dev.off()
# ----------------------------------------------

# ----------------------------------------------
# Comparison of IPT for HIV between PNLS / PATI
# ----------------------------------------------
###################
 ##### TO DO #####
###################
# ----------------------------------------------

# ----------------------------------------------
# national level prisoners/miners data from SNIS
# ----------------------------------------------
kvp = snis_reg_natl[ grepl(pattern="Mineurs", element, ignore.case = TRUE), ]
kvp = rbind(kvp, snis_reg_natl[ grepl(pattern="Prison", element, ignore.case = TRUE), ])
kvp[ grepl(pattern="Mineurs", element, ignore.case = TRUE), population:= "miners" ]
kvp[ grepl(pattern="Prison", element, ignore.case = TRUE), population:= "prisonners" ]
kvp[ grepl(pattern="trait", element, ignore.case = TRUE), variable:= "cases_treated" ]
kvp[ is.na(variable), variable:="number_of_cases"]


g <- ggplot(kvp, aes(x=date, y=value, color = population)) +
  geom_point() + geom_line() +
  ggtitle("Key/Vulnerable populations: prisonners and miners") +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16),
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14), 
        strip.text = element_text(size = 14)) +
  scale_y_continuous( label= scales :: comma ) +
  facet_wrap( ~ variable, scales = "free")
g

kvp_var_wide = copy(kvp)
kvp_var_wide[, c('element', 'element_eng'):= NULL]
kvp_var_wide = dcast.data.table(kvp, quarter + date + category + population ~ variable)
kvp_var_wide[, percent_cases_treated := (cases_treated / number_of_cases)*100]

g2 <- ggplot(kvp_var_wide, aes(x=date, y=percent_cases_treated)) +
  geom_point() + geom_line() +
  ggtitle("Key/Vulnerable populations: prisonners and miners, percent of cases treated") +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16),
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14), 
        strip.text = element_text(size = 14)) +
  scale_y_continuous( label= scales :: comma ) 
g2


pdf(paste0(out_dir, "ts_kvp_miners_prisoners_data.pdf"), height = 9, width = 13)
print(g)
print(g2)
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# national level treatment and testing data from SNIS
# ----------------------------------------------
treatment_natl <- snis_reg_natl[grepl(pattern= "RH", element), ]
treatment_natl$indicator <- "TB_case_treated"
treatment_natl[ , element := gsub("TB_Nombre ", "", element)]
treatment_natl[, c("adult_or_child", "initial_or_retrt", "dose") := tstrsplit(element, " ", keep=c(1,2,3))]
treatment_natl[, dose:= element]
treatment_natl[, dose := ifelse(adult_or_child == "adulte", gsub(pattern = "adulte ", " ", dose),  gsub(pattern = "enfant ", " ", dose) )]
treatment_natl$dose <- trimws(treatment_natl$dose)
treatment_natl[, dose := ifelse(initial_or_retrt == "Initial", gsub(pattern = "Initial ", " ", dose),  gsub(pattern = "Retrt ", " ", dose) )]
treatment_natl$dose <- trimws(treatment_natl$dose)

treatment_natl <- treatment_natl[ dose == "2RHZE / 4RH"]

g1 <- ggplot(treatment_natl, aes(x=date, y=value, color = dose)) +
  geom_point() + geom_line() +
  ggtitle("TB Treatment - nationally (source: SNIS PATI TB)") +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16),
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14), 
        strip.text = element_text(size = 14)) +
  scale_y_continuous( label= scales :: comma ) +
  facet_grid(initial_or_retrt ~ adult_or_child, scales = "free")

testing_natl <- snis_reg_natl[grepl(pattern= "Frottis", element), ]
testing_natl <-  testing_natl[!grepl(pattern= "Cas", element), ]
testing_natl[grepl(pattern = "Expert", element), test_type := "Xpert"]
testing_natl[is.na(test_type), test_type := "Smear"]
testing_natl[grepl(pattern = "positifs", element), indicator := "test_positive"]
testing_natl[grepl(pattern = "effectu", element), indicator := "test_complete"]

g2 <- ggplot(testing_natl, aes(x=date, y=value, color = indicator)) +
  geom_point() + geom_line() +
  ggtitle("TB Testing - nationally (source: SNIS PATI TB)") +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16),
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14),
        strip.text = element_text(size = 14)) +
  scale_y_continuous( label= scales :: comma ) +
  facet_wrap(~ test_type, scales = "free")


pdf(paste0(out_dir, "tb_treatment_testing_natl.pdf"), height = 9, width = 13)
print(g1)
print(g2)
dev.off()
# ----------------------------------------------

# ----------------------------------------------
# comparison of case registry between PNLT/SNIS
# ----------------------------------------------
# subset to just case data
dt_case <- snis_reg[grepl("Cas", element, ignore.case = TRUE),]
dt_case <- setorderv(dt_case, 'element')
dt_incCase <- dt_case[grepl("nouveau|rechute", element, ignore.case = TRUE),]
dt_incCase <- dt_incCase[!grepl("hors", element, ignore.case = TRUE),]

# aggregate snis_reg to dps level

dt_incCase_dps <- dt_incCase[, .(value=sum(value, na.rm = TRUE)), by = c('date', 'dps', 'element', "element_eng")]

dt_incCase_dps[grepl("nouveau", element, ignore.case = TRUE), type_of_case:= "new"]
dt_incCase_dps[grepl("rechute", element, ignore.case = TRUE), type_of_case:= "relapse"]
dt_incCase_dps[grepl("pulmonaire, confirmés bactériologiquement", element, ignore.case = TRUE), tb_diagnostic:= "tpp"]
dt_incCase_dps[grepl("extrapulmonaire", element, ignore.case = TRUE), tb_diagnostic:= "tep"]
dt_incCase_dps[grepl("pulmonaire, diagnostiqués cliniquement", element, ignore.case = TRUE), tb_diagnostic:= "tpc"]
dt_incCase_dps[, variable := paste(tb_diagnostic, type_of_case, sep = "_")]

dt_incCase_dps = dt_incCase_dps[, .(date, dps, variable, value)]

# subset to just Q1 2018
snis_comp = dt_incCase_dps[date == "2018-01-01"]
snis_comp[, data_source:= "SNIS_PATI"]

# prep pnlt to match up
pnlt_comp = pnlt_reg[, .(date, dps, tpp_new, tpp_relapse, tpc_new, tpc_relapse, tep_new, tep_relapse)]
pnlt_comp = pnlt_comp[grep(dps, pattern="kongo-central"), dps:= "kongo-central"]
pnlt_comp = melt.data.table(pnlt_comp, id.vars=c("date", "dps") )
pnlt_comp <- pnlt_comp[, .(value=sum(value, na.rm = TRUE)), by = c('date', 'dps', 'variable')]
pnlt_comp[, data_source:= "PNLT"]
pnlt_comp[, date:= as.Date(date)]
pnlt_comp = pnlt_comp[date == "2018-01-01"]

# merge
dt <- rbindlist(list(pnlt_comp, snis_comp), use.names = TRUE, fill = TRUE)
dt[, dps := as.character(dps)]
dt_wide <- dcast.data.table(dt, date + dps + variable ~ data_source)
dt_wide[ , diff := PNLT - SNIS_PATI]

# graph
g <- ggplot(dt_wide, aes(x=PNLT, y=SNIS_PATI)) + 
  geom_point() + geom_abline() +
  ggtitle(paste0("Comparison between PNLT and SNIS_PATI -\ncases registered data sheet")) +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~variable, scales = "free") +
  scale_y_continuous( label= scales :: comma )
print(g)

g2 <- ggplot(dt_wide[dps != "kinshasa"], aes(x=PNLT, y=SNIS_PATI)) + 
  geom_point() + geom_abline() +
  ggtitle(paste0("Comparison between PNLT and SNIS_PATI -\ncases registered data sheet, without Kinshasa")) +
  theme_bw() +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~variable, scales = "free") +
  scale_y_continuous( label= scales :: comma )
print(g2)

pdf(paste0(out_dir, "SNIS_PNLT_comparison_of_cases_registered.pdf"), height = 9, width = 13)
print(g)
print(g2)
dev.off()
# ----------------------------------------------

