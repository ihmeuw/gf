# Audrey Batzel 
# 12-10-18
#
# Figures for results chain for DRC report 
setwd('C:/local/gf/')
# ----------------------------------------------


# --------------------
# Set up R / install packages
# --------------------
rm(list=ls())
library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(readxl)
library(stats)
library(Rcpp)
# --------------------  

# ---------------------------------------------------
# Files and directories
# ---------------------------------------------------
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir <-paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/')
dir_pnlp <- paste0(dir, 'prepped_data/PNLP/')
dir_dhis = paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/prepped/')

# input files
pnlp_natl <- "post_imputation/imputedData_run2_agg_country.rds"
dhis_base <- "base_services_drc_01_2017_09_2018_prepped.rds"
dhis_sigl <- "sigl_drc_01_2015_07_2018_prepped.rds"
pnlp_preMI <- "final_data_for_imputation.csv"

# output files
out_dir = paste0(root, "Project/Evaluation/GF/results_chains/cod/malaria/")
pnlp_natl_filepath = paste0(out_dir, "prepped_data/pnlp_natlAgg_data.rds")  # can also use these vars to read in these files
base_natl_filepath = paste0(out_dir, "prepped_data/base_natlAgg_data.rds")
sigl_natl_filepath = paste0(out_dir, "prepped_data/sigl_natlAgg_data.rds")
all_data_filepath = paste0(out_dir, "prepped_data/data_sources_combined_natl.rds")

# functions
source('./core/standardizeHZNames.R')
source('./core/standardizeDPSNames.R')
# ---------------------------------------------------

# ---------------------------------------------------
# Load prepped data
# ---------------------------------------------------
pnlp <- readRDS(paste0(dir_pnlp, pnlp_natl)) # pnlp data at the national level by month and indicator
snis <- readRDS(paste0(dir_dhis, dhis_base)) # snis data still needs to be aggregated
sigl <- readRDS(paste0(dir_dhis, dhis_sigl))

before_MI <- read.csv(paste0(dir_pnlp, pnlp_preMI), stringsAsFactors = FALSE)
# ---------------------------------------------------

# ---------------------------------------------------
# calculate number of missing data points ("imputed data points") per year/variable
# ---------------------------------------------------
before_MI <- as.data.table(before_MI)
sd.cols = colnames(before_MI)[ c(7:99) ]
num_missing <- before_MI[, lapply(.SD, function(x) sum(is.na(x))), by = date, .SDcols = sd.cols]
sd.cols = colnames(num_missing)[2:length(num_missing)]

percent_missing <- num_missing[, lapply(.SD, function(x) ((x/520)*100)), by = date, .SDcols = sd.cols]
saveRDS(percent_missing, paste0(dir_pnlp, "percent_missing_data_by_date_indicator.rds"))
# ---------------------------------------------------

# ---------------------------------------------------
# DATA PREP - CAN SKIP IF YOU LOAD IN SAVED VERSIONS IN GF\results_chains\cod\malaria\prepped_data\...
# ---------------------------------------------------
# subset to just malaria
snis$type <- trimws(snis$type)  # for some reason there is both "malaria" and "malaria " in the type unique values
snis <- snis[ type == "malaria", ]

# snis - clean var names
snis$element <- trimws(snis$element)
snis$element_eng <- trimws(snis$element_eng)

snis[element== "A 2.1 MILD distribués a la CPN2+", element_eng := "LLIN_distAtANC"]
snis[element== "A 2.1 MILD distribués a la CPN1", element_eng := "LLIN_distAtANC"]
snis[element== "A 1.4 TDR positif", element_eng := "RDT_positive"]
snis[element== "A 1.4 TDR réalisé", element_eng := "RDT_completed"]
snis[element== "A 2.1 Sulfadox. + Pyrimét 1ère dose reçue", element_eng := "SP_1st"]
snis[element== "A 2.1 Sulfadox. + Pyrimét 2ème dose reçue", element_eng := "SP_2nd"]
snis[element== "A 2.1 Sulfadox. + Pyrimét 3ème dose reçue", element_eng := "SP_3rd"]
snis[element== "A 2.1 Sulfadox. + Pyrimét 4ème dose reçue", element_eng := "SP_4th"]

# aggregate SNIS data to be at the national level
snis_natl <- snis[, .(natlValue= sum(value, na.rm=TRUE)), by=c("date", "year", "element", "element_eng", "category")]  
    # unique identifiers at the natl level are date, element/element_eng, and category
saveRDS(snis_natl, base_natl_filepath)

# pnlp data
pnlp$year <- year(pnlp$date)
pnlp[indicator=="ITN", indicator:= "LLIN"]
setnames(pnlp, "mean", "value")
saveRDS(pnlp, pnlp_natl_filepath)

# sigl data
sigl$type <- trimws(sigl$type)  
sigl <- sigl[ type == "malaria", ]
sigl$element <- trimws(sigl$element)
sigl$element_eng <- trimws(sigl$element_eng)

sigl_cons <- sigl[ grepl("consommée", element, ignore.case = FALSE), ]   # subset to just quantity consumed of different things
sigl_cons_natl <- sigl_cons[, .(natlValue= sum(value, na.rm=TRUE)), by=c("date", "year", "element", "element_eng", "category")]  

sigl_cons_natl[element== "C1 12.1 Artesunate-Amodiaquine (+14 ans, 6 cés) 100mg+270mg Comprimé - quantité consommée", 
               element_eng := "ASAQconsumed_14yrsAndOlder"]
sigl_cons_natl[element== "C2 12.2 Artesunate 60mg Injectable - Quantité consommée", 
               element_eng := "ASAQconsumed_inj"]
sigl_cons_natl[element== "C1 12.1 Artesunate 400mg Suppositoire - quantité consommée", 
               element_eng := "ASAQconsumed_supp400"]
sigl_cons_natl[element== "C1 12.1 Artesunate-Amodiaquine (6-13 ans, 3 cés) 100mg+270mg Comprimé - quantité consommée", 
               element_eng := "ASAQconsumed_6to13yrs"]
sigl_cons_natl[element== "C1 12.1 Artesunate-Amodiaquine (2-11 mois) 25mg+67,5mg Comprimé - quantité consommée", 
               element_eng := "ASAQconsumed_2to11mos"]
sigl_cons_natl[element== "C1 12.1 Artesunate-Amodiaquine (12-59 mois) 50mg+135mg Comprimé - quantité consommée", 
               element_eng := "ASAQconsumed_1to5yrs"]
sigl_cons_natl[element== "C1 12.1 Lumefantrine+ Artemether 40mg+240mg Comprimé - quantité consommée", 
               element_eng := "ArtLum_consumed(240+40)"]
sigl_cons_natl[element== "C1 12.1 Lumefantrine+ Artemether 80mg+480mg Comprimé - quantité consommée", 
               element_eng := "ArtLum_consumed(480+80)"]
sigl_cons_natl[element== "C1 12.1 Sulfadoxine + Pyriméthamine 500mg+25mg Cés - quantité consommée", 
               element_eng := "SP_consumed"]
sigl_cons_natl[element== "C1 12.1 Artesunate 200mg Suppositoire - quantité consommée", 
               element_eng := "ASAQconsumed_supp200"]
sigl_cons_natl[element== "C1 12.1 MIILD - pièce - quantité consommée", 
               element_eng := "LLIN_consumed"]

sigl_cons_natl <- sigl_cons_natl[ year >= 2017, ]

saveRDS(sigl_cons_natl, sigl_natl_filepath)
# ---------------------------------------------------

# ---------------------------------------------------
# subset data
# ---------------------------------------------------
pnlp <- pnlp[, .(date, year, indicator, subpopulation, value)]
pnlp$data_source <- "PNLP"
snis_natl$data_source <- "SNIS_base"
sigl_cons_natl$data_source <- "SNIS_sigl"

snis_all <- rbind(snis_natl, sigl_cons_natl)
snis_all <- snis_all[, c("indicator", "subpopulation") := tstrsplit(element_eng, "_", fixed=TRUE)]
setnames(snis_all, "natlValue", "value")
snis_all <- snis_all[, .(date, year, indicator, subpopulation, value, data_source)]

all_data <- rbind(pnlp, snis_all)
saveRDS(all_data, all_data_filepath)
# ---------------------------------------------------

# ---------------------------------------------------
# more data cleaning
# ---------------------------------------------------
all_data[indicator== "A 1.4 Severe malaria", indicator := "newCasesMalariaSevere"]
all_data[indicator== "A 1.4 Severe malaria treated", indicator := "severeMalariaTreated"]

all_data[indicator== "A 1.4 Confirmed simple malaria", indicator := "newCasesMalariaMild"]
all_data[indicator== "A 1.4 Confirmed simple malaria treated", indicator := "mildMalariaTreated"]

all_data[indicator== "A 1.5 Confirmed simple malaria - pregnant woman", c("indicator", "subpopulation") := list("newCasesMalariaMild", "pregnantWomen") ]
all_data[indicator== "A 1.5 Confirmed simple malaria treated - pregnant woman", c("indicator", "subpopulation") := list("mildMalariaTreated", "pregnantWomen") ]

all_data[indicator== "A 1.5 Severe malaria - pregnant woman", c("indicator", "subpopulation") := list("newCasesMalariaSevere", "pregnantWomen") ]
all_data[indicator== "A 1.5 Severe malaria treated - pregnant woman", c("indicator", "subpopulation") := list("severeMalariaTreated", "pregnantWomen") ]

# sum over category variable from the original data set (from snis base there are two rows per )
all_data[, .(value = sum(value, na.rm=TRUE)), by = .(date, year, indicator, subpopulation, data_source)]
saveRDS(all_data, all_data_filepath)
# ---------------------------------------------------

# ---------------------------------------------------
# if starting from here : LOAD DATA
# ---------------------------------------------------
pnlp <- readRDS(pnlp_natl_filepath)
sigl_cons_natl <- readRDS(sigl_natl_filepath)
snis_natl <- readRDS(base_natl_filepath)
all_data <- readRDS(all_data_filepath)
# ---------------------------------------------------

# ---------------------------------------------------
# subset data
# ---------------------------------------------------
# activities
pnlp_act <- pnlp[ indicator %in% c("ASAQreceived", "LLIN", "ArtLum", "RDT"), ]
pnlp_act <- pnlp_act[ subpopulation %in% c("received", "14yrsAndOlder", "1to5yrs", "2to11mos", "6to13yrs"), ]

# outputs
pnlp_out <-  pnlp[ indicator %in% c("ASAQused", "LLIN", "ArtLum", "RDT", "SP") & subpopulation != "received", ]
pnlp_out <-  pnlp_out[ subpopulation != "positive", ]
# ---------------------------------------------------

# ---------------------------------------------------
# Variable naming for facets
# ---------------------------------------------------
facet_names <- c(
  `ArtLum` = "Artemether - Lumefantrine (AL)",
  `ASAQreceived` = "Artesunate - Amodiaquine (ASAQ)",
  `LLIN` = "Long-lasting insecticide-treated nets (LLINs)",
  `RDT` = "Rapid Diagnostic Tests (RDTs)",
  `ACTs_all` = "Artemisinin-based combination therapy (ACT) doses"
)
# ---------------------------------------------------

# ---------------------------------------------------
# graphs
# ---------------------------------------------------
# 1)
# Line graph showing ITNs received, ACTs received, RDTs received, and Injectible+suppository ACTs received (5 lines) over time 
# since the start of the data
  # NOTE: follow up with David on whether we want to show since 2015 like he said originally?
  # Do not have "received" data in SNIS - use calculated values from SIGL if possible
  # Do not have "injectable+suppository ACTs" in PNLP
  
g <- ggplot(pnlp_act, aes(x=date, y=value, color=subpopulation)) + 
  theme_bw() + geom_point() + geom_line() +
  ggtitle(paste0("Activities: Commodities received")) +
  ylab("Count") + xlab("Date") + labs(caption = "Source: Programme National de Lutte contre le Paludisme (PNLP)") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~indicator, scales = "free_y", labeller = as_labeller(facet_names)) +
  scale_y_continuous( label= scales :: comma )
print(g)

pnlp_act_combinedASAQ <- pnlp_act[, .(value = sum(value, na.rm=TRUE)), by=c("date", "year", "indicator")]
pnlp_act_combinedASAQ <- pnlp_act_combinedASAQ[ indicator == "ArtLum" & date < "2015-01-01", value := NA]
dt_wide <- dcast.data.table(pnlp_act_combinedASAQ, date + year ~ indicator) 
dt_wide <- dt_wide[, ACTs_all := rowSums(.SD, na.rm=TRUE), .SDcols = c('ArtLum', 'ASAQreceived')]
pnlp_act_combinedACTs <- melt.data.table(dt_wide, id.vars = c("date", "year"), variable.name = "indicator")
pnlp_act_combinedACTs[ indicator == "LLIN", graph_panel := "1"]
pnlp_act_combinedACTs[ indicator %in% c("RDT", "ACTs_all"), graph_panel := "2"]

g1 <- ggplot(pnlp_act_combinedACTs[ indicator %in% c("LLIN", "RDT", "ACTs_all")], aes(x=date, y=value, color=indicator)) + 
  theme_bw() + geom_point() + geom_line() +
  ggtitle(paste0("Activities: Commodities received")) +
  ylab("Count") + xlab("Date") + labs(caption = "Source: Programme National de Lutte contre le Paludisme (PNLP)
                                      Note: ACT includes doses of ASAQ and AL. No data for AL before 2015.") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~indicator, scales = "free_y", labeller = as_labeller(facet_names)) +
  scale_y_continuous( label= scales :: comma )
print(g1)


#2)
# Line graph showing ITNs distributed, Patients treated, RDTs used, and Patients treated by CHWs, Injectible+suppository ACTs used and ACTs used 
# during ANC (6 lines) over time since1 the start of the data

graph <- all_data[ indicator %in% c("LLIN", "RDT") & !(subpopulation %in% c("received", "positive")), ]

# sum by data source
graph <- graph[ data_source == "PNLP" |  subpopulation %in% c("consumed", "completed") ] 
    # to sum by data source, make sure to remove LLIN_distAtANC from SNIS because that may be counted in SNIS LLIN_consumed already

graph2 <- graph[, .(sum_by_source = sum(value, na.rm = TRUE)), by= .(date, year, indicator, data_source)]

g <- ggplot(graph2, aes(x=date, y=sum_by_source, color=data_source)) + 
  theme_bw() + geom_point() + geom_line() +
  ggtitle(paste0("")) +
  ylab("Date") + xlab("Count") + labs(caption = "") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~indicator, scales = "free_y" ) +
  scale_y_continuous( label= scales :: comma )
print(g)


# ---------------------------------------------------

