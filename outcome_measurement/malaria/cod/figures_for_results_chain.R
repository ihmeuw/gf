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

export_tables_dir = paste0(out_dir, "prepped_data/data_tables_for_figures/")

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
# ---------------------------------------------------

# ---------------------------------------------------
# Calculate number of missing data points ("imputed data points") per year/variable
# ---------------------------------------------------
# load data before MI
before_MI <- read.csv(paste0(dir_pnlp, pnlp_preMI), stringsAsFactors = FALSE)

before_MI <- as.data.table(before_MI)
sd.cols = colnames(before_MI)[ c(7:99) ]
num_missing <- before_MI[, lapply(.SD, function(x) sum(is.na(x))), by = date, .SDcols = sd.cols]
sd.cols = colnames(num_missing)[2:length(num_missing)]

percent_missing <- num_missing[, lapply(.SD, function(x) ((x/520)*100)), by = date, .SDcols = sd.cols]
saveRDS(percent_missing, paste0(dir_pnlp, "percent_missing_data_by_date_indicator.rds"))
percent_missing <- readRDS(paste0(dir_pnlp, "percent_missing_data_by_date_indicator.rds"))
# ---------------------------------------------------

# ---------------------------------------------------
# DATA PREP - CAN SKIP IF YOU LOAD IN SAVED VERSIONS IN GF/results_chains/cod/malaria/prepped_data/...
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
# Subset data
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
# More data cleaning
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
# If starting from here : LOAD DATA
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
  `ACTs_all` = "Artemisinin-based combination therapy (ACT) doses",
  `SP` = "Sulfadoxine-Pyrimethamine (SP) used during 1st ANC visit",
  `SSCACT` = "Doses of ACTs used by CHWs",
  `mildMalariaTreated` = "Patients with uncomplicated malaria treated",
  `severeMalariaTreated` = "Patients with severe malaria treated",
  `prct_treated_mild` = "Percent of new cases of uncomplicated malaria treated",
  `prct_treated_severe` = "Percent of new cases of severe malaria treated",
  `prct_SP_over_ANC` = "Percent of ANC visits with SP administered",
  `deaths_per_100kcases_all` = "Deaths per 100,000 cases",
  `deaths_per_100kcases_maternal` = "Maternal deaths per 100,000 cases",
  `cases_over_lagCases` = "Cases divided by cases one year earlier"
)

# ---------------------------------------------------
# filename = "outputs - commodities distributed"
# pdf(paste0(out_dir, "visualizations/", filename, ".pdf"), height = 12, width = 14)
# print(g)
# dev.off()

# ---------------------------------------------------
# graphs
# ---------------------------------------------------
# 1)
# Line graph showing ITNs received, ACTs received, RDTs received, and Injectible+suppository ACTs received (5 lines) over time 
# since the start of the data
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
  geom_point() + geom_line() +
  ggtitle(paste0("Activities: Commodities received")) +
  ylab("Count") + xlab("Date") + labs(caption = "Source: Programme National de Lutte contre le Paludisme (PNLP)
                                      Note: ACT includes doses of ASAQ and AL. No data for AL before 2015.") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~indicator, scales = "free_y", labeller = as_labeller(facet_names)) +
  scale_y_continuous( label= scales :: comma ) + guides(color = FALSE) + theme_bw( base_size = 18)
print(g1)

saveRDS(pnlp_act_combinedACTs, paste0(export_tables_dir, "figure_1_dt.rds"))
# ---------------------------------------------------
#2)
# Line graph showing ITNs distributed, Patients treated, RDTs used, and Patients treated by CHWs, Injectible+suppository ACTs used and ACTs used 
# during ANC (6 lines) over time since1 the start of the data
  
graph <- all_data[ indicator %in% c("LLIN", "RDT", "severeMalariaTreated", "mildMalariaTreated", "SP", "SSCACT") & !(subpopulation %in% c("received", "positive")), ]

# remove dates where a variable is 100% missing in percent_missing - SSCACT needs to be done before agg across subpops
  # keep SSCACT with subpop = NA for 2015 and 2016, keep with subpops <5 and >5 for 2017
  # just SP_1st
graph <- graph[ data_source == "PNLP" & indicator == "SSCACT" & is.na(subpopulation) & date < "2015-01-01", value := NA]
graph <- graph[ data_source == "PNLP" & indicator == "SSCACT" & is.na(subpopulation) & date > "2016-12-01", value := NA]
graph <- graph[ data_source == "PNLP" & indicator == "SSCACT" & is.na(subpopulation) & date > "2016-12-01", value := NA]
graph <- graph[ data_source == "PNLP" & indicator == "SSCACT" & !is.na(subpopulation) & date < "2017-01-01", value := NA]
graph <- graph[ indicator == "SP" & subpopulation != "1st",  value := NA]

# sum by data source, date, and indicator (over subpopulations)
graph2 <- graph[, .(sum_by_source = sum(value, na.rm = TRUE)), by= .(date, year, indicator, data_source)]

# remove last two months of SNIS data
graph2 <- graph2[ date <= "2018-06-01"]
graph2 <- graph2[ sum_by_source == 0, sum_by_source := NA]

# remove SNIS base data for LLINs
graph2 <- graph2[ data_source== "SNIS_base" & indicator == "LLIN", sum_by_source := NA]

# remove data before 2014 for cases treated
graph2 <- graph2[ indicator %in% c("severeMalariaTreated", "mildMalariaTreated") & date < "2014-01-01", sum_by_source:= NA]
graph2 <- graph2[ data_source %in% c("SNIS_base", "SNIS_sigl"), data_source:="SNIS"]
graph2 <- graph2[!is.na(sum_by_source)]

g <- ggplot(graph2, aes(x=date, y=sum_by_source, color=data_source)) + 
  geom_point() + geom_line() +
  ggtitle(paste0("Outputs: commodities distributed and patients treated")) +
  ylab("Count") + xlab("Date") + labs(caption = "") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~indicator, scales = "free_y", labeller = as_labeller(facet_names) ) +
  scale_y_continuous( label= scales :: comma ) + 
  guides(color = guide_legend(title= "Data Source:")) + theme_bw( base_size = 18) + theme(legend.position = "bottom") 
print(g)

saveRDS(graph2, paste0(export_tables_dir, "figure_2_dt.rds"))
# ---------------------------------------------------
# 3)
# Line graph showing Patients treated divided by cases notified, Severe cases treated divided by severe cases notified and ACTs used during ANC divided by ANC visits (1st visit) 
# (3 lines) over time since the start of the data

# subset
graph3 <- all_data[ indicator %in% c("newCasesMalariaSevere", "newCasesMalariaMild", "severeMalariaTreated", "mildMalariaTreated", "SP", "ANC") 
                    & !(subpopulation %in% c("received", "2nd", "4th", "3rd", "consumed")), ]

# sum over subpopulations
graph3_sum <- graph3[, .(sum_by_source = sum(value, na.rm = TRUE)), by= .(date, year, indicator, data_source)]

# cast wide
graph3_wide <- dcast.data.table(graph3_sum, date + year + data_source ~ indicator, value.var = 'sum_by_source')

# account for outliers in new cases mild (instead of deleting the point and re-running the entire code to get to natl sum, I've just subtracted that point's value from the total here)
graph3_wide[data_source == "SNIS_base" & date == "2017-04-01", newCasesMalariaMild := newCasesMalariaMild - 170172]
graph3_wide[data_source == "SNIS_base" & date == "2017-08-01", newCasesMalariaMild := newCasesMalariaMild - 111111]

# variable calculations
graph3_wide <- graph3_wide[, prct_treated_mild := (mildMalariaTreated / newCasesMalariaMild) *100]
graph3_wide <- graph3_wide[, prct_treated_severe := (severeMalariaTreated / newCasesMalariaSevere) *100]
graph3_wide <- graph3_wide[, prct_SP_over_ANC := (SP / ANC) * 100]

# melt
graph3_long <- melt.data.table(graph3_wide, id.vars = c('date', 'year', 'data_source'), variable.name = "indicator")
graph3_long <- graph3_long[ indicator %in% c("prct_treated_mild", "prct_treated_severe", "prct_SP_over_ANC")]

graph3_long <- graph3_long[indicator %in% c("prct_treated_mild", "prct_treated_severe") & date < "2014-01-01", value:= NA]
graph3_long <- graph3_long[!is.na(value)]
graph3_long <- graph3_long[data_source== "SNIS_base", data_source := "SNIS"]

#remove outlier point
#graph3_long <- graph3_long[data_source == "SNIS" & date == "2017-04-01" & indicator == "prct_treated_mild", value := NA]

g <- ggplot(graph3_long[date <= "2018-06-01"], aes(x=date, y=value, color=data_source)) + 
  geom_point() + geom_line() +
  ggtitle(paste0("Outcomes: ")) +
  ylab("Count") + xlab("Date") + labs(caption = "") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~indicator, scales = "free", labeller = as_labeller(facet_names) ) +
  scale_y_continuous( label= scales :: comma ) + 
  guides(color = guide_legend(title= "Data Source:")) + theme_bw( base_size = 18) + theme(legend.position = "bottom")  
print(g)

# check problems with num / denom > 100 ? 
cols_to_include <- colnames(before_MI)[grepl("severe", colnames(before_MI), ignore.case = TRUE)]
check <- before_MI[, c('date', 'dps', 'health_zone', cols_to_include)]
check <- as.data.table(check)

sd.cols <- colnames(check)[grepl("cases", colnames(check), ignore.case = TRUE)]
check <- check[, all_severe_cases := rowSums(.SD, na.rm=TRUE), by=c('date', 'dps', 'health_zone'), .SDcols = sd.cols]

sd.cols <- colnames(check)[grepl("treated", colnames(check), ignore.case = TRUE)]
check <- check[, all_severe_cases_treated := rowSums(.SD, na.rm=TRUE), by=c('date', 'dps', 'health_zone'), .SDcols = sd.cols]

errors <- check[all_severe_cases_treated > all_severe_cases, ]


saveRDS(graph3_long, paste0(export_tables_dir, "figure_3_dt.rds"))
# ---------------------------------------------------
# 4)
# Line graph showing Deaths divided by cases notified, Maternal deaths (or deaths among reproductive-aged women if necessary) divided by 
# maternal cases notified and Cases notified divided by lag(cases notified) over time since the start of the data

all_data[indicator== "A 1.4 Presumed malaria", indicator := ("presumedMalaria") ]

graph4 <- all_data[ indicator %in% c("newCasesMalariaSevere", "newCasesMalariaMild", "malariaDeaths"), ]
# took out presumedMalaria because of a lot of missingness before 2017

# deaths - all cases 
# subset
deaths <- graph4[data_source == "PNLP",]
# sum over subpopulations
deaths <- deaths[ , .(value = sum( value , na.rm = TRUE)), by=.(date, year, indicator, data_source)]
# cast
deaths <- dcast.data.table(deaths, date + year + data_source ~ indicator)
deaths[, deaths_per_100kcases_all := (malariaDeaths / (newCasesMalariaMild + newCasesMalariaSevere)) * 100000]

# deaths - maternal cases
maternal_deaths <- graph4[data_source == "PNLP" & subpopulation == "pregnantWomen"]
maternal_deaths <- dcast.data.table(maternal_deaths, date + year + data_source ~ indicator)
maternal_deaths[, deaths_per_100kcases_maternal := (malariaDeaths / (newCasesMalariaMild + newCasesMalariaSevere )) * 100000]

# cases over lag(cases)
cases <- graph4[ indicator != "malariaDeaths", ]
# sum over subpopulations
cases <- cases[ , .(all_malaria_cases = sum( value , na.rm = TRUE)), by=.(date, year, data_source)]
# order by date
cases <- setorder(cases, data_source, date)
# create a variable for lag cases
cases <- cases[, lag_cases := data.table::shift(all_malaria_cases, n = 12L, type = "lag"), by = "data_source"] # same month, year before
cases <- cases[, cases_over_lagCases := all_malaria_cases / lag_cases]

# combine
graph4 <- merge(deaths, maternal_deaths, by = c('date', 'year','data_source'), all = TRUE)
graph4 <- merge(graph4, cases, by = c('date', 'year','data_source'), all = TRUE)
graph4 <- graph4[, .(date, year, data_source, deaths_per_100kcases_all, deaths_per_100kcases_maternal, cases_over_lagCases)]

# melt long
graph4 <- melt.data.table(graph4, id.vars= c("date", "year", "data_source"), variable.name = "indicator")
graph4 <- graph4[data_source == "SNIS_base" & date > "2018-06-01", value := NA]

graph4_subset <- graph4[indicator %in% c("deaths_per_100kcases_all", "deaths_per_100kcases_maternal", "cases_over_lagCases")]
graph4_subset[data_source == "SNIS_base", data_source:= "SNIS"]

g <- ggplot(graph4_subset, aes(x=date, y=value, color=data_source)) + 
  geom_point() + geom_line() +
  ggtitle(paste0("Outcomes: ")) +
  ylab("Count") + xlab("Date") + labs(caption = "") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_text(size=16), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=14)) +
  facet_wrap( ~indicator, scales = "free", labeller = as_labeller(facet_names) ) +
  #scale_y_continuous( label= scales :: comma ) + 
  guides(color = guide_legend(title= "Data Source:")) + theme_bw( base_size = 26) + theme(legend.position = "bottom")  
print(g)

out_dir_offJ = "C:/Users/abatzel/Desktop/malaria_results_chain_copy/added_vis_off_J/" 
pdf( paste0(out_dir_offJ, "figure4_edited.pdf"), height = 9, width = 20)
print(g)
dev.off()

saveRDS(graph4, paste0(export_tables_dir, "figure_4_dt.rds"))
