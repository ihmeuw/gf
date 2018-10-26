# ----------------------------------------------
# Audrey Batzel
# 10/18/18
# Descriptive analysis of drugs consumed vs. cases notified using PNLT and SNIS data
# ----------------------------------------------
## Set up R / install packages 
# ----------------------------------------------
rm(list=ls())
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(maptools)
library(grid)
library(gridExtra)
setwd('C:/local/gf/')
source('./core/standardizeDPSnames_function.R')
# ----------------------------------------------


# ----------------------------------------------
## set up the directories to read/export files: 
# ----------------------------------------------
# Directories
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j/')
output_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLT_data/')
data_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/")
shape_dir <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/gadm36_COD_shp/")
snis_path <- "J:/Project/Evaluation/GF/outcome_measurement/cod/dhis/prepped/"

# input files
pnlt_case_screening_17 <- "PNLT_case_screening_2017.csv"
pnlt_case_screening_18 <- "PNLT_case_screening_2018.csv"
dps_shapefile <-  "gadm36_COD_1.shp"
prepped_drug_data <- "drugs_consumed_lost_available.rds"

# output files

# ----------------------------------------------


# ----------------------------------------------
## load data
# ----------------------------------------------
dt17 <- read.csv(paste0(data_dir, pnlt_case_screening_17), stringsAsFactors = FALSE)
dt17 <- as.data.table(dt17)

dt18 <- read.csv(paste0(data_dir, pnlt_case_screening_18), stringsAsFactors = FALSE)
dt18 <- as.data.table(dt18)

drcShape <- shapefile(paste0(shape_dir, dps_shapefile)) # shapefile with all DPS (use a different one for health zones)

dt_snis <- readRDS(paste0(snis_path, prepped_drug_data))
# ----------------------------------------------


# ----------------------------------------------
# prep and combine pnlt data
# ----------------------------------------------
#---- changed in pnlt_translations doc(done!), rerun prep code to change in prepped data-
setnames(dt18, "ziehl_comp", "tbPresumed_ziehl_comp")
setnames(dt18, "ziehl_pos", "tbPresumed_ziehl_pos")
setnames(dt18, "ziehl_comp.1", "ziehl_comp")
setnames(dt18, "ziehl_pos.1", "ziehl_pos")

dt <- rbindlist(list(dt17, dt18), use.names=TRUE, fill= TRUE)
dt$date <- as.Date(dt$date)

# subset to just case notifications for ease
id_vars <- c("dps", "sheet", "quarter", "data_year", "file_year", "date")
dt <- dt[, c(id_vars, "tot_case"), with=FALSE]

# combine kongo-central-est and kongo-central-ouest into kongo-central
dt[ dps %in% c("kongo-central-est", "kongo-central-ouest"), dps:= "kongo-central"]
# sum vars to add kongo-central-est and kongo-cental-ouest together

dt <- dt[, tot_case := sum(tot_case), by = id_vars]
dt <- unique(dt)

dt$dps <- standardizeDPSnames(dt$dps)
# ----------------------------------------------


# ----------------------------------------------
# prepare snis data for merge
# ----------------------------------------------
# snis data is more accurate from 2017 on for drc
dt_snis <- dt_snis[ date >= "2017-01-01" & date <= "2018-03-01"]
dt_snis[, element:= NULL]
dt_snis[, element_eng:= NULL]
dt_snis[, element_id:= NULL]
dt_snis[, tableau:= NULL]

all_vars <- colnames(dt_snis)
vars_for_cast <- all_vars[!all_vars %in% c("variable", "value")]

# create formula for cast
f <- as.formula(paste(paste(vars_for_cast, collapse = " + "), "~ variable"))

# cast variable wide so we can add/subtract vars
dt_snis_wide <- dcast.data.table(dt_snis, f, value.var = "value")

# aggregate to dps values
sd_cols = c("available", "consumed", "lost")
dps_level_snis <- dt_snis_wide[, lapply(.SD, sum, na.rm=TRUE), by=c("date", "dps", "drug", "category", "type"), .SDcols = sd_cols]
dps_level_snis$dps <- standardizeDPSnames(dps_level_snis$dps)

# PNLT data is quarterly, so combine months for snis into quarters
dps_level_snis$date <- as.character(dps_level_snis$date)
dps_level_snis[date %in% c("2017-01-01", "2017-02-01", "2017-03-01"), date:= "2017-01-01"] #quarter 1 (matches to pnlt this way)
dps_level_snis[date %in% c("2017-04-01", "2017-05-01", "2017-06-01"), date:= "2017-04-01"] #quarter 2, etc...
dps_level_snis[date %in% c("2017-07-01", "2017-08-01", "2017-09-01"), date:= "2017-07-01"]
dps_level_snis[date %in% c("2017-10-01", "2017-11-01", "2017-12-01"), date:= "2017-10-01"]
dps_level_snis[date %in% c("2018-01-01", "2018-02-01", "2018-03-01"), date:= "2018-01-01"]
dps_level_snis$date <- as.Date(dps_level_snis$date)

# sum over the data so it's collapsed on date
dps_level_snis <- dps_level_snis[, lapply(.SD, sum, na.rm=TRUE), by=c("date", "dps", "drug", "category", "type"), .SDcols = sd_cols]
dps_level_snis <- unique(dps_level_snis)

dt_snis <- dps_level_snis[type == "tb", ]  # snis data on tb drugs by dps, by quarter, and by drug
dt_snis <- dt_snis[,.(date, dps, drug, consumed, available, lost)]
# ----------------------------------------------


# ----------------------------------------------
# Merge two data sources
# ----------------------------------------------
merged <- merge(dt, dt_snis, by=c("date", "dps"))

# get year totals
year_totals_snis <- dt_snis[date<"2018-01-01"]
sd_cols = c("available", "consumed", "lost")
year_totals_snis <- year_totals_snis[, lapply(.SD, sum, na.rm=TRUE), by=c("dps", "drug"), .SDcols = sd_cols]

year_totals_pnlt <- dt[date<"2018-01-01"]
year_totals_pnlt <- year_totals_pnlt[, .(total_cases_2017 = sum(tot_case)), by=c("dps", "data_year")]

merged_year_total <- merge(year_totals_pnlt, year_totals_snis, by="dps")
# ----------------------------------------------


# ----------------------------------------------
# prep the shapefile: fortify the shapefile to get coordinates and convert it to a data table / standardize dps names for merging
# ----------------------------------------------
coordinates = as.data.table(fortify(drcShape, region='NAME_1'))
coordinates$id <- standardizeDPSnames(coordinates$id)
# this name doesn't save properly so you may have to re-copy and paste the name from the shapefile
coordinates$id <- gsub("ã???equateur", "equateur", coordinates$id)
# ----------------------------------------------


# ----------------------------------------------
# scatterplot tot cases against each type of drug consumed
# ----------------------------------------------
plot <- ggplot(merged, aes(x=tot_case, y=consumed, color=dps)) + theme_bw()+
  geom_point() +
  facet_wrap(~drug) +
  ggtitle("Notified cases vs. drugs consumed by quarter \n(2017 and first quarter 2018)")
plot  

  # labs(caption= "Source: Programme National de Lutte contre le Paludisme (PNLP)") +
  # theme(axis.text=element_text(size=14),axis.title=element_text(size=16),  legend.title=element_blank(), 
  #       legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12, vjust = 0))

merged_year_total$consumed <- as.numeric(merged_year_total$consumed)
merged_year_total$total_cases_2017 <- as.numeric(merged_year_total$total_cases_2017)

plot2 <- ggplot(merged_year_total[dps != "kinshasa" & drug %in% c('RH 150mg+75mg','RHZE')], aes(x=total_cases_2017, y=consumed, color=dps)) + theme_bw()+
  geom_point(size=4) +
  geom_text(data=merged_year_total[drug %in% c('RH 150mg+75mg','RHZE') & dps %in% c("haut-katanga", "nord-kivu", "kasai-oriental"),], aes(label=dps)) + 
  facet_wrap(~drug) +
  ggtitle("2017 Total notified cases vs. total drugs consumed")+
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14),  legend.title=element_blank(),
        legend.text =element_text(size=12), plot.title = element_text(size=18), plot.caption = element_text(size=12, vjust = 0)) +
  guides(color= FALSE)
plot2



# ----------------------------------------------




