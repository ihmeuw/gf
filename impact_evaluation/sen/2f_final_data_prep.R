# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: 3. Merge prepped data for Sen TB Model
# DATE: Auguts 15, 2019
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

source('./impact_evaluation/sen/set_up_r.r')

# ----------------------------------------------------------
# Load and subset the data
# ----------------------------------------------------------

# subset data into the different datasets
all.data <- readRDS(outputFile2d)
hospital.data <- all.data[type=='HOPITAL']
district.data <- all.data[type=='DISTRICT']
community.data <- all.data[type=='COMMUNITY']
tb_mdr <- readRDS(outputFile2e)

# prep each variable according to data source and level reported

# HOSPITAL AND DISTRICT variables WHICH CAN BE SUMMED (counts)
datatemp1 = all.data[, lapply(.SD, sum, na.rm=TRUE), 
                          by=c('region','date'), 
                          .SDcols=c('tb_tfc', 'tot_genexpert','tot_res','gueris_total','tb_vih_arv', 'tb_vih')]

# VALUES WHICH ARE ONLY REPORTED BY DISTRICT AND CAN BE SUMMED
datatemp2 = district.data[, lapply(.SD, sum, na.rm=TRUE), 
                          by=c('region','date'), 
                          .SDcols=c('tpm_chimio_enf')]

# DISTRICT VALUES WHICH MUST BE AVERAGED (percents or rates)
datatemp3 = district.data[, lapply(.SD, mean, na.rm=TRUE), 
                          by=c('region','date'), 
                          .SDcols=c('perf_lab')]

# COMMUNITY DATA (WHICH is Reported in annual totals) and must be divided by 4
datatemp4 = community.data[, lapply(.SD, function(x){x*0.25}), 
                      by=c('region', 'annee'), 
                      .SDcols=c('com_mobsoc', 'com_cause', 'com_radio', 'com_enf_ref', 'com_nom_touss', 'com_vad_touss')]

# create four dataframes one for each quarter
datatemp41 <- datatemp4
datatemp42 <- datatemp4
datatemp43 <- datatemp4
datatemp44 <- datatemp4

# create four quarter variables
datatemp41$quarter <- 0
datatemp42$quarter <- 0.25
datatemp43$quarter <- 0.5
datatemp44$quarter <- 0.75

# bind together
combind1 <- rbind(datatemp41, datatemp42)
combind2 <- rbind(combind1, datatemp43)
combind3 <- rbind(combind2, datatemp44)

# add date variable
combind3[,date:=quarter+annee]
# delete year and quarter
datatemp5 = combind3[,c("annee","quarter"):=NULL]

# DISTRICT AND HOSPITAL VALUES WHICH MUST BE AVERAGED (percents or rates)
datatemp6 = all.data[, lapply(.SD, mean, na.rm=TRUE), 
                          by=c('region','date'), 
                          .SDcols=c('gueris_taux')]

# MERGE different data sets together
merge1 <- merge(datatemp1, datatemp2, by = c("region", "date"), all = TRUE)
merge2 <- merge(merge1, datatemp3, by = c("region", "date"), all = TRUE)
merge3 <- merge(merge2, datatemp5, by = c("region", "date"), all = TRUE)
DT1 <- merge(merge3, datatemp6, by = c("region", "date"), all = TRUE)

#----------------------------------------------------------------------------------
# Merge TB-MDR data to other outcomes data
# ---------------------------------------------------------------------------------

# Get counts using datatable of treated and diagnosed
# change values to date

datatemp6 <- tb_mdr[,date_dx:=as.Date(tb_mdr$date_diag, "%m/%d/%Y")]
datatemp6 <- datatemp6[,date_tx:=as.Date(tb_mdr$date_trait, "%m/%d/%Y")]

# Generate counts of MDR-TB cases diagnoses by region and quarter
datatemp6$dx_count <- 1

# use either the date of treatment or date of diagnosis to assign values
datatemp6$date_either <- ifelse(is.na(datatemp6$date_dx), datatemp6$date_tx, datatemp6$date_dx)

# restore the date attribute
class(datatemp6$date_either) <- class(datatemp6$date_tx)

# count how many cured
datatemp6$mdr_success <- 0
datatemp6$mdr_success[which(datatemp6$resultat=="Gueris")] <- 1

# subset to necessary data
datatemp6 <- datatemp6[,.(region, date_either, dx_count, mdr_success)]

# add quarter and year information
datatemp6[, quarter:=quarter(date_either)]
datatemp6[, year:=year(date_either)]
datatemp6[, date_either:=NULL]

#Create date variable
datatemp6[, quarter:=(quarter/4)-0.25] #Q1 should be .00, Q2 should be .25, etc. 
datatemp6[, date:=year+quarter]

# Calcuate how many diagnosed and how many cured
datatemp6 <- datatemp6[,lapply(.SD, sum), by=c('region', 'date'),.SDcols=c("dx_count", "mdr_success")]

# add in data for kedougou region
datatemp7 <- datatemp6[region=="TAMBACOUNDA"]
datatemp7$region[which(datatemp7$region=="TAMBACOUNDA")] <- "KEDOUGOU"
DT2 <- rbind(datatemp6, datatemp7)

# merge tb_mdr_data to other outputs data
outputs_prepped <- merge(DT1, DT2, by=c('region', 'date'), all=TRUE)

# -------------------------------------------------
# Create derived variables on new dataset
# -------------------------------------------------

# create variable of cases identified by other means
outputs_prepped$tb_cas_id <- outputs_prepped$tb_tfc - outputs_prepped$tot_genexpert
outputs_prepped$tb_cas_id[which(outputs_prepped$tb_cas_id<0)] <- 0

# create variable of mdr-tb treatment success rate
outputs_prepped$mdr_success_rate <- outputs_prepped$mdr_success / outputs_prepped$dx_count

# ----------------------------------------------------------

outputs_prepped = na.omit(outputs_prepped, cols=c('region', 'date'))
outputs_prepped$tb_tfc <- as.numeric(outputs_prepped$tb_tfc)

# Finish and save 

# save
saveRDS(outputs_prepped, outputFile2f)

# save a time-stamped version for reproducibility
archive(outputFile2f)
# ----------------------------------------------------------
