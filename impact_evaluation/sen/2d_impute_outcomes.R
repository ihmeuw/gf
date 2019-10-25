# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: 2d. Use multiple-imputation to fill in missing values for Senegal TB
# DATE: September 25, 2019
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------


source('./impact_evaluation/sen/set_up_r.r')

# load data
DT <- readRDS(outputFile2a) # Outcome data


# split into three dataframes: hospital, district level, and community level to perform imputation on the correct variables

# hospital level variables
hospital <- DT[type=="HOPITAL",.(region, centre, date, type, tb_tfc, ntr_cpx, tot_genexpert, tot_res, gueris_total, gueris_taux, tb_vih, tb_vih_arv)]
# logit transformations won't work if values are 0 and 1
hospital$gueris_taux[which(hospital$gueris_taux==0)] <- 0.001
hospital$gueris_taux[which(hospital$gueris_taux==1)] <- 0.999


# district level variables
district <- DT[type=="DISTRICT",.(region, centre, date, type, tb_tfc, perf_lab, ntr_cpx, tot_genexpert, tot_res, 
                  gueris_total, gueris_taux, tb_vih_arv, tpm_chimio_enf, tpm_chimio_pvvih, tb_vih)]
district$gueris_taux[which(district$gueris_taux==0)] <- 0.001
district$gueris_taux[which(district$gueris_taux==1)] <- 0.999
district$perf_lab[which(district$perf_lab==0)] <- 0.001
district$perf_lab[which(district$perf_lab==1)] <- 0.999

# community level variables (reported annually only)
community = DT[, lapply(.SD, mean), by=c('region', 'annee'), .SDcols=c('com_mobsoc', 'com_cause', 'com_radio', 'com_enf_ref', 'com_nom_touss', 'com_vad_touss')]
# other variables added to annual level to aid in imputation
annual.com <- DT[, lapply(.SD, sum), by=c('region', 'annee'), .SDcols=c('tb_tfc','ntr_cpx', 'tot_genexpert', 
                                                                        'gueris_total', 'tb_vih_arv','tb_vih')]
community <- merge(community, annual.com)

# IMPUTATIONS ON EACH DATASET
h.out <- amelia(hospital, m=1, ts='date', idvars = c('region', 'centre', 'type'), 
                sqrts = c('tb_tfc', 'ntr_cpx', 'tot_genexpert', 'tot_res', 'gueris_total', 'tb_vih', 'tb_vih_arv'), 
                lgstc = c('gueris_taux'))

d.out <-amelia(district, m=1, ts='date', idvars = c('region', 'centre', 'type'), 
               sqrts = c('tb_tfc', 'ntr_cpx', 'tot_genexpert', 'tot_res', 'gueris_total', 'tb_vih_arv', 'tpm_chimio_enf', 'tpm_chimio_pvvih', 'tb_vih'), 
               lgstc = c('gueris_taux', 'perf_lab'))

c.out <- amelia(community, m=1, ts='annee', idvars = c('region'), sqrts = c('com_mobsoc', 'com_cause', 'com_radio', 'com_enf_ref', 'com_nom_touss', 'com_vad_touss', 'tb_tfc', 'ntr_cpx', 'tot_genexpert', 'gueris_total', 'tb_vih_arv', 'tb_vih'))


# extract datable from each of the imputation rounds
DTh <- h.out$imputations[[1]]
DTd <- d.out$imputations[[1]]
DTc <- c.out$imputations[[1]]

# add identifier for community level data
DTc <- DTc[,.(region, annee, com_mobsoc, com_cause, com_radio, com_enf_ref, com_nom_touss, com_vad_touss)]
DTc$type <- 'COMMUNITY'

# store seperate imputated datasets in a list
hdc_list <- list(DTh, DTd, DTc)
names(hdc_list) <- c("Hospital", "District", "Community")

# save
saveRDS(hdc_list, outputFile2d)

# save a time-stamped version for reproducibility
archive(outputFile2d)
                  