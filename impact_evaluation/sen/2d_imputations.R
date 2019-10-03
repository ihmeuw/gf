# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: 2d. Use multiple-imputation to fill in missing values for Senegal TB
# DATE: September 25, 2019
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------


source('./impact_evaluation/sen/set_up_r.r')

# load data
DT <- readRDS(outputFile2a)


# split into two dataframes: hospital and district level to perform imputation on the correct variables

hospital <- DT[type=="HOPITAL",.(region, centre, date, type, tb_tfc, ntr_cpx, tot_genexpert, tot_res, 
                  gueris_total, gueris_taux)]

district <- DT[type=="DISTRICT",.(region, centre, date, type, com_mobsoc, com_cause, com_radio, com_enf_ref, com_nom_touss, tb_tfc,
                  perf_lab, ntr_rhz, ntr_erhz, ntr_serhz, ntr_cpx, tot_genexpert, tot_res, tbtot_taux_det, 
                  gueris_total, gueris_taux, tb_vih_arv, tpm_chimio_enf, tpm_chimio_pvvih, tb_vih2)]

# IMPUTATIONS ON EACH DATASET
h.out <- amelia(hospital, m=1, ts='date', idvars = c('region', 'centre', 'type'))

d.out <-amelia(district, m=1, ts='date', idvars = c('region', 'centre', 'type'))

# extract datable from each of the imputation rounds
DTh <- h.out$imputations[[1]]
DTd <- d.out$imputations[[1]]

# data cleaning
# remove negative values and replace with zeros
DTh$tot_genexpert[which(DTh$tot_genexpert<0)] <- 0
DTh$tot_res[which(DTh$tot_res<0)] <- 0
DTh$gueris_taux[which(DTh$gueris_taux<0)] <- 0

DTd$tot_genexpert[which(DTd$tot_genexpert<0)] <- 0
DTd$tpm_chimio_pvvih[which(DTd$tpm_chimio_pvvih<0)] <- 0
DTd$tpm_chimio_enf[which(DTd$tpm_chimio_enf<0)] <- 0
DTd$ntr_rhz[which(DTd$ntr_rhz<0)] <- 0
DTd$ntr_erhz[which(DTd$ntr_erhz<0)] <- 0
DTd$ntr_serhz[which(DTd$ntr_serhz<0)] <- 0
DTd$tot_res[which(DTd$tot_res<0)] <- 0
DTd$com_mobsoc[which(DTd$com_mobsoc<0)] <- 0
DTd$com_cause[which(DTd$com_cause<0)] <- 0
DTd$com_radio[which(DTd$com_radio<0)] <- 0
DTd$com_enf_ref[which(DTd$com_enf_ref<0)] <- 0
DTd$com_nom_touss[which(DTd$com_nom_touss<0)] <- 0

# round decimals to whole counts
DTh$tot_genexpert <- round(DTh$tot_genexpert)
DTh$tot_res <- round(DTh$tot_res)

DTd$com_mobsoc <- round(DTd$com_mobsoc)
DTd$com_cause <- round(DTd$com_cause)
DTd$com_radio <- round(DTd$com_radio)
DTd$com_enf_ref <- round(DTd$com_enf_ref)
DTd$tpm_chimio_enf <- round(DTd$tpm_chimio_enf)
DTd$com_nom_touss <- round(DTd$com_nom_touss)
DTd$ntr_erhz <- round(DTd$ntr_erhz)
DTd$ntr_serhz <- round(DTd$ntr_serhz)
DTd$ntr_rhz <- round(DTd$ntr_rhz)
DTd$tot_genexpert <- round(DTd$tot_genexpert)
DTd$tpm_chimio_pvvih <- round(DTd$tpm_chimio_pvvih)

# merge back together
merge_file <- rbind(DTh, DTd, fill=TRUE)

# save
saveRDS(merge_file, outputFile2d)

# save a time-stamped version for reproducibility
archive(outputFile2d)
                  