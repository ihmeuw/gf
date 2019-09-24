# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: To clean Senegal TB data (called "PANEL PNT TB-MR final_01072019")
# DATE: August 14 2019

# INSTRUCTIONS: Set working directory to the root of this repo
# ----------------------------------------------------------

# set-up
source('./impact_evaluation/sen/set_up_r.r')

# read in data
# CSV file encoded in UTF-8 will preserve the accent marks
DT <- fread(raw_data_file, encoding = "UTF-8")

# ------------------------------
# remove rows and columns
# ------------------------------

# remove unecessary columns at end
DT[,137:319:=NULL]

# remove rows that present summed values
DT <- DT[ !(DT$Centre %in% c("TOTAL TRIMESTRE", "TOTAL ANNEE")),]
DT <- DT[ !(DT$REGION %in% c("SENEGAL"))]

# remove bottom row which doesn't contain year or quarter
DT <- na.omit(DT, cols=c(4:5), invert = FALSE)

# ------------------------------
# rename headers
# ------------------------------
codes <- fread(data_codebook, encoding = "UTF-8")
names(DT) <- codes[,Code]

# ------------------------------
# clean data
# ------------------------------
# remove whitespace from numbers
DT[, pop:= trimws(pop)]
DT[, com_cause:= trimws(com_cause)]
DT[, tb_diag_nc := trimws(tb_diag_nc)]
DT[, tb_diag_nc_b := trimws(tb_diag_nc_b)]
DT[, exstock1 := trimws(exstock1)]
DT[, exstock2 := trimws(exstock2)]

# remove commas (,)
DT$pop <- gsub(",","",DT$pop)
DT$tpm_plus_attendus <- gsub(",","",DT$tpm_plus_attendus)
DT$tbtot_taux_det <- gsub(",","",DT$tbtot_taux_det)
DT$gueris_total <- gsub(",","",DT$gueris_total)
DT$trait_pc <- gsub(",","",DT$trait_pc)
DT$com_vad_touss <- gsub(",", "", DT$com_vad_touss)
DT$tb_diag_nc <- gsub(",", "", DT$tb_diag_nc)
DT$tb_diag_nc_b <- gsub(",", "", DT$tb_diag_nc_b)
DT$exstock1 <- gsub(",","", DT$exstock1)
DT$exstock2 <- gsub(",","", DT$exstock2)
DT$com_cause <- gsub(",","", DT$com_cause)
DT$vad_tot <- gsub(",", "", DT$vad_tot)
DT$tbtot_cas_atendu <- gsub(",", "", DT$tbtot_cas_atendu)
DT$com_nom_touss <- gsub(",", "", DT$com_nom_touss)

# remove dashes (-)
DT$pop <- gsub("-","",DT$pop)
DT$tpm_plus_attendus <- gsub("-","",DT$tpm_plus_attendus)
DT$tbtot_taux_det <- gsub("-","",DT$tbtot_taux_det)
DT$gueris_total <- gsub("-","",DT$gueris_total)
DT$trait_pc <- gsub("-","",DT$trait_pc)
DT$retr_tarv <- gsub("-","",DT$retr_tarv)
DT$retr_tbvih <- gsub("-","",DT$retr_tbvih)
DT$retr_vihantitb <- gsub("-","",DT$retr_vihantitb)
DT$retr_cotrimox <- gsub("-","",DT$retr_cotrimox)
DT$tpm_chimio_enf <- gsub("-","",DT$tpm_chimio_enf)
DT$tpm_chimio_pvvih <- gsub("-","",DT$tpm_chimio_pvvih)
DT$tb_diag_nc <- gsub("-", "", DT$tb_diag_nc)
DT$tb_diag_nc_b <- gsub("-", "", DT$tb_diag_nc_b)
DT$tb_diag_rech <- gsub("-", "", DT$tb_diag_rech)
DT$tb_diag_rech_b <- gsub("-", "", DT$tb_diag_rech_b)
DT$dur_rupt1 <- gsub("-", "", DT$dur_rupt1)
DT$dur_rupt2 <- gsub("-", "", DT$dur_rupt2)
DT$dur_rupt3 <- gsub("-", "", DT$dur_rupt3)
DT$com_radio <- gsub("-", "", DT$com_radio)
DT$com_enf_ref <- gsub("-", "", DT$com_enf_ref)
DT$exstock1 <- gsub("-", "", DT$exstock1)
DT$exstock2 <- gsub("-", "", DT$exstock2)
DT$vad_tot <- gsub("-", "", DT$vad_tot)
DT$com_vad_irr <- gsub("-", "", DT$com_vad_irr)
DT$com_vad_pdv <- gsub("-", "", DT$com_vad_pdv)
DT$com_det_ttf <- gsub("-", "", DT$com_det_ttf)
DT$com_suivis <- gsub("-", "", DT$com_suivis)

# remove percent signs
DT$tbtot_taux_det <- gsub("%","",DT$tbtot_taux_det)
DT$trait_pc <- gsub("%", "",DT$trait_pc)
DT$gueris_taux <- gsub("%", "", DT$gueris_taux)
DT$deces_pc <- gsub("%", "", DT$deces_pc)
DT$echec_pc <- gsub("%","", DT$echec_pc)
DT$pdv_pc <- gsub("%","", DT$pdv_pc)
DT$eval_pc <- gsub("%", "", DT$eval_pc)
DT$noneval_pc <- gsub("%", "", DT$eval_pc)
DT$tpm_plus_tauxdet <- gsub("%", "", DT$tpm_plus_tauxdet)
DT$perf_lab <- gsub("%", "", DT$perf_lab)
DT$pauvrety <- gsub("%", "", DT$pauvrety)

# convert values to numeric
DT[,pop:=as.numeric(pop)]
DT[,tpm_plus_attendus:=as.numeric(tpm_plus_attendus)]
DT[,tbtot_taux_det:=as.numeric(tbtot_taux_det)]
DT[,gueris_total:=as.numeric(gueris_total)]
DT[,trait_pc:=as.numeric(trait_pc)]
DT[,gueris_taux:=as.numeric(gueris_taux)]
DT[,deces_pc:=as.numeric(deces_pc)]
DT[,echec_pc:=as.numeric(echec_pc)]
DT[,pdv_pc:=as.numeric(pdv_pc)]
DT[,noneval_pc:=as.numeric(noneval_pc)]
DT[,eval_pc:=as.numeric(eval_pc)]
DT[,com_cause:=as.numeric(com_cause)]
DT[,com_radio:=as.numeric(com_radio)]
DT[,com_vad_touss:=as.numeric(com_vad_touss)]
DT[,com_enf_ref:=as.numeric(com_enf_ref)]
DT[, retr_tarv:=as.numeric(retr_tarv)]
DT[,retr_tbvih :=as.numeric(retr_tbvih)]
DT[, retr_vihantitb:=as.numeric(retr_vihantitb)]
DT[, retr_cotrimox:=as.numeric(retr_cotrimox)]
DT[, tpm_chimio_enf:=as.numeric(tpm_chimio_enf)]
DT[, tpm_chimio_pvvih:=as.numeric(tpm_chimio_pvvih)]
DT[, tb_diag_nc:=as.numeric(tb_diag_nc)]
DT[, tb_diag_nc_b:=as.numeric(tb_diag_nc_b)]
DT[, dur_rupt1 :=as.numeric(dur_rupt1)]
DT[, dur_rupt2:=as.numeric(dur_rupt2)]
DT[, dur_rupt3:=as.numeric(dur_rupt3)]
DT[, exstock1:=as.numeric(exstock1)]
DT[, exstock2:=as.numeric(exstock2)]
DT[, vad_tot:=as.numeric(vad_tot)]
DT[, tb_diag_rech:=as.numeric(tb_diag_rech)]
DT[, tb_diag_rech_b:=as.numeric(tb_diag_rech_b)]
DT[, tpm_plus_tauxdet:=as.numeric(tpm_plus_tauxdet)]
DT[, com_vad_irr:=as.numeric(com_vad_irr)]
DT[, com_vad_pdv:=as.numeric(com_vad_pdv)]
DT[, com_nom_touss:=as.numeric(com_nom_touss)]
DT[, com_det_ttf:=as.numeric(com_det_ttf)]
DT[, com_suivis:=as.numeric(com_suivis)]
DT[, perf_lab:=as.numeric(perf_lab)]
DT[, pauvrety:=as.numeric(pauvrety)]
DT[, tbtot_cas_atendu:=as.numeric(tbtot_cas_atendu)]

# convert percent whole numbers to decimals
DT[, tbtot_taux_det:=tbtot_taux_det/100]
DT[, trait_pc:=trait_pc/100]
DT[, gueris_taux:=gueris_taux/100]
DT[, deces_pc:=deces_pc/100]
DT[, echec_pc:=echec_pc/100]
DT[, pdv_pc:=pdv_pc/100]
DT[, struct_micros_pc:=struct_micros_pc/100]
DT[, struct_xray_pc:= struct_xray_pc/100]
DT[, struct_tbmedprem_pc:= struct_tbmedprem_pc/100]
DT[, struct_tbstrepto_pc:= struct_tbstrepto_pc/100]
DT[, tpm_plus_tauxdet:=tpm_plus_tauxdet/100]
DT[, perf_lab:=perf_lab/100]
DT[, pauvrety:=pauvrety/100]

# fix typo where some hospitals aren't identified as hospitals
DT$type[which(DT$centre=='HEAR')] <- "HOPITAL"
DT$type[which(DT$centre=='H,Fann M Infect')] <- "HOPITAL"
DT$type[which(DT$centre=='HALD')] <- "HOPITAL"
DT$type[which(DT$centre=='HOGGY')] <- "HOPITAL"
DT$type[which(DT$centre=='HPD')] <- "HOPITAL"

# add date variable to dataset
DT = DT[, date:=annee+((trimestre-1)/4)]

# -------------------------------------------------
# Use imputation to fill in missing values
# -------------------------------------------------
library(amelia)

# subest variables to keep for model
DT <- DT[,.(region, centre, date, type, tb_tfc, ntr_cpx, gueris_total, trait_tot, tb_vih_arv, gueris_taux, trait_pc, tbtot_taux_det,
   com_mobsoc, com_cause, com_radio, com_enf_ref, com_nom_touss,
   perf_lab,
   ntr_rhz, ntr_erhz, ntr_serhz,
   tot_genexpert, tot_confirme, tot_res,
   tpm_chimio_enf, tpm_chimio_pvvih)]

a.out <-amelia(DT, m=5, ts='date', idvars = c('region', 'centre', 'type'))

# -------------------------------------------------
# Create derived variables
# -------------------------------------------------

# create combined total treatment variable
#a.out$ntr_all <- NA  
#lapply(a.out, function(x) lappy(x, rowSums((ntr_rhz, ntr_erhz, ntr_serhz))
#rowSums(a.out$imputations$imp1[ntr_rhz, ntr_erhz])

# create variable of tbvih arv treatment rate
#DT$tbvih_arvtx_rate <- DT$tb_vih_arv/DT$tb_vih2
#DT$tbvih_arvtx_rate[which(DT$tb_vih2==0)]<-NA

#-----------------------------------------------------
# Check to make sure you're still uniquely identifying data 
#-----------------------------------------------------
DT[duplicated(DT, by=c('region','centre', 'annee', 'trimestre')), dup:=TRUE]
if (nrow(DT[dup==TRUE])!=0){
  print(paste0("There are ", nrow(DT[dup==TRUE]), " duplicates in region, centre, year, and quarter in the outcomes data. Review."))
}

DT = DT[, -c('dup')]

# -----
# imputation
# -----
library(Amelia)
a.out <-amelia(DT[,.()], idvars=c('region', 'centre', 'type', 'annee', 'trimestre'))


# save prepped data in new R object and csv
saveRDS(DT, outputFile2a)
write.csv(DT, "prepped_outputs_outcomes.csv")
archive(outputFile2a)

#---------------
print("Step 2a: Prep outcomes impacts completed successfully.")
