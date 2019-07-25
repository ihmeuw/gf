# R file to clean Senegal TB data located in
# Filename= PANEL PNT TB-MR final_01072019
# Francisco Rios Casas
# July 22, 2019

# set-up
library(data.table)

# read in data
# CSV file encoded in UTF-8 will preserve the accent marks
DT <- fread("C:\\Users\\frc2\\Documents\\data\\tb\\PANEL PNT TB-MR final_01072019.csv", encoding = "UTF-8")

# ------------------------------
# remove rows and columns
# ------------------------------

# remove unecessary columns at end
DT[,76:261:=NULL]

# remove rows that present summed values
DT <- DT[ !(DT$Centre %in% c("TOTAL TRIMESTRE", "TOTAL ANNEE")),]
DT <- DT[ !(DT$REGION %in% c("SENEGAL"))]

# create unique identifier: region_centre_year_quarter
DT$id <- paste(DT$REGION,DT$Centre,DT$Année,DT$Trimestre, sep="-")

# remove duplicate entries
DT <- unique(DT, by=c("id"))

# remove any rows in which there is no identifying information
DT <- DT[ !(DT$id %in% c("--NA-NA"))]

# ------------------------------
# rename headers
# ------------------------------
codes <- fread("C:\\Users\\frc2\\Documents\\sen_tb_codebook.csv", encoding = "UTF-8")
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
DT$tpm_att <- gsub(",","",DT$tpm_att)
DT$tpm_det <- gsub(",","",DT$tpm_det)
DT$tpm_taux_det <- gsub(",","",DT$tpm_taux_det)
DT$tbtot_att <- gsub(",","",DT$tbtot_att)
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

# remove dashes (-)
DT$pop <- gsub("-","",DT$pop)
DT$tpm_att <- gsub("-","",DT$tpm_att)
DT$tpm_det <- gsub("-","",DT$tpm_det)
DT$tpm_taux_det <- gsub("-","",DT$tpm_taux_det)
DT$tbtot_att <- gsub("-","",DT$tbtot_att)
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

# remove percent signs
DT$tbtot_taux_det <- gsub("%","",DT$tbtot_taux_det)
DT$tpm_taux_det <- gsub("%","",DT$tpm_taux_det)
DT$trait_pc <- gsub("%", "",DT$trait_pc)
DT$gueris_taux <- gsub("%", "", DT$gueris_taux)
DT$deces_pc <- gsub("%", "", DT$deces_pc)
DT$echec_pc <- gsub("%","", DT$echec_pc)
DT$pdv_pc <- gsub("%","", DT$pdv_pc)
DT$eval_pc <- gsub("%", "", DT$eval_pc)
DT$noneval_pc <- gsub("%", "", DT$eval_pc)

# convert values to numeric
DT[,pop:=as.numeric(pop)]
DT[,tpm_att:=as.numeric(tpm_att)]
DT[,tpm_det:=as.numeric(tpm_det)]
DT[,tpm_taux_det:=as.numeric(tpm_taux_det)]
DT[,tbtot_att:=as.numeric(tbtot_att)]
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

# convert percent whole numbers to decimals
DT[, tbtot_taux_det:=tbtot_taux_det/100]
DT[, tpm_taux_det:=tpm_taux_det/100]
DT[, trait_pc:=trait_pc/100]
DT[, gueris_taux:=gueris_taux/100]
DT[, deces_pc:=deces_pc/100]
DT[, echec_pc:=echec_pc/100]
DT[, pdv_pc:=pdv_pc/100]
DT[, struct_micros_pc:=struct_micros_pc/100]
DT[, struct_xray_pc:= struct_xray_pc/100]
DT[, struct_tbmedprem_pc:= struct_tbmedprem_pc/100]
DT[, struct_tbstrepto_pc:= struct_tbstrepto_pc/100]

# save prepped data in new R object and csv
setwd("J:/Project/Evaluation/GF/outcome_measurement/sen/prepped_data")
saveRDS(DT, "sen_tb_indicators.rds")
write.csv(DT, file="sen_tb_indicators.csv")

