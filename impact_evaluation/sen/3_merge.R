# AUTHOR: Francisco Rios Casas
# PURPOSE: Merge data to run SEM
# 3_merge

library(data.table)

# read file
data <- readRDS("J:/Project/Evaluation/GF/impact_evaluation/sen/prepped_data/sen_tb_indicators_08132019.RDS")

# keep variables to be used in model
data[,.(com_mobsoc, com_cause, com_radio, com_vad_pdv, com_enf_ref, com_nom_touss, tb_tfc, perf_lab, ntr_rhz, ntr_erhz, ntr_serhz, ntr_cpx,
        tot_genexpert, tot_confirme, tot_res,
        tbtot_taux_det,
        gueris_total, gueris_taux, trait_tot, trait_pc)]