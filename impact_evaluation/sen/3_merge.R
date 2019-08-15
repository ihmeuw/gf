# AUTHOR: Francisco Rios Casas
# PURPOSE: Merge data to run SEM
# 3_merge

library(data.table)

# ----------------------------------------------------------
# Load data

# Read in the previously saved files for resource tracking in 2a
resource_tracking <- readRDS('J:/Project/Evaluation/GF/impact_evaluation/sen/prepped_data/prepped_resource_tracking.RDS')

# Read in the previously saved file for outputs/activities in 2b
outputs_activities <- readRDS('J:/Project/Evaluation/GF/impact_evaluation/sen/prepped_data/prepped_outputs_outcomes.RDS')

# -----------------------------------------------------------------------------------------------------
# Prep/merge data

# aggregate data to the admin1 (Regions in Senegal)
# VALUES WHICH CAN BE SUMMED

# VALUES WHICH MUST BE AVERAGED

# keep variables to be used in model
data = outputs_activities[,.(region, annee, centre, trimestre, com_mobsoc, com_cause, com_radio, com_vad_pdv, com_enf_ref, com_nom_touss, tb_tfc, perf_lab, ntr_rhz, ntr_erhz, ntr_serhz, ntr_cpx,
        tot_genexpert, tot_confirme, tot_res,
        tbtot_taux_det,
        gueris_total, gueris_taux, trait_tot, trait_pc)]

data = data[, date:=annee+((trimestre-1)/4)] # add date variable
data = data[, lapply(.SD, sum), by=c('region','annee','centre','date'), .SDcols=names(data)[!names(data) %in% c('region','annee','trimestre','date','district')]]

