# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: Merge prepped data for Sen TB Model
# DATE: Auguts 15, 2019
# ----------------------------------------------------------

library(data.table)

# ----------------------------------------------------------
# Load data

# Read in the previously saved files for resource tracking in 2a
resource_tracking <- readRDS('J:/Project/Evaluation/GF/impact_evaluation/sen/prepped_data/prepped_resource_tracking.RDS')

# Read in the previously saved file for outputs/outcomes in 2b
outputs_outcomes <- readRDS('J:/Project/Evaluation/GF/impact_evaluation/sen/prepped_data/prepped_outputs_outcomes.RDS')

# -----------------------------------------------------------------------------------------------------
# Prep data on outputs_outcomes

# add date variable
outputs_outcomes = outputs_outcomes[, date:=annee+((trimestre-1)/4)]

# keep only variables that are relevant for the model
sub_data = outputs_outcomes[,.(region, centre, date, 
                                 com_mobsoc, com_cause, com_radio, com_vad_pdv, com_enf_ref, com_nom_touss, 
                                 tb_tfc, perf_lab, 
                                 ntr_rhz, ntr_erhz, ntr_serhz, ntr_cpx,
                                 tot_genexpert, tot_confirme, tot_res,
                                 tbtot_taux_det,
                                 gueris_total, gueris_taux, trait_tot, trait_pc)]

# aggregate data to the admin1 (Regions in Senegal)

# VALUES WHICH CAN BE SUMMED (counts)
summed_data = sub_data[, lapply(.SD, sum), by=c('region','date'), .SDcols=c('tb_tfc',
                                                                            'ntr_rhz', 'ntr_erhz', 'ntr_serhz', 'ntr_cpx', 
                                                                            'tot_genexpert', 'tot_confirme', 'tot_res',
                                                                            'gueris_total', 'trait_tot')]


# VALUES WHICH MUST BE AVERAGED (percents or rates)
averaged_data = sub_data[, lapply(.SD, mean, na.rm=TRUE), by=c('region','date'), .SDcols=c('perf_lab', 'tbtot_taux_det', 'gueris_taux', 'trait_pc')]


# VALUES WHICH MUST BE DIVIDED EVENLY (data reported in annual totals only)


