# prep table with model coefficients for annex of HSM paper - for Senegal

# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------

#-----------------------------------------
# TO-DO: 
# * update to run for senegal

#-----------------------------------------

# ---------------------------------------
# Install packages and set up R  
# Taken from David's code in 6a_display_sem_results.r
# Adapted by Francisco and then Audrey
# ---------------------------------------

# clear memory
rm(list=ls())
# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/drc/set_up_r.r')

# load model results
# this is the confirmed "best model"
load("J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/model_runs/first_half_model_results_pc_2019_07_30_11_01_04.rdata") 
data1=copy(data)
means1 = copy(means)

# second half model
load(outputFile5b)
data2=copy(data)
means2 = copy(means)

nodeTable1 = fread(nodeTableFile1)
nodeTable1 = nodeTable1[, -c('x', 'y')]
nodeTable2 = fread(nodeTableFile2)
nodeTable2 = nodeTable2[, -c('x', 'y')]
################## Section below cleans and reformats datatable into format for HSM paper
# combine first and second half models
means = rbind(means1, means2)
nodeTable = rbind(nodeTable1, nodeTable2)

# merge cleaned names onto them
means = merge(means, nodeTable, by.x = 'lhs', by.y = 'variable', all = TRUE)
setnames(means, 'label', 'lhs_label')
means = merge(means, nodeTable, by.x = 'rhs', by.y = 'variable', all = TRUE)
setnames(means, 'label', 'rhs_label')

means[is.na(rhs_label), rhs_label := rhs]
means[is.na(lhs_label), lhs_label := lhs]

#  calculate 95% CI Lower and Upper
means[, lower := est - (1.95996*se)]
means[, upper := est + (1.95996*se)]

# round numbers to two decimal places
means[, est:=round(est, 2)]
means[, lower:=round(lower, 2)]
means[, upper:=round(upper, 2)]
means[, est.std:=round(est.std, 2)]

means[, model := 'DRC Malaria']

# keep only dependent variable, independent variable, est, lower, upper, est.std
annex_table = means[,.(model, lhs_label, rhs_label, est, lower, upper, est.std)]

# change column names
setnames(annex_table, "model", "Model")
setnames(annex_table, "lhs_label", "Dependent variable (y)")
setnames(annex_table, "rhs_label", "Independent variable (x)")
setnames(annex_table, "est", "Estimate")
setnames(annex_table, "lower", "Lower CI")
setnames(annex_table, "upper", "Upper CI")
setnames(annex_table, "est.std", "Estimate (standardized)")

write.csv(annex_table, "J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/DRC_model_annex.csv", row.names = FALSE)
