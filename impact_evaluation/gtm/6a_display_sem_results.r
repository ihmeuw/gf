# -----------------------------------
# David Phillips
# 
# 2/4/2019
# This visualizes results of the SEM
# -----------------------------------


# -----------------------------------------------
# Load data and functions

source('./impact_evaluation/gtm/set_up_r.r')

# load home-made sem graphing function
source('./impact_evaluation/_common/graphLavaan.r')

# load model results
load(outputFile5a)

# load nodeTable for graphing
nodeTable = fread(nodeTableFile1)
# -----------------------------------------------

# Fix LHS and RHS variables 
urFits[, lhs:=trimws(lhs)]
urFits[, rhs:=trimws(rhs)]

# -----------------------------------------------
# Aggregate department-level fit objects

# compute averages (approximation of standard error, would be better as Monte Carlo simulation)
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
urFits[, se_ratio.std:=se.std/est.std]
urFits[, se_ratio:=se/est]
urFit = urFits[, lapply(.SD, mean, na.rm=TRUE), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit[se>abs(se_ratio*est), se:=abs(se_ratio*est)]
# -----------------------------------------------


# -----------------------------------------------
# Resolve interaction terms

# make a version with no RSSH effect
no_rssh=copy(urFit) 
no_rssh_table = fread(nodeTableFile3)
no_rssh = no_rssh[lhs%in%no_rssh_table$variable & rhs%in%no_rssh_table$variable]

# make a version showing the effect of spending, modified by a typical level of RSSH
rssh_interaction=copy(urFit)
rssh_interaction_table = fread(nodeTableFile4)

# look up a constant to use as the RSSH level (standardized and unstandardized)
mean_rssh = mean(data$gf_rssh_cumulative)
data[, gf_rssh_cumulative_std:=(gf_rssh_cumulative-mean(gf_rssh_cumulative))/ifelse(sd(gf_rssh_cumulative)>0, sd(gf_rssh_cumulative), 1)]
mean_rssh_std = mean(data$gf_rssh_cumulative_std)

# look up a minimum to use for the no-RSSH scenario because the standardized data has a mean of zero
min_rssh_std = min(data$gf_rssh_cumulative_std)

# isolate interaction terms and prep to add them to main effects
interaction_effects = urFit[grepl('\\:|\\*',rhs), c('lhs','op','rhs','est','est.std'), with=FALSE] #This isn't working?? 
interaction_effects[, rhs:=gsub(':gf_rssh_cumulative','',rhs)]
interaction_effects[, rhs:=gsub('gf_rssh_cumulative:','',rhs)]

# multiply constants by interaction coefficients
interaction_effects[, est:=est*mean_rssh]
interaction_effects[, est.std_min_em:=est.std*min_rssh_std]
interaction_effects[, est.std:=est.std*mean_rssh_std]
setnames(interaction_effects, c('est','est.std'), c('est_em','est.std_em'))

# add effect modifier coefficients to main effects
rssh_interaction = merge(rssh_interaction, interaction_effects, by=c('lhs','op','rhs'), all.x=TRUE)
rssh_interaction[!is.na(est_em), est:=est+est_em]
rssh_interaction[!is.na(est.std_em), est.std:=est.std+est.std_em]

# adjust-down the no-RSSH scenario using the minimum
no_rssh = merge(no_rssh, interaction_effects, by=c('lhs','op','rhs'), all.x=TRUE)
no_rssh[!is.na(est.std_min_em), est.std:=est.std+est.std_min_em]

# now drop interaction terms
no_rssh = no_rssh[!grepl(':', rhs)]
rssh_interaction = rssh_interaction[!grepl(':', rhs)]

# Note: for standardized coefficients, the mean rssh that gets multiplied by the interaction term is zero
# the no_rssh scenario is intended to display standardized coefficients at a low level of rssh, the right 
# comparison is the urFit

# Note: at this point, the coefficients related to rhs=='gf_rssh_cumulative' are the
# main effect of RSSH NOT including the interaction term held at its mean (the above 
# implements main effects of other variables holding RSSH at its mean). To implement 
# main effects of RSSH, we would need to resolve equations with multiple interaction
# terms. That has not been done yet. (David 11/22/2019)
# -----------------------------------------------


# -----------------------------------------------
# Graph

# my sem graph function for first half "unrelated regressions" model
p5 = semGraph(parTable=urFit, nodeTable=nodeTable, 
              scaling_factors=NA, standardized=TRUE, 
              lineWidth=1, curved=0, tapered=FALSE, colScaleMin=-2, colScaleMax=2, 
              labSize1 = 3, labSize2 = 3, baseSize=10)

graph_no_rssh_std = semGraph(parTable=no_rssh, nodeTable=no_rssh_table, 
                             scaling_factors=NA, standardized=TRUE, 
                             lineWidth=1, curved=0, tapered=FALSE,
                             colScaleMin=-2, colScaleMax=2, 
                             labSize1 = 3, labSize2 = 3, baseSize=10)

# Make some pathways graphs 
path1 = semGraph(parTable=rssh_interaction, nodeTable=rssh_interaction_table,
              scaling_factors=NA, standardized=TRUE,
              lineWidth=1, curved=0, tapered=FALSE,
              colScaleMin=-2, colScaleMax=2, labSize1 = 3, labSize2 = 3, baseSize=10,
              dim=TRUE, highlight=c("gf_tb_cumulative", "gf_mdrtb_cumulative", "gf_tbhiv_cumulative", "TB_Patients_Tested_for_HIV_act_cumulative", 
                                    "Number_of_Cases_Screened_for_MDR_act_cumulative", "Secondline_Distributed_act_cumulative", "Additional_Cases_Detected_via_ACF_out_cumulative"))

path2 = semGraph(parTable=rssh_interaction, nodeTable=rssh_interaction_table,
                 scaling_factors=NA, standardized=TRUE,
                 lineWidth=1, curved=0, tapered=FALSE,
                 colScaleMin=-2, colScaleMax=2, labSize1 = 3, labSize2 = 3, baseSize=10,
                 dim=TRUE, highlight=c("ghe_tb_cumulative", "TB_Patients_Tested_for_HIV_act_cumulative", 
                                       "Number_of_Cases_Screened_for_MDR_act_cumulative", "Firstline_Distributed_act_cumulative", "Cases_Notified_out_cumulative"))

# ----------------------------------------------

# -----------------------------------
# Save output
print(paste('Saving:', outputFile6a)) 
pdf(outputFile6a, height=6, width=9)
print(p5)
print(path1)
print(path2)
print(graph_no_rssh_std)
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile6a)
# -----------------------------------
