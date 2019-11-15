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
interaction_effects = urFit[grepl(':',rhs), c('lhs','op','rhs','est','est.std'), with=FALSE]
interaction_effects[, rhs:=gsub(':.*','',rhs)]

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
# -----------------------------------------------


# -----------------------------------------------
# Graph

# my sem graph function for first half "unrelated regressions" model
p5 = semGraph(parTable=urFit, nodeTable=nodeTable, 
              scaling_factors=NA, standardized=TRUE, 
              lineWidth=1.5, curved=0, tapered=FALSE, colScaleMin=-0.5, colScaleMax=1.5, labSize1 = 4, labSize2 = 4)

p5_nolab = semGraph(parTable=urFit, nodeTable=nodeTable, 
                    scaling_factors=NA, standardized=TRUE, 
                    lineWidth=1.5, curved=0, tapered=FALSE, edgeLabels=FALSE, colScaleMin=-0.5, colScaleMax=1.5, 
                    labSize1 = 4, labSize2 = 4)

#New SEM graphs for RSSH interaction term datasets - both standardized and not. 
graph_no_rssh = semGraph(parTable=no_rssh, nodeTable=no_rssh_table, 
                         scaling_factors=NA, standardized=FALSE, 
                         lineWidth=1.5, curved=0, tapered=FALSE,
                         colScaleMin=-0.5, colScaleMax=1.5, labSize1 = 4, labSize2 = 4)

graph_no_rssh_std = semGraph(parTable=no_rssh, nodeTable=no_rssh_table, 
                         scaling_factors=NA, standardized=TRUE, 
                         lineWidth=1.5, curved=0, tapered=FALSE,
                         colScaleMin=-0.5, colScaleMax=1.5, labSize1 = 4, labSize2 = 4)

graph_rssh_interaction = semGraph(parTable=rssh_interaction, nodeTable=rssh_interaction_table, 
                         scaling_factors=NA, standardized=FALSE, 
                         lineWidth=1.5, curved=0, tapered=FALSE,
                         colScaleMin=-0.5, colScaleMax=1.5, labSize1 = 4, labSize2 = 4)

graph_rssh_interaction_std = semGraph(parTable=rssh_interaction, nodeTable=rssh_interaction_table, 
                             scaling_factors=NA, standardized=TRUE, 
                             lineWidth=1.5, curved=0, tapered=FALSE,
                             colScaleMin=-0.5, colScaleMax=1.5, labSize1 = 4, labSize2 = 4)


# ----------------------------------------------


# Adding a few specific pathways to visualize using 'dim' and 'highlight' options. 
#MDR pathway 
p7 = semGraph(parTable=urFit, nodeTable=nodeTable,
              scaling_factors=NA, standardized=TRUE,
              lineWidth=1.5, curved=0, tapered=FALSE, labSize2 = 4, labSize1 = 4,
              dim=TRUE, highlight=c("Number_of_Cases_Screened_for_MDR_act_cumulative", "MDR_Cases_Started_Treatment_out_cumulative", 
                                    "Secondline_Distributed_act_cumulative", "Proportion_of_MDR_Cases_Treated_out", 
                                    "Proportion_of_Patients_Receiving_DST_out", 
                                    "gf_mdrtb_cumulative", "ghe_tb_cumulative", "odah_tb_cumulative"))

#Cases notified pathway
p8 = semGraph(parTable=urFit, nodeTable=nodeTable,
              scaling_factors=NA, standardized=TRUE,
              lineWidth=1.5, curved=0, tapered=FALSE, labSize2 = 4, labSize1 = 4,
              dim=TRUE, highlight=c("Cases_Notified_out_cumulative", "Additional_Cases_Detected_via_ACF_out_cumulative", 
                                    "Children_less5_referred_out_cumulative", "gf_tbhiv_cumulative", "gf_mdrtb_cumulative", 
                                    "Case_Notification_Rate_imp", 
                                    "gf_tb_cumulative", "ghe_tb_cumulative", "odah_tb_cumulative", "Case_Notification_Rate_imp_log"))

#GHE pathway 
p9 = semGraph(parTable=urFit, nodeTable=nodeTable,
              scaling_factors=NA, standardized=TRUE,
              lineWidth=1.5, curved=0, tapered=FALSE, labSize2 = 4, labSize1 = 4,
              dim=TRUE, highlight=c("ghe_tb_cumulative", "Number_of_Cases_Screened_for_MDR_act_cumulative", 
                                    "Cases_Notified_out_cumulative", "Cases_Started_on_Treatment_out_cumulative", "Firstline_Distributed_act_cumulative", 
                                    "TB_Patients_Tested_for_HIV_act_cumulative"))
#Just visualize outputs to outcomes 
outcomes_outputs = as.vector(nodeTable[x%in%c(3, 10), unique(variable)])

p10 = semGraph(parTable=urFit, nodeTable=nodeTable,
               scaling_factors=NA, standardized=TRUE,
               lineWidth=1.5, curved=0, tapered=FALSE, labSize2 = 4, labSize1 = 4,
               dim=TRUE, highlight=c(outcomes_outputs))
# -----------------------------------
# Save output
print(paste('Saving:', outputFile6a)) 
pdf(outputFile6a, height=6, width=9)
# print(p5)
# print(p5_nolab)
# print(graph_no_rssh)
print(graph_no_rssh_std)
# print(graph_rssh_interaction)
print(graph_rssh_interaction_std)
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile6a)
# -----------------------------------
# 
# # #Save just the GLM diagrams with correlation coefficients as PNGs. 
# ggsave("J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/model_first_half.png", p5, height=10, width=13)

# sep_terg_save = "J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/september_terg_presentation/"
# #Save the specific graphics for reports in their own folder. 
# ggsave(paste0(sep_terg_save, "model_first_half_coefficients.png"), p5, height=10, width=13)
# ggsave(paste0(sep_terg_save, "model_first_half_coefficients_nolab.png"), p5_nolab, height=10, width=13)
# ggsave(paste0(sep_terg_save, "mdr_pathway.png"), p7, height=10, width=13)
# ggsave(paste0(sep_terg_save, "cases_notified_pathway.png"), p8, height=10, width=13)
# ggsave(paste0(sep_terg_save, "ghe_pathway.png"), p9, height=10, width=13)
# ggsave(paste0(sep_terg_save, "outputs_outcomes_pathway.png"), p10, height=10, width=13)
