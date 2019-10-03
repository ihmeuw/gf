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
no_rssh_table = fread(nodeTableFile3)
no_rssh = no_rssh[lhs%in%no_rssh_table$variable & rhs%in%no_rssh_table$variable]

#This dataset models the interaction term. 
# DAVID PLEASE REVIEW THIS!!
rssh_interaction=copy(urFit1)
rssh_interaction_table = fread(nodeTableFile4)
MEAN_RSSH = mean(data$gf_rssh_cumulative) #Generate global variable for mean RSSH. 

#Wherever there is an interaction term on the right hand side, calculate interaction term. 
interaction_est = data.table()
for (v in c('gf_tb_cumulative', 'gf_mdrtb_cumulative', 'gf_tbhiv_cumulative')){
  #First grab the interaction terms. 
  subset = rssh_interaction[rhs==paste0(v, ":gf_rssh_cumulative"), .(lhs, est, est.std)] 
  
  #Calculate the interaction term for both est and est.std. 
  subset[, est_interaction:=est*MEAN_RSSH] 
  subset[, est.std_interaction:=est.std*MEAN_RSSH]
  
  #Then, format data before appending.  
  subset[, rhs:=v]
  subset = subset[, .(lhs, rhs, est_interaction, est.std_interaction)]
  interaction_est = rbind(interaction_est, subset)
} 

#Merge new interaction terms onto old dataset. 
rssh_interaction = merge(rssh_interaction, interaction_est, by=c('lhs', 'rhs'), all=T) #Shouldn't need all argument but adding it anyway. 
  
#Replace 'est' and 'est.std' with final interaction term where appropriate. 
rssh_interaction[!is.na(est_interaction), est:=est + est_interaction]
rssh_interaction[!is.na(est.std_interaction), est.std:=est.std+est.std_interaction]

#Subset down to only the variables you care about. 
rssh_interaction = rssh_interaction[lhs%in%rssh_interaction_table$variable & rhs%in%rssh_interaction_table$variable]

# my sem graph function for first half "unrelated regressions" model
p5 = semGraph(parTable=urFit1, nodeTable=nodeTable1, 
	scaling_factors=NA, standardized=TRUE, 
	lineWidth=1.5, curved=0, tapered=FALSE, colScaleMin=-0.5, colScaleMax=1.5, labSize1 = 4, labSize2 = 4)

p5_nolab = semGraph(parTable=urFit1, nodeTable=nodeTable1, 
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
p7 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
              scaling_factors=NA, standardized=TRUE,
              lineWidth=1.5, curved=0, tapered=FALSE, labSize2 = 4, labSize1 = 4,
              dim=TRUE, highlight=c("Number_of_Cases_Screened_for_MDR_act_cumulative", "MDR_Cases_Started_Treatment_out_cumulative", 
                                    "Secondline_Distributed_act_cumulative", "Proportion_of_MDR_Cases_Treated_out", 
                                    "Proportion_of_Patients_Receiving_DST_out", 
                                    "gf_mdrtb_cumulative", "ghe_tb_cumulative", "odah_tb_cumulative"))

#Cases notified pathway
p8 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
              scaling_factors=NA, standardized=TRUE,
              lineWidth=1.5, curved=0, tapered=FALSE, labSize2 = 4, labSize1 = 4,
              dim=TRUE, highlight=c("Cases_Notified_out_cumulative", "Additional_Cases_Detected_via_ACF_out_cumulative", 
                                    "Children_less5_referred_out_cumulative", "gf_tbhiv_cumulative", "gf_mdrtb_cumulative", 
                                    "Case_Notification_Rate_imp", 
                                    "gf_tb_cumulative", "ghe_tb_cumulative", "odah_tb_cumulative", "Case_Notification_Rate_imp_log"))

#GHE pathway 
p9 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
              scaling_factors=NA, standardized=TRUE,
              lineWidth=1.5, curved=0, tapered=FALSE, labSize2 = 4, labSize1 = 4,
              dim=TRUE, highlight=c("ghe_tb_cumulative", "Number_of_Cases_Screened_for_MDR_act_cumulative", 
                                    "Cases_Notified_out_cumulative", "Cases_Started_on_Treatment_out_cumulative", "Firstline_Distributed_act_cumulative", 
                                    "TB_Patients_Tested_for_HIV_act_cumulative"))
#Just visualize outputs to outcomes 
outcomes_outputs = as.vector(nodeTable1[x%in%c(3, 10), unique(variable)])

p10 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
               scaling_factors=NA, standardized=TRUE,
               lineWidth=1.5, curved=0, tapered=FALSE, labSize2 = 4, labSize1 = 4,
               dim=TRUE, highlight=c(outcomes_outputs))
# -----------------------------------
# Save output
print(paste('Saving:', outputFile6a)) 
pdf(outputFile6a, height=6, width=9)
# print(p1)
# print(p2)
# print(p3)
# print(p4)
print(p5)
print(p5_nolab)
print(graph_no_rssh)
print(graph_no_rssh_std)
print(graph_rssh_interaction)
print(graph_rssh_interaction_std)
# print(p6)
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile6a)
# -----------------------------------
# 
# #Save just the GLM diagrams with correlation coefficients as PNGs. 
ggsave("J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/model_first_half.png", p5, height=10, width=13)
# ggsave("J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/model_second_half.png", p6, height=10, width=13)

sep_terg_save = "J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/september_terg_presentation/"
#Save the specific graphics for reports in their own folder. 
ggsave(paste0(sep_terg_save, "model_first_half_coefficients.png"), p5, height=10, width=13)
ggsave(paste0(sep_terg_save, "model_first_half_coefficients_nolab.png"), p5_nolab, height=10, width=13)
# ggsave(paste0(sep_terg_save, "model_second_half_coefficients.png"), p6, height=10, width=13)
# ggsave(paste0(sep_terg_save, "model_second_half_coefficients_nolab.png"), p6_nolab, height=10, width=13)
ggsave(paste0(sep_terg_save, "mdr_pathway.png"), p7, height=10, width=13)
ggsave(paste0(sep_terg_save, "cases_notified_pathway.png"), p8, height=10, width=13)
ggsave(paste0(sep_terg_save, "ghe_pathway.png"), p9, height=10, width=13)
ggsave(paste0(sep_terg_save, "outputs_outcomes_pathway.png"), p10, height=10, width=13)
