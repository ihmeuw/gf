# -----------------------------------
# Francisco Rios, David Phillips
# 
# 9/13/2019
# This visualizes results of the SEM
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/sen/set_up_r.r')

# load home-made sem graphing function
source('./impact_evaluation/_common/graphLavaan.r')

# load model results
load(outputFile5a)
data1=copy(data)
model1=copy(model)
# means1 = copy(means)
# summaries1 = copy(summaries)
urFits1 = copy(urFits)
# load(outputFile5b)
# data2=copy(data)
# model2=copy(model)
# means2 = copy(means)
# summaries2 = copy(summaries)
#urFits2 = copy(urFits)

# load nodeTable for graphing
nodeTable1 = fread(nodeTableFile1)
#nodeTable2 = fread(nodeTableFile2)

names(data1)[!names(data1)%in%nodeTable1$variable]

# ensure there are no extra variables introducted from nodeTable
nodeTable1 = nodeTable1[variable %in% names(data1)]
# nodeTable2 = nodeTable2[variable %in% names(data2)]

# compute averages (approximation of standard error, would be better as Monte Carlo simulation)
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
urFits1[, se_ratio.std:=se.std/est.std]
urFits1[, se_ratio:=se/est]
urFit1 = urFits1[, lapply(.SD, mean, na.rm=TRUE), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit1[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit1[se>abs(se_ratio*est), se:=abs(se_ratio*est)]

# -----------------------------------------------

# # ----------------------------------------------
# Display results

# my sem graph function for first half "unrelated regressions" model
p0 = semGraph(parTable=urFit1, nodeTable=nodeTable1, 
                            scaling_factors=NA, standardized=TRUE, 
                            lineWidth=1.5, curved=0, tapered=FALSE, edgeLabels=FALSE, colScaleMin=-1.5, colScaleMax=1.5, colScaleBreaks = 1)

p1 = semGraph(parTable=urFit1, nodeTable=nodeTable1, 
	scaling_factors=NA, standardized=TRUE, 
	lineWidth=1.5, curved=0, tapered=FALSE, colScaleMin= -1.5, colScaleMax=1.5, colScaleBreaks = 1)

p2 = semGraph(parTable=urFit1, nodeTable=nodeTable1, 
              scaling_factors=NA, standardized=TRUE, 
              lineWidth=1.5, curved=0, tapered=FALSE, colScaleMin= -50, colScaleMax=50, colScaleBreaks = 25)

# treatment success rate pathways
p3 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
         scaling_factors=NA, standardized=TRUE,
         lineWidth=1.5, curved=0, tapered=FALSE,
         dim=TRUE, colScaleMin = -1.5,
         highlight=c('com_mobsoc_cumulative',
                               'com_radio_cumulative',
                               'com_vad_touss_cumulative',
                               'lead_gueris_taux',
                               'tb_tfc_cumulative',
                               'tb_vih_arv_cumulative'
         ))

# IPT children pathway
p4 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
              scaling_factors=NA, standardized=TRUE,
              lineWidth=1.5, curved=0, tapered=FALSE,
              dim=TRUE, highlight=c('com_vad_touss_cumulative',
                                    'com_enf_ref_cumulative',
                                    'lead_tpm_chimio_enf_cumulative',
                                    'lag_gf_tbcare_cumulative'
              ))

# treatment first line pathway
p5 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
              scaling_factors=NA, standardized=TRUE,
              lineWidth=1.5, curved=0, tapered=FALSE,
              dim=TRUE, highlight=c('tb_tfc_cumulative',
                                    'tb_cas_id_cumulative',
                                    'tot_genexpert_cumulative',
                                    'lead_gueris_taux'
              ))

p6 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
                   scaling_factors=NA, standardized=TRUE,
                   lineWidth=1.5, curved=0, tapered=FALSE,
                   dim=TRUE, highlight=c('com_mobsoc_cumulative',
                                         'tb_cas_id_cumulative',
                                         'lead_gueris_taux'
                   ))

p7 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
         scaling_factors=NA, standardized=TRUE,
         lineWidth=1.5, curved=0, tapered=FALSE,
         dim=TRUE, highlight=c('com_mobsoc_cumulative', 
                               'com_radio_cumulative',
                               'com_vad_touss_cumulative',
                               'tb_cas_id_cumulative'
         ))


p8 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
         scaling_factors=NA, standardized=TRUE,
         lineWidth=1.5, curved=0, tapered=FALSE,
         dim=TRUE, highlight=c('com_vad_touss_cumulative',
                               'com_nom_touss_cumulative',
                               'lead_gueris_taux',
                               'com_enf_ref_cumulative',
                               'tb_cas_id_cumulative'
         ))


p9 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
               scaling_factors=NA, standardized=TRUE,
               lineWidth=1.5, curved=0, tapered=FALSE,
               dim=TRUE, highlight=c('perf_lab',
                                     'tb_cas_id_cumulative',
                                     'tot_genexpert_cumulative',
                                     'lag_gf_detect_cumulative'
                                     
               ))

p10 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
              scaling_factors=NA, standardized=TRUE,
              lineWidth=1.5, curved=0, tapered=FALSE,
              dim=TRUE, highlight=c('perf_lab',
                                    'tb_cas_id_cumulative',
                                    'tot_genexpert_cumulative',
                                    'lag_other_dah_T1_1_cumulative'
                                    
              ))

p11 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
               scaling_factors=NA, standardized=TRUE,
               lineWidth=1.5, curved=0, tapered=FALSE,
               dim=TRUE, 
               colScaleMin = -1.5, colScaleMax = 0.8,
               highlight=c('lag_gf_mdrtb_cumulative',
                                     'lag_other_dah_T3_cumulative',
                                     'tot_genexpert_cumulative',
                                     'mdr_success_cumulative',
                                     'patients_prop_genexpert_cumulative',
                                     'dx_count_cumulative',
                           'lag_other_dah_T1_1_cumulative', 'lag_gf_detect_cumulative'
                           
               ))
p12 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
                     scaling_factors=NA, standardized=TRUE,
                     lineWidth=1.5, curved=0, tapered=FALSE,
                     dim=TRUE, colScaleMin = 0,
               highlight=c('perf_lab',
                                           'tb_cas_id_cumulative',
                                           'tot_genexpert_cumulative',
                                           'lag_gf_detect_cumulative',
                                           'lag_gf_tbcare_cumulative', 
                                           'com_vad_touss_cumulative',
                                           'com_radio_cumulative',
                                           'com_mobsoc_cumulative',
                                           'tb_tfc_cumulative'                     ))


#p5_nolab = semGraph(parTable=urFit1, nodeTable=nodeTable1, 
#              scaling_factors=NA, standardized=TRUE, 
#              lineWidth=1.5, curved=0, tapered=FALSE, edgeLabels=FALSE, colScaleMin=-0.5, colScaleMax=2, colScaleBreaks = 1)

# ----------------------------------------------
# -----------------------------------
# Save output
print(paste('Saving:', outputFile6a)) 
pdf(outputFile6a, height=9, width=12)
print(p0)
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)
print(p9)
print(p10)
print(p11)
print(p12)
dev.off()

# save a time-stamped version for reproducibility
archive(outputFile6a)

# Adding a few specific pathways to visualize using 'dim' and 'highlight' options. 
##MDR pathway 
#
#p7 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
#              scaling_factors=NA, standardized=TRUE,
#              lineWidth=1.5, curved=0, tapered=FALSE, 
#              dim=TRUE, highlight=c('ntr_all_cumulative', 'gueris_taux'))
#
#semGraph(parTable=urFit1, nodeTable=nodeTable1,
#              scaling_factors=NA, standardized=TRUE,
#              lineWidth=1.5, curved=0, tapered=FALSE, colScaleMax = 2,
#              dim=TRUE, highlight=c('com_mobsoc_cumulative', 
#                                    'com_cause_cumulative',
#                                    'com_radio_cumulative',
#                                    'tb_tfc_cumulative'
#                                    ))
#
## cough orienting pathway
#semGraph(parTable=urFit1, nodeTable=nodeTable1,
#         scaling_factors=NA, standardized=TRUE,
#         lineWidth=1.5, curved=0, tapered=FALSE,
#         dim=TRUE, highlight=c('com_mobsoc_cumulative', 
#                               'com_cause_cumulative',
#                               'com_radio_cumulative',
#                               'com_nom_touss_cumulative'
#         ))
#
## cough orienting pathway
#semGraph(parTable=urFit1, nodeTable=nodeTable1,
#         scaling_factors=NA, standardized=TRUE,
#         lineWidth=1.5, curved=0, tapered=FALSE,
#         dim=TRUE, highlight=c('tot_genexpert_cumulative',
#                               'lag_exp_T3_cumulative',
#                               'lag_other_dah_T3_cumulative'
#                                ))
#
##Cases notified pathway
#semGraph(parTable=urFit1, nodeTable=nodeTable1,
#              scaling_factors=NA, standardized=TRUE,
#              lineWidth=1.5, curved=0, tapered=FALSE, 
#              dim=TRUE, highlight=c('ntr_all_cumulative',
#                                    'tb_tfc_cumulative',
#                                    'com_enf_ref_cumulative',
#                                    'tpm_chimio_enf_cumulative'))
#
##GHE pathway 
#p9 = semGraph(parTable=urFit1, nodeTable=nodeTable1,
#              scaling_factors=NA, standardized=TRUE,
#              lineWidth=1.5, curved=0, tapered=FALSE, 
#              dim=TRUE, highlight=c("ghe_tb_cumulative", "Number_of_Cases_Screened_for_MDR_act_cumulative", 
#                                    "Cases_Notified_out_cumulative", "Cases_Started_on_Treatment_out_cumulative", "Firstline_Distributed_act_cumulative", 
#                                    "TB_Patients_Tested_for_HIV_act_cumulative"))






















