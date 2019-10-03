# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: 3. Merge prepped data for Sen TB Model
# DATE: Auguts 15, 2019
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

source('./impact_evaluation/sen/set_up_r.r')

# ----------------------------------------------------------
# Load data

# Read in the previously saved files for resource tracking in 2b
resource_tracking <- readRDS(outputFile2b)

# Read in the previously saved file for outcomes for tb_mdr in 2c
tb_mdr <- readRDS(outputFile2c)

# Read in the previously saved file for outputs/outcomes in 2d
outputs_outcomes <- readRDS(outputFile2d)

# ----------------------------------------------------------
# Prep data on outputs_outcomes
# ----------------------------------------------------------

# keep only variables that are relevant for the model
sub_data = outputs_outcomes[,.(region, centre, date, type,
                                 com_mobsoc, com_cause, com_radio, com_enf_ref, com_nom_touss, 
                                 tb_tfc, perf_lab, 
                                 ntr_rhz, ntr_erhz, ntr_serhz, ntr_cpx,
                                 tot_genexpert, tot_res,
                                 tbtot_taux_det,
                                 gueris_total, gueris_taux, tb_vih_arv,
                                 tpm_chimio_enf, tpm_chimio_pvvih, tb_vih2
                               )]

# aggregate data to the admin1 (Regions in Senegal) ----------------------------------------------------------

# VALUES WHICH CAN BE SUMMED (counts)
datatemp1 = sub_data[, lapply(.SD, sum, na.rm=TRUE), by=c('region','date'), .SDcols=c('tb_tfc', 'tot_genexpert', 
                                                                            'tot_res', 'ntr_rhz', 'ntr_erhz', 'ntr_serhz', 'ntr_cpx',
                                                                            'gueris_total', 
                                                                            'tb_vih_arv', 'tb_vih2')]

# VALUES WHICH MUST BE AVERAGED (percents or rates)
datatemp2 = sub_data[, lapply(.SD, mean, na.rm=TRUE), by=c('region','date'), .SDcols=c('gueris_taux')]

# VALUES WHICH MUST BE DIVIDED EVENLY (data reported in annual totals only)
datatemp3 = sub_data[, lapply(.SD, mean, na.rm=TRUE), by=c('region', 'date'), .SDcols=c('com_mobsoc', 'com_cause', 'com_radio', 'com_enf_ref', 'com_nom_touss')]
datatemp3 = datatemp3[, lapply(.SD, function(x){x*0.25}), by=c('region', 'date'), .SDcols=c('com_mobsoc', 'com_cause', 'com_radio', 'com_enf_ref', 'com_nom_touss')]

# VALUES WHICH ARE NOT REPORTED BY HOSPITALS and MUST BE AVERAGED
datatemp4 = sub_data[type!="HOPITAL", lapply(.SD, mean, na.rm=TRUE), by=c('region','date'), .SDcols=c('perf_lab', 'tbtot_taux_det')]

# remove hospital rows from the data
# VALUES WHICH ARE NOT REPORTED BY HOSPITALS and can be SUMMED
datatemp5 = sub_data[type!="HOPITAL", lapply(.SD, sum, na.rm=TRUE), by=c('region','date'), .SDcols=c('tpm_chimio_enf', 'tpm_chimio_pvvih')]

# merge properly formatted outputs data
merge1 <- merge(datatemp1, datatemp2, by = c("region", "date"), all = TRUE)
merge2 <- merge(merge1, datatemp3, by = c("region", "date"), all = TRUE)
merge3 <- merge(merge2, datatemp4, by = c("region", "date"), all = TRUE)
outputs_prepped <- merge(merge3, datatemp5, by = c("region", "date"), all = TRUE)

# -------------------------------------------------
# Create derived variables on new dataset
# -------------------------------------------------

# create combined total treatment variable
outputs_prepped$ntr_all <- NA 
outputs_prepped$ntr_all <- rowSums(outputs_prepped[,.(ntr_rhz, ntr_erhz, ntr_serhz)])

# create variable of tbvih arv treatment rate
outputs_prepped$tbvih_arvtx_rate <- outputs_prepped$tb_vih_arv/outputs_prepped$tb_vih2
outputs_prepped$tbvih_arvtx_rate[which(outputs_prepped$tb_vih2==0)]<-NA

#----------------------------------------------------------------------------------
# Merge TB-MDR data to other outcomes data
# --------------------------------------------------------------------------------

#first rectangualrize the tb_mdr_data
 hzFrame = unique(outputs_outcomes[, c('region')])
 i=1
 for(d in unique(resource_tracking$date)) {
   hzFrame[, date:=d]
   if(i==1) frame = copy(hzFrame)
   if(i>1) frame = rbind(frame, hzFrame)
   i=i+1
 }
 
tb_mdr_rect = merge(frame, tb_mdr, by=c('region', 'date'), allow.cartesian=TRUE, all=TRUE)
 
# # Merge rectangularized tb_mdr and outputs/activites data 
outcome_data <- merge(tb_mdr_rect, outputs_prepped, by=c('region', 'date'), all=TRUE)

# ----------------------------------------------------------------------
# Merge rectangularized resource tracking and outputs/activites data 
merge_file <- merge(resource_tracking, outcome_data, by=c('date'), all=TRUE)

# --------------------------------------------------------------------------
# Distribute inputs by health zone proportionally to activities

# list variables that need redistribution
# note: these vectors must be the same length and order matters
inVars = c('other_dah_T1_1',
           'exp_T1_1',
           'other_dah_T1_2',
           'exp_T1_2',
           'exp_T1_5',
           'exp_T2',
           'exp_T3',
           'other_dah_T3')

# list corresponding variables to define distribution proportions
actVars = c('value_screen_act',
            'value_screen_act',
            'ntr_all',
            'ntr_all',
            'value_com_act',
            'tb_vih_arv',
            'value_mdr_act',
            'value_mdr_act')

# create combined variables for redistribution where necessary
merge_file[, value_com_act:=com_mobsoc + com_cause + com_radio + com_enf_ref + com_nom_touss]
merge_file[, value_screen_act:=tot_genexpert + dx_count + tb_tfc + perf_lab]
merge_file[, value_mdr_act:=dx_count + tot_genexpert]

# loop over financial variables and redistribute subnationally
for(i in seq_along(inVars)) {
  v = inVars[i]
  a = actVars[i]
  
  # disallow zeroes
  min = min(merge_file[get(a)>0][[a]], na.rm=TRUE)
  merge_file[, mean:=mean(get(a), na.rm=TRUE), by=region]
  merge_file[is.na(mean), mean:=min]
  
  # distribute proportionally
  merge_file[, tmp:=get(a)+min]
  merge_file[is.na(tmp), tmp:=mean]
  merge_file[, prop:=tmp/sum(tmp), by=date]
  merge_file[, (v):=get(v)*prop]
  
  # test that it worked
  orig = sum(resource_tracking[[v]], na.rm=TRUE)
  new = sum(merge_file[[v]], na.rm=TRUE)
  if (abs(orig-new)>.1) stop(i) 
}	
# --------------------------------------------------------------------------


# ----------------------------------------------------------
# Finish and save 

# drop unnecessary variables
merge_file = merge_file[, -c('mean','tmp','prop','value_com_act', 'value_com_act', 'value_mdr_act')]
# need to add merge variables back in

# save
saveRDS(merge_file, outputFile3)

# save a time-stamped version for reproducibility
archive(outputFile3)
# ----------------------------------------------------------
