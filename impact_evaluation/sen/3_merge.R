# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: 3. Merge prepped data for Sen TB Model
# DATE: Auguts 15, 2019
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

source('./impact_evaluation/sen/set_up_r.r')

# ----------------------------------------------------------
# Load data

# Read in the previously saved file for outputs/outcomes in 2a
outputs_outcomes <- readRDS(outputFile2a)

# Read in the previously saved files for resource tracking in 2b
resource_tracking <- readRDS(outputFile2b)

# Read in the previously saved file for outcomes for tb_mdr in 2c
tb_mdr <- readRDS(outputFile2c)

# -----------------------------------------------------------------------------------------------------
# Prep data on outputs_outcomes
# ----------------------------------------------------------

# add date variable
outputs_outcomes = outputs_outcomes[, date:=annee+((trimestre-1)/4)]

# keep only variables that are relevant for the model
sub_data = outputs_outcomes[,.(region, centre, date, type,
                                 com_mobsoc, com_cause, com_radio, com_enf_ref, com_nom_touss, 
                                 tb_tfc, perf_lab, 
                                 ntr_all,
                                 ntr_rhz, ntr_erhz, ntr_serhz, ntr_cpx,
                                 tot_genexpert, tot_confirme, tot_res,
                                 tbtot_taux_det,
                                 gueris_total, gueris_taux, trait_tot, trait_pc, tb_vih_arv,
                                 tpm_chimio_enf, tpm_chimio_pvvih, tbvih_arvtx_rate
                               )]

# aggregate data to the admin1 (Regions in Senegal) ----------------------------------------------------------

# VALUES WHICH CAN BE SUMMED (counts)
summed_data = sub_data[, lapply(.SD, sum, na.rm=TRUE), by=c('region','date'), .SDcols=c('tb_tfc', 'ntr_all',
                                                                            'ntr_rhz', 'ntr_erhz', 'ntr_serhz', 'ntr_cpx', 
                                                                            'tot_genexpert', 'tot_confirme', 'tot_res',
                                                                            'gueris_total', 'trait_tot', 'tb_vih_arv', 'tbvih_arvtx_rate')]

# VALUES WHICH MUST BE AVERAGED (percents or rates)
averaged_data = sub_data[, lapply(.SD, mean, na.rm=TRUE), by=c('region','date'), .SDcols=c('gueris_taux', 'trait_pc')]

# VALUES WHICH MUST BE DIVIDED EVENLY (data reported in annual totals only)
divided_data = sub_data[, lapply(.SD, mean, na.rm=TRUE), by=c('region', 'date'), .SDcols=c('com_mobsoc', 'com_cause', 'com_radio', 'com_enf_ref', 'com_nom_touss')]
divided_data = divided_data[, lapply(.SD, function(x){x*0.25}), by=c('region', 'date'), .SDcols=c('com_mobsoc', 'com_cause', 'com_radio', 'com_enf_ref', 'com_nom_touss')]

# VALUES WHICH ARE NOT REPORTED BY HOSPITALS and MUST BE AVERAGED
exc_data = sub_data[type!="HOPITAL", lapply(.SD, mean, na.rm=TRUE), by=c('region','date'), .SDcols=c('perf_lab', 'tbtot_taux_det')]

# VALUES WHICH ARE NOT REPORTED BY HOSPITALS and can be SUMMED
exc_summed_data = sub_data[type!="HOPITAL", lapply(.SD, sum, na.rm=TRUE), by=c('region','date'), .SDcols=c('tpm_chimio_enf', 'tpm_chimio_pvvih')]

# merge properly formatted outputs data
data1 <- merge(summed_data, averaged_data, by = c("region", "date"), all = TRUE)
data2 <- merge(data1, divided_data, by = c("region", "date"), all = TRUE)
data3 <- merge(data2, exc_data, by = c("region", "date"), all = TRUE)
outputs_prepped <- merge(data3, exc_summed_data, by = c("region", "date"), all = TRUE)


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
inVars = c('other_dah_T1',
           'exp_T1',
           'exp_T2',
           'exp_T3',
           'other_dah_T3')

# list corresponding variables to define distribution proportions
actVars = c('value_prev_act',
            'value_prev_act',
            'tb_vih_arv',
            'dx_count',
            'other_dah_T3')

# create combined variables for redistribution where necessary
merge_file[, value_prev_act:=com_mobsoc + com_cause + com_radio + tot_genexpert + ntr_all]


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
merge_file = merge_file[, -c('mean','tmp','prop','value_prev_act')]
# need to add merge variables back in

# save
saveRDS(merge_file, outputFile3)

# save a time-stamped version for reproducibility
archive(outputFile3)
# ----------------------------------------------------------
