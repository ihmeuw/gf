# model: sen_tb_model1
# purpose: intended to be version 1 of first-half model with data currently available

model = '
    # linkage 1 regressions
    com_mobsoc_cumulative ~ lag_other_dah_T1_cumulative + lag_exp_T1_cumulative
    com_cause_cumulative ~ lag_other_dah_T1_cumulative + lag_exp_T1_cumulative
    com_radio_cumulative ~ lag_other_dah_T1_cumulative + lag_exp_T1_cumulative
    
    tot_genexpert_cumulative ~ lag_other_dah_T1_cumulative + lag_exp_T1_cumulative

    # linkage 2 regresions
    
    com_enf_ref_cumulative ~ com_mobsoc_cumulative + lag_exp_R2_cumulative + lag_exp_R3_cumulative
    com_nom_touss_cumulative ~ com_mobsoc_cumulative + com_cause_cumulative + com_radio_cumulative + lag_exp_R2_cumulative + lag_exp_R3_cumulative
    tb_tfc_cumulative ~ com_mobsoc_cumulative + tot_genexpert_cumulative + lag_exp_R2_cumulative + lag_exp_R3_cumulative + perf_lab
    
    perf_lab ~ lag_exp_R2_cumulative + lag_exp_R3_cumulative
    ntr_rhz_cumulative ~ lag_exp_R2_cumulative + lag_exp_R3_cumulative + com_mobsoc_cumulative
    
    #mdr_tb_dx_cumulative ~ lag_exp_T3_cumulative
    #mdr_tb_tx_cumulative ~ lag_exp_T3_cumulative
    
    tb_vih_arv_cumulative ~ lag_exp_T2_cumulative
    
    # fixed variances
    
    # covariances

    # latent variable
'


