# model: sen_tb_model1
# purpose: intended to be version 1 of the model (both halves combined into one)

model = '
    # linkage 1 regressions
    
    tb_vih_cumulative ~ lag_exp_T2_cumulative
    
    com_mobsoc_cumulative ~ lag_exp_T1_5_cumulative
    com_vad_touss_cumulative ~ lag_exp_T1_5_cumulative
    com_radio_cumulative ~ lag_exp_T1_5_cumulative
    
    perf_lab ~ lag_other_dah_T1_1_cumulative + lag_exp_T1_1_cumulative

    # linkage 2 regresions
    tb_vih_arv_cumulative ~ tb_vih_cumulative
    com_enf_ref_cumulative ~ com_vad_touss_cumulative
    com_nom_touss_cumulative ~ com_vad_touss_cumulative
    tb_cas_id_cumulative ~ com_mobsoc_cumulative + com_vad_touss_cumulative + com_radio_cumulative + perf_lab + lag_other_dah_T1_1_cumulative + lag_exp_T1_1_cumulative + com_nom_touss_cumulative
    tot_genexpert_cumulative ~ com_nom_touss_cumulative + lag_other_dah_T1_1_cumulative + lag_exp_T1_1_cumulative + patients_prop_genexpert_cumulative + perf_lab
    dx_count_cumulative ~ patients_prop_genexpert_cumulative + tot_genexpert_cumulative
    patients_prop_genexpert_cumulative ~ lag_other_dah_T3_cumulative + lag_exp_T3_cumulative
    
    # linkage 3 regressions
    
    tb_tfc_cumulative ~ tb_cas_id_cumulative + tot_genexpert_cumulative
    
    # linkage 4 regressions
    
    lead_gueris_taux ~ tb_vih_arv_cumulative + com_mobsoc_cumulative + com_vad_touss_cumulative + com_radio_cumulative
    lead_tpm_chimio_enf_cumulative ~ com_enf_ref_cumulative
    mdr_success_rate ~ dx_count_cumulative
    
    # fixed variances
    
    # covariances

    # latent variable
'