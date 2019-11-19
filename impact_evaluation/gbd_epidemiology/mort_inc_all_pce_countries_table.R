# sources from impact_evaluation/gbd_epidemiology/synthesis_epidemiology_2019.R
# create a table of incidence and mortality to include in synthesis
# Caitlin O'Brien-Carelli
# 11/19/2019

# subset to summary causes
causes = c('HIV/AIDS', 'Malaria', 'Tuberculosis')

# get 2017 incidence rates
incidence = dt[measure=='Incidence' & year==2017 & metric=='Rate' & cause %in% causes & sex=='Both', .(val = round(val, 1)), by=.(location, cause)]
incidence = dcast(incidence, location~cause)

# get 2017 incidence rates
incidence_roc = dt[measure=='Incidence' & year==2017 & metric=='Rate' & cause %in% causes & sex=='Both', .(roc), by=.(location, cause)]
incidence_roc = dcast(incidence_roc, location~cause)
setnames(incidence_roc, c('location', 'hiv_roc', 'mal_roc', 'tb_roc'))

incidence = merge(incidence, incidence_roc, by='location')

#---------------------------------
# get 2017 mortality rates

# get 2017 incidence rates
mort = dt[measure=='Deaths' & year==2017 & metric=='Rate' & cause %in% causes & sex=='Both', .(val = round(val, 1)), by=.(location, cause)]
mort = dcast(mort, location~cause)

# get 2017 incidence rates
mort_roc = dt[measure=='Deaths' & year==2017 & metric=='Rate' & cause %in% causes & sex=='Both', .(roc), by=.(location, cause)]
mort_roc = dcast(mort_roc, location~cause)
setnames(mort_roc, c('location', 'hiv_roc', 'mal_roc', 'tb_roc'))

mort = merge(mort, mort_roc, by='location')

#---------------------------------
# create the table and export

synth_table = rbind(incidence, mort)

#--------------------------
# export to copy into report

write.csv(synth_table, paste0(dir, 'incidence_mortality_table_all_pce_countries.csv'))

#------------------------------------------------------