# sources from impact_evaluation/gbd_epidemiology/synthesis_epidemiology_2019.R
# create a table of incidence and mortality to include in synthesis
# Caitlin O'Brien-Carelli
# 2/4/2020

#---------------------------------
# subset to summary causes
causes = c('HIV/AIDS', 'Malaria', 'Tuberculosis')

# get 2017 incidence rates
incidence = dt[measure=='Incidence' & year==2018 & metric=='Rate' & cause %in% causes & sex=='Both',
               .(val = round(val, 1)), by=.(location, cause)]
incidence = dcast(incidence, location~cause)

# get 2017 incidence rates
incidence_roc = dt[measure=='Incidence' & year==2018 & metric=='Rate' & cause %in% causes & sex=='Both',
                   .(roc = roc), by=.(location, cause)]
incidence_roc = dcast(incidence_roc, location~cause)
setnames(incidence_roc, c('location', 'hiv_roc', 'mal_roc', 'tb_roc'))

incidence = merge(incidence, incidence_roc, by='location')

#---------------------------------
# get 2017 mortality rates

# get 2017 incidence rates
mort = dt[measure=='Deaths' & year==2017 & metric=='Rate' & cause %in% causes & sex=='Both',
          .(val = round(val, 1)), by=.(location, cause)]
mort = dcast(mort, location~cause)

# get 2017 incidence rates
mort_roc = dt[measure=='Deaths' & year==2017 & metric=='Rate' & cause %in% causes & sex=='Both', .(roc = roc), by=.(location, cause)]
mort_roc = dcast(mort_roc, location~cause)
setnames(mort_roc, c('location', 'hiv_roc', 'mal_roc', 'tb_roc'))

mort = merge(mort, mort_roc, by='location')

# #---------------------------

#---------------------------------
# create the table and format the results for visualization

synth_table = rbind(incidence, mort)

#--------------------------
# export to copy into report

write.csv(synth_table, paste0(dir, 'outputs/', set, '_incidence_mortality_table_all_pce_countries.csv'))

#------------------------------------------------------
# hiv only table - compare to just hiv without hiv/tb
# incidence rates are identical - only include mortality

if (set=='gbd') {
hiv_dt = fread(paste0(dir, 'raw_data/gbd/ihme_age_standardized_2017_hiv_only.csv'))
hiv_dt = hiv_dt[age=='Age-standardized' & metric=='Rate' & measure=='Deaths']

#-------------------
# reset the order for the table
hiv_dt$location = factor(hiv_dt$location, c("Cambodia", "Democratic Republic of the Congo",
                "Guatemala", "Mozambique", "Myanmar", 
                "Senegal", "Sudan", "Uganda", "Global"), 
                     c("Cambodia", "DRC", "Guatemala", 
                       "Mozambique", "Myanmar", "Senegal", 
                       "Sudan", "Uganda", "Global Trend"))

#-------------------
# death rates
hdeath = hiv_dt[measure=='Deaths' & metric=='Rate' & cause=="HIV/AIDS resulting in other diseases" & sex=='Both' & (year==2000 | year==2017), 
                .(value = val), by=.(location, year)]
hdeath = dcast(hdeath, location~year)
setnames(hdeath, c('location', 'y2000', 'y2017'))
hdeath[ ,roc:=round((log(y2017/y2000)/17), 2)]

# merge and format
hdeath[ ,y2000:=round(y2000, 1)]
hdeath[ ,y2017:=round(y2017, 1)]
hdeath[ ,roc:=100*roc]
 
#---------------------------
# export HIV mortality separately from tb

write.csv(hdeath, paste0(dir, 'outputs/', set, '_hiv_only_mortality_table_all_pce_countries.csv')) }

#-----------------------------------------------
# export a treatment coverage table

if (set=='who_unaids') {
cv = readRDS(paste0(dir, 'prepped_data/', 
                    set, '_coverage_prepped.rds'))

# subset and display
cv = cv[year==2010 | year==2017, .(value = mean),
        by=.(cause, location, year)]
cv[ , variable:=paste0(cause, year)]

# shape wide 
cv = dcast(cv, location~variable)
write.csv(cv, paste0(dir, 'outputs/', set,
      '_coverage_table_all_pce_countries.csv'))
}

print("All done, kid!")


