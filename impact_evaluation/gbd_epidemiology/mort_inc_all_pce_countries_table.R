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
incidence_roc = dt[measure=='Incidence' & year==2017 & metric=='Rate' & cause %in% causes & sex=='Both', .(roc = round(roc, 2)), by=.(location, cause)]
incidence_roc = dcast(incidence_roc, location~cause)
setnames(incidence_roc, c('location', 'hiv_roc', 'mal_roc', 'tb_roc'))

incidence = merge(incidence, incidence_roc, by='location')

#---------------------------------
# get 2017 mortality rates

# get 2017 incidence rates
mort = dt[measure=='Deaths' & year==2017 & metric=='Rate' & cause %in% causes & sex=='Both', .(val = round(val, 1)), by=.(location, cause)]
mort = dcast(mort, location~cause)

# get 2017 incidence rates
mort_roc = dt[measure=='Deaths' & year==2017 & metric=='Rate' & cause %in% causes & sex=='Both', .(roc = round(roc, 2)), by=.(location, cause)]
mort_roc = dcast(mort_roc, location~cause)
setnames(mort_roc, c('location', 'hiv_roc', 'mal_roc', 'tb_roc'))

mort = merge(mort, mort_roc, by='location')


#---------------------------
# pull in global trends

glob = fread(paste0(dir, 'ihme_gbd_incidence_deaths_global.csv'))

# calculate annualized rates of change in incidence - 2000 to 2017
global_inc = glob[measure=='Incidence', .(round(val, 1)), by=.(year, cause)]
global_inc = dcast(global_inc, cause~year)
setnames(global_inc, c('cause', 'y2000', 'y2017'))
global_inc[, roc:=round((log(y2017/y2000)/17), 2)]
global_inc[ ,y2000:=NULL]

# calculate annualized rates of change in mortaliy - 2000 to 2017
global_mort = glob[measure=='Deaths', .(round(val, 1)), by=.(year, cause)]
global_mort = dcast(global_mort, cause~year)
setnames(global_mort, c('cause', 'y2000', 'y2017'))
global_mort[, roc:=round((log(y2017/y2000)/17), 2)]
global_mort[ ,y2000:=NULL]

# reshape and merge
global_mort[ ,cause:=NULL]
global_mort = cbind(global_mort[1,], global_mort[2,], global_mort[3,])
global_mort[ ,location:='Global']

global_inc[ ,cause:=NULL]
global_inc = cbind(global_inc[1,], global_inc[2,], global_inc[3,])
global_inc[ ,location:='Global']

# merge them into the table
setnames(global_mort, c("HIV/AIDS", "hiv_roc", "Malaria", "mal_roc", "Tuberculosis", "tb_roc", "location"))
setnames(global_inc, c("HIV/AIDS", "hiv_roc", "Malaria", "mal_roc", "Tuberculosis", "tb_roc", "location"))
incidence = rbind(incidence, global_inc)
mort = rbind(mort, global_mort)

#---------------------------------
# create the table and export

synth_table = rbind(incidence, mort)

#--------------------------
# export to copy into report

write.csv(synth_table, paste0(dir, 'outputs/incidence_mortality_table_all_pce_countries.csv'))

#------------------------------------------------------
# hiv only table - compare to just hiv without hiv/tb

# incidence rates
hincidence = dt[measure=='Incidence' & metric=='Rate' & cause=="HIV/AIDS resulting in other diseases" & sex=='Both' & (year==2000 | year==2017), 
                .(val = round(val, 1)), by=.(location, year)]
hincidence = dcast(hincidence, location~year)
setnames(hincidence, c('location', 'y2000', 'y2017'))
hincidence[ ,roc:=round((log(y2017/y2000)/17), 2)]

# death rates
hdeath = dt[measure=='Deaths' & metric=='Rate' & cause=="HIV/AIDS resulting in other diseases" & sex=='Both' & (year==2000 | year==2017), 
                .(val = round(val, 1)), by=.(location, year)]
hdeath = dcast(hdeath, location~year)
setnames(hdeath, c('location', 'y2000', 'y2017'))
hdeath[ ,roc:=round((log(y2017/y2000)/17), 2)]

# merge and export
hiv_table = rbind(hincidence, hdeath)

#---------------------------
# export HIV alone

write.csv(hiv_table, paste0(dir, 'outputs/hiv_incidence_mortality_no_tb_table_all_pce_countries.csv'))

#-----------------------------------------------



