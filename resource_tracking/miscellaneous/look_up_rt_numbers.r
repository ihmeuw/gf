# Trash code to look things up for the reports
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/'
inFile = paste0(dir, 'total_resource_tracking_data.csv')
data = fread(inFile)
data = data[country=='Congo (Democratic Republic)' & disease=='malaria' & data_source=='fpm' & year>2017]

agg = data[, .(budget=sum(budget), prs=paste0(unique(grant_number), collapse=', ')), by='gf_module']
agg[order(-budget)][1:10]

agg = data[, .(budget=sum(budget), modules=paste0(unique(gf_module), collapse=', '), prs=paste0(unique(grant_number), collapse=', ')), by='gf_intervention']
agg[order(-budget)][1:10]

agg = data[, .(budget=sum(budget), modules=paste0(unique(gf_module), collapse=', '), interventions=paste0(unique(gf_intervention), collapse=', '), prs=paste0(unique(grant_number), collapse=', ')), by='sda_activity']
agg[order(-budget)][1:10]


data = fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv")
dah = data[country=='Congo (Democratic Republic)' & disease=='malaria' & data_source=='fgh' & year>2014 & fin_data_type=='model_estimates']
gf = data[country=='Congo (Democratic Republic)' & disease=='malaria' & data_source=='fpm' & year>2014]

gf[, window:=ifelse(year<2018, '2015-2017','2018-2020')]

dah[, sum(disbursement), by='year']
gf[, sum(budget), by='year']
gf[, sum(budget), by=c('window')]
comp = dcast(gf[, sum(budget), by=c('window', 'abbrev_module')], abbrev_module~window)
comp[,diff:=get('2018-2020')-get('2015-2017')]
comp[is.na(diff) & !is.na(get('2018-2020')), diff:=get('2018-2020')-0]
comp[is.na(diff) & !is.na(get('2015-2017')), diff:=0-get('2015-2017')]
comp[order(diff)]

dah = data[country=='Congo (Democratic Republic)' & disease=='malaria' & data_source=='fgh' & year%in%c(2017,2018) & fin_data_type=='model_estimates']
gf = data[country=='Congo (Democratic Republic)' & disease=='malaria' & data_source=='fpm' & year%in%c(2017,2018)]

sum(dah$disbursement)
sum(gf$budget)
