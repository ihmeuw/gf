library(data.table)
library(ggplot2)
data = fread("C:/local/GF_copy_01032019/results_chains/cod/malaria/prepped_data/IHME-GBD_2017_DATA-8a42301c-1.csv")

data = data[(measure=='Incidence' & metric=='Number') | (measure=='Prevalence' & metric=='Percent') | (measure=='Deaths' & metric=='Rate')]

data[measure=='Incidence', val:=val/1000000]
data[measure=='Incidence', upper:=upper/1000000]
data[measure=='Incidence', lower:=lower/1000000]

data[measure=='Prevalence', val:=val*100]
data[measure=='Prevalence', upper:=upper*100]
data[measure=='Prevalence', lower:=lower*100]

data[measure=='Deaths', measure:='Deaths (rate per 100k)']
data[measure=='Incidence', measure:='Incidence (millions of cases)']
data[measure=='Prevalence', measure:='Prevalence (percentage)']

data[, measure:=factor(data$measure, levels=unique(data$measure), ordered=TRUE)]

ggplot(data, aes(y=val, ymin=lower, ymax=upper, x=year, color=measure)) + 
	geom_ribbon(fill='grey85', color=NA) + 
	geom_line(size=1.1) + 
	geom_point() + 
	facet_wrap(~measure, scales='free') + 
	labs(y='', x='', caption='Grey area reflects model uncertainty') + 
	theme_bw(base_size=16) + 
	theme(legend.position="none") 
	