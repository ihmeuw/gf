# -------------------------------------------
# David Phillips and Audrey Batzel
# 
# 1/31/2019
# Exploratory graphs comparing SNIS and PNLP
# -------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(gridExtra)
# -------------------


# -------------------------------------------
# Files and directories

# root directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/impact_evaluation/cod/')

# input file
inFile = paste0(dir, 'prepped_data/snis_pnlp_malaria_hz_level.rds')

# output file
outFile = paste0(dir, 'visualizations/SNIS_PNLP_compare/snis_pnlp_comparisons_natl.pdf')
outFile2 = paste0(dir, 'visualizations/SNIS_PNLP_compare/snis_pnlp_comparisons_dps.pdf')
outFile3 = paste0(dir, 'visualizations/SNIS_PNLP_compare/snis_pnlp_comparisons_dps_scatterplot.pdf')
# -------------------------------------------


# ---------------------------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(inFile)

# clean up dataset names
data = droplevels(data)
data[grepl('SIGL1',data_set), data_set:='sigl1']
data[grepl('SIGL2',data_set), data_set:='sigl2']
data[grepl('base',data_set), data_set:='base_services']

# identify indicators that are in both
datasets = unique(data[,c('indicator','data_set')])
datasets = dcast.data.table(datasets, indicator~data_set)
overlapping_indicators = datasets[!is.na(pnlp) & 
	(!is.na(sigl1) | !is.na(sigl2) | !is.na(base_services))]
	
# keep only indicators that are in both
data = data[indicator %in% overlapping_indicators$indicator]

# collapse subpopulations where necessary to get both to match
data[data_set=='base_services' & indicator=='LLIN' & 
	subpopulation %in% c('distAtANC1', 'distAtANC2'), subpopulation:='distAtANC'] # note this comparison is comparing LLIN distAtANC in both, but in the past we have used the sum of PNLP LLIN vars compared to SIGL LLIN consumed 
data[data_set=='sigl1' & indicator=='ArtLum' & 
	subpopulation %in% c('consumed(240+40)', 'consumed(480+80)'), subpopulation:='used']
idVars = names(data)[names(data)!='value']
data = data[, .(value=sum(value)), by=idVars]
	
# drop subpopulations that don't have an equivalent
idVars = idVars[idVars!='data_set']
form = as.formula(paste(paste(idVars, collapse='+'),'~data_set'))
data = dcast.data.table(data, form)
data = data[!is.na(pnlp) & (!is.na(sigl1) | !is.na(base_services))]

# combine sigl and base (test first)
if (nrow(data[!is.na(sigl1) & !is.na(base_services)])>0) { 
	stop('SIGL and Base Services overlap still, decide which to use') 
}
data[, snis:=ifelse(is.na(sigl1), base_services, sigl1)]
data$sigl1 = NULL
data$base_services = NULL

# reshape long again
long = melt(data, id.vars=idVars, variable.name='data_set')
# ---------------------------------------------------------------------------------------


# -------------------------------------------
# Set up to graph

# make national aggregate
byVars = c('date','indicator','subpopulation','data_set')
nat = long[, .(value=sum(value,na.rm=TRUE)), by=byVars]
byVars = byVars[byVars!='data_set']
natwide = data[, .(pnlp=sum(pnlp,na.rm=TRUE), snis=sum(snis,na.rm=TRUE)), by=byVars]

# make dps aggregate
byVars = c('dps','date','indicator','subpopulation','data_set')
dps = long[, .(value=sum(value,na.rm=TRUE)), by=byVars]
byVars = byVars[byVars!='data_set']
dpswide = data[, .(pnlp=sum(pnlp,na.rm=TRUE), snis=sum(snis,na.rm=TRUE)), by=byVars]
# -------------------------------------------


# -------------------------------------------
# Graph at national level

# comparison time series graphs
natTs = list()
indicators = unique(data$indicator)
indicators = indicators[order(indicators)]
for (p in seq_along(indicators)) { 
	i = indicators[p]
	natTs[[p]] = ggplot(nat[indicator==i], 
		aes(y=value, x=date, linetype=data_set, color=subpopulation)) + 
			geom_point(size=1.25, alpha=.5) + 
			geom_line(size=.5) + 
			facet_wrap(~indicator, scales='free_y') + 
			labs(y=NULL, x=NULL, linetype='Source', color=NULL) + 
			theme_bw(base_size=9) + 
			theme(strip.text=element_text(size=11)) 
}

# comparison scatterplots
natCs = ggplot(natwide, aes(y=snis, x=pnlp, color=subpopulation)) + 
	geom_abline(slope=1, intercept=0) + 
	geom_point(alpha=.5) + 
	facet_wrap(~indicator, scales='free') + 
	labs(y='SNIS', x='PNLP', color=NULL) + 
	theme_bw(base_size=14)
# -------------------------------------------


# -------------------------------------------
# Save
pdf(outFile, height=5.5, width=10)
do.call('grid.arrange', natTs)
natCs
dev.off()
# -------------------------------------------


# -------------------------------------------
# Graph at DPS level

# currently a problem with Ituri - drop out for now
dps = dps[dps!="ituri", ]
dpswide = dpswide[dps != "ituri", ]

# comparison time series graphs
list_of_dpsTs = list()
indicators = unique(data$indicator)
indicators = indicators[order(indicators)]
indicators = indicators[!indicators %in% c("ArtLum")]

for (x in seq_along(unique(dps$dps))){
  dpsTs = list()
  d = unique(dps$dps)[x]
  for (p in seq_along(indicators)) { 
    i = indicators[p]
    if (p == 1){
      dpsTs[[p]] = 
        ggplot(dps[indicator==i & dps == d,], 
                          aes(y=value, x=date, linetype=data_set, color=subpopulation)) + 
        geom_point(size=1.25, alpha=.5) + 
        geom_line(size=.5) + 
        facet_wrap(~indicator, scales='free_y') + 
        labs(y=NULL, x=NULL, linetype='Source', color=NULL) + 
        theme_bw(base_size=9) + 
        theme(strip.text=element_text(size=11)) +
        ggtitle( d )   } else {
      dpsTs[[p]] = ggplot(dps[indicator==i & dps == d,], 
                        aes(y=value, x=date, linetype=data_set, color=subpopulation)) + 
      geom_point(size=1.25, alpha=.5) + 
      geom_line(size=.5) + 
      facet_wrap(~indicator, scales='free_y') + 
      labs(y=NULL, x=NULL, linetype='Source', color=NULL) + 
      theme_bw(base_size=9) + 
      theme(strip.text=element_text(size=11))
    }
  list_of_dpsTs[[x]] = dpsTs
  }
}

# comparison scatterplots
dpsCs = ggplot(dpswide, aes(y=snis, x=pnlp, color=subpopulation)) +
  geom_abline(slope=1, intercept=0) +
  geom_point(alpha=.5, size = 0.3) +
  facet_wrap(~indicator, scales='free') +
  labs(y='SNIS', x='PNLP', color=NULL) +
  theme_bw(base_size=14)
# -------------------------------------------


# -------------------------------------------
# Save
pdf(outFile2, height=5.5, width=10)
for (i in seq_along(list_of_dpsTs)) {
  do.call('grid.arrange', list_of_dpsTs[[i]])
}
dpsCs
dev.off()
# -------------------------------------------