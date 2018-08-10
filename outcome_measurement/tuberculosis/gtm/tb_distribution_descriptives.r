# ------------------------------------------------------------------
# David Phillips
# 
# 8/9/2018
# Analysis of supply chain data
# The current working directory should be the root of dataPath below
# ------------------------------------------------------------------


# ------------------
# Clear memory
rm(list=ls())
# ------------------


# ------------------------------------------------------------------
# File paths (different whether the code is being run at ihme or ciesar)
at_ciesar = 0
if (at_ciesar) { 
	codePath = 'PCE/gf/'
	dataPathDistr = './PCE/Outcome Measurement Data/TUBERCULOSIS'
	outFile = paste0(dataPathDistr, '/TB Supply Chain.pdf')
} else { 
	codePath = 'C:/local/gf/'
	dataPathDistr = './GF/outcome_measurement/gtm/prepped_data/'
	outFile = paste0(dataPathDistr, '../visualizations/TB Supply Chain.pdf')
}
# ------------------------------------------------------------------


# ------------------------------------------------------------------------
# Requirements: 
source(paste0(codePath, 'core/GT_load_data.R'), encoding = 'UTF-8')
source(paste0(codePath, 'core/GT_helper_functions.R'), encoding = 'UTF-8')
# ------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Load/prep data

# Load data
tbdistr = fread(paste0(dataPathDistr, '/GTM-TB-distribution-2013-2018.csv'))

# trim white space
tbdistr[, Product:= trimws(Product)]

# Fix mispellings
tbdistr[Product == 'RIFAMPICINA, CAPSULA DE 300 MG.', Product:='RIFAMPICINA, TABLETA DE 300 MG.']
tbdistr[Product == 'RIFAMPICINA 100MG/5ML SUSPENSION FRASCO 120ML' , Product := 'RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML.']
tbdistr[Product == 'ISONIAZIDA, TAB LETA DE 300 MG.', Product:='ISONIAZIDA, TABLETA DE 300 MG.']
tbdistr[Product == 'PIRAZINAMIDA, TABLET DE 500 MG.', Product:='PIRAZINAMIDA, TABLETA DE 500 MG.']
tbdistr[Product == 'ESTREPTOMICINA, VIAL/FRSCO DE 1 GRAMO', Product:= 'ESTREPTOMICINA, VIAL/FRASCO DE 1 GRAMO']
tbdistr[Product == 'ESTREPTOMICINA, VIAL/FRSCO DE 1 GRAMO', Product:= 'ESTREPTOMICINA, VIAL/FRASCO DE 1 GRAMO']
tbdistr[, Product:=gsub('CICLOCERINA','CICLOSERINA',Product)]
tbdistr[, Medicine:=gsub('CICLOCERINA','CICLOSERINA',Medicine)]

# fix alternate name of pharma company
tbdistr[, Supplier:=gsub(', S.A.', '', Supplier)]

# load list of high priority municipalities
highpr = fread(paste0(dataPathDistr, '../../../mapping/gtm/high_priority_muni.csv'))
highpr = highpr[high_priority==1]

# format names to help find department codes
dptList = data.table(munisGT)[, c('NOMBRE__','COD_DEPT__')]
dptList[, NOMBRE__:=gsub('á','a',NOMBRE__)]
dptList[, NOMBRE__:=gsub('é','e',NOMBRE__)]
dptList[, NOMBRE__:=gsub('í','i',NOMBRE__)]
dptList[, NOMBRE__:=gsub('ó','o',NOMBRE__)]
dptList[, NOMBRE__:=gsub('ñ','n',NOMBRE__)]
highpr[adm2_name=='Guatemala City', adm2_name:='Guatemala']

# test to confirm there are no more munis on the high priority list that don't match
if ((length(highpr[!adm2_name %in% dptList$NOMBRE__]$adm2_name))>0) {
	stop('Some municipality names from the high-priority list don\'t match the official list!')
}

# find the department code for each high-priority municipality
highpr = merge(highpr, dptList, by.x='adm2_name', by.y='NOMBRE__')
highprDpt = unique(highpr[, c('high_priority','COD_DEPT__')])
tbdistr = merge(tbdistr, highprDpt, by.x='code_dept', by.y='COD_DEPT__', all.x=TRUE)

# finally make the priority variable
tbdistr[is.na(high_priority), high_priority:=0]
tbdistr[, priority:=ifelse(high_priority==1, 'High Priority Departments', 'Other Departments')]

# make a date variable
tbdistr[, date:=as.Date(paste('01',Month,Year,sep='-'), '%d-%m-%Y')]

# combine drug name and dosage
tbdistr[, regimen:=paste0(Medicine, ' (', MG, 'mg)')]

# make an indicator for guatemala city
tbdistr[, guatemala:=Department=='GUAT. CENTRAL']
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Set up to graph

# aggregate to high/low priority
byVars = c('date','regimen','priority','guatemala')
prAgg = tbdistr[, .('Amount'=sum(Amount)), by=byVars]
prAgg[guatemala==TRUE, priority:='Guatemala City']
prAgg$guatemala = NULL

# reshape aggregate wide to compute ratio
prAggWide = dcast.data.table(prAgg, date+regimen~priority)
setnames(prAggWide, c('date','regimen','guatemala','high','other'))
prAggWide[, ratio_high_other:=high/other]
prAggWide[, ratio_high_gtm_other:=(high+guatemala)/other]
prAggWide[!is.finite(ratio_high_other), ratio_high_other:=NA]
prAggWide[!is.finite(ratio_high_gtm_other), ratio_high_gtm_other:=NA]
prAggWide[, mean:=mean(ratio_high_other, na.rm=TRUE), by='regimen']
prAggWide[, mean_incl_gtm:=mean(ratio_high_gtm_other, na.rm=TRUE), by='regimen']

# aggregate by provider
byVars = c('date','regimen','Supplier')
supAgg = tbdistr[, .('Amount'=sum(Amount)), by=byVars]

# list of most common drug regimens
commonDrugs = c('ETAMBUTOL (400mg)', 'ETHIONAMIDA (250mg)', 'ISONIAZIDA (100mg)', 
				'ISONIAZIDA (300mg)', 'LEVOFLOXACINA (250mg)', 'PIRAZINAMIDA (500mg)', 
				'RIFAMPICINA (300mg)','CICLOSERINA (250mg)')
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Graphs about high vs low priority areas

# drugs by product over time among high/low priority departments
p1 = ggplot(prAgg[regimen %in% commonDrugs], aes(x=date, y=Amount, color=priority)) + 
		geom_line(alpha=.75) + 
		geom_point(alpha=.75) + 
		facet_wrap(~regimen, scales='free_y') + 
		labs(title='TB Drugs Distributed', y='Units Distributed', color='', x='Month') + 
		theme_bw() + 
		theme(legend.position=c(0.825, 0.175), legend.text=element_text(size=14),
			legend.background=element_rect(fill='white', colour=NA))

# drugs by product over time excluding two huge points
p2 = ggplot(prAgg[regimen %in% commonDrugs & Amount<150000], aes(x=date, y=Amount, color=priority)) + 
		geom_line(alpha=.75) + 
		geom_point(alpha=.75) + 
		facet_wrap(~regimen, scales='free_y') + 
		labs(title='TB Drugs Distributed', y='Units Distributed', color='', x='Month', caption='Two shipments >150,000 not shown') + 
		theme_bw() + 
		theme(legend.position=c(0.825, 0.175), legend.text=element_text(size=14),
			legend.background=element_rect(fill='white', colour=NA))

# ratio of high to low
p3 = ggplot(prAggWide[regimen %in% commonDrugs], aes(y=ratio_high_other, x=date)) + 
		geom_line(alpha=.75) + 
		geom_point(alpha=.75) + 
		geom_hline(aes(yintercept=1, color='1:1') , lty='dashed') + 
		geom_hline(data=prAggWide[regimen %in% commonDrugs], 
			aes(yintercept=mean, color='Average'), lty='twodash', size=1) + 
		facet_wrap(~regimen, scales='free_y') +
		scale_color_manual('', values=c('Average'='#a1d99b', '1:1'='grey65')) + 
		labs(title='Ratio of High-Priority Departments (Excluding Guatemala) to Other Departments', 
			y='Ratio', x='Month') + 
		theme_bw() + 
		theme(legend.position=c(0.825, 0.175), legend.text=element_text(size=14),
			legend.background=element_rect(fill='white', colour=NA))

# ratio of high to low including guatemala city
p4 = ggplot(prAggWide[regimen %in% commonDrugs], aes(y=ratio_high_gtm_other, x=date)) + 
		geom_line(alpha=.75) + 
		geom_point(alpha=.75) + 
		geom_hline(aes(yintercept=1, color='1:1') , lty='dashed') + 
		geom_hline(data=prAggWide[regimen %in% commonDrugs], 
			aes(yintercept=mean, color='Average'), lty='twodash', size=1) + 
		facet_wrap(~regimen, scales='free_y') + 
		scale_color_manual('', values=c('Average'='#a1d99b', '1:1'='grey65')) + 
		labs(title='Ratio of High-Priority Departments (Including Guatemala) to Other Departments', 
			y='Ratio', x='Month') + 
		theme_bw() + 
		theme(legend.position=c(0.825, 0.175), legend.text=element_text(size=14),
			legend.background=element_rect(fill='white', colour=NA))
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Graphs about Global Fund vs other suppliers

# basic graph
p5 = ggplot(supAgg[regimen %in% commonDrugs], aes(x=date, y=Amount, color=Supplier)) + 
		geom_line(alpha=.75) + 
		geom_point(alpha=.75) + 
		facet_wrap(~regimen, scales='free_y') + 
		labs(title='TB Drugs Distributed by Supplier', y='Units Distributed', color='', x='Month') + 
		theme_bw() + 
		theme(legend.position=c(0.85, 0.2), legend.text=element_text(size=10),
			legend.background=element_rect(fill='white', colour=NA))
# ---------------------------------------------------------------------------


# ---------------------------------
# Save graphs
pdf(outFile, height=5.5, width=10)
p1
p2
p3
p4
p5
dev.off()
# ---------------------------------
