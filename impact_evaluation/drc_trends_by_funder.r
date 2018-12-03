# --------------------------------------------------
# David Phillips
# 
# 10/11/2018
# Explore trends by funder
# --------------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(stringr)
library(ggplot2)
library(RColorBrewer)
# -------------------


# --------------------------------------------------
# Files and directories

# directory
dir = 'J:/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/'

# input file
inFile = paste0(dir, 'PNLP/post_imputation/imputedData_hz_withFunderData.rds')

# output file
outFile = paste0(dir, '../visualizations/funder_analysis.pdf')
# --------------------------------------------------


# ----------------------------------------------------------------------------------------
# Load/prep data

# load
allData = readRDS(inFile)
data = copy(allData)

# drop unidentified health zones
data = data[dps!='0']

# fill in missing funders when one month is filled in and the rest of the year is blank
# often, the first few months are filled in, but this appears to mean 'split between'
data[, year:=year(date)]
data[, tmp:=paste(unique(funder), collapse='/'), by=c('dps','health_zone','year')]
data[, tmp:=gsub('/NA', '', tmp)]
data[, tmp:=gsub('NA/', '', tmp)]
data[tmp!='', funder:=tmp]
data$tmp = NULL
data[funder=='NA', funder:=NA]

# condense funder names
extras = c('SA','CEMUBAC','RAISE','BPRM/PMI/GIRO-SVI/UNHCR',
			'CTB/ASSNIP','Gavi','MSF','MDM','CERF','CORDAID',
			'CARITAS','ECHO','NEANT','EU','SANS','KOICA','PARSS')
for(e in extras) data[, funder:=gsub(e, 'Others', funder)]
for(u in c('UNPD','UNFPA','UNICEF')) data[, funder:=gsub(u, 'UN Agencies', funder)]
data[funder=='GF/NMF', funder:='GF']

# clean up condensed names
data[, funder:=gsub('Others/GF', 'GF/Others', funder)]
data[, funder:=gsub('Others/PMI', 'PMI/Others', funder)]
data[, funder:=gsub('OthersNS', 'Others', funder)]
data[, funder:=gsub('Others/UN Agencies', 'UN Agencies/Others', funder)]
data[, funder:=gsub('Others/Others', 'Others', funder)]
data[, funder:=gsub('PMI/GF', 'GF/PMI', funder)]
data[, funder:=gsub('UN Agencies/GF', 'GF/UN Agencies', funder)]
data[, funder:=gsub('UN Agencies/Others/GF', 'GF/UN Agencies/Others', funder)]
data[, funder:=gsub('WB/GF', 'GF/WB', funder)]
# -------------------------------------------------------------------------------------


# --------------------------------------------------
# Explore completeness of the funder variable by year

# funder seems to go from 78% missing to 25% missing in 2014
pct_missing_year = data[variable=='ANC_1st', .(mean(is.na(funder))), by=year]
p1 = ggplot(pct_missing_year, aes(y=V1, x=year)) + 
	geom_bar(stat='identity') + 
	labs(title='Proportion of Health Zone-Months with Missing Funder Information',
		y='Proportion', x='') + 
	theme_bw(base_size=14)

# missingness by dps post 2014
pct_missing_dps = data[variable=='ANC_1st' & year>=2014, .(mean(is.na(funder))), by=dps]
p2 = ggplot(pct_missing_dps, aes(y=V1, x=reorder(dps, -V1))) + 
	geom_bar(stat='identity') + 
	labs(title='Proportion of Health Zone-Months with Missing Funder Information',
		subtitle='Excluding Data before 2014', y='Proportion', x='') + 
	theme_bw(base_size=14) + 
	theme(axis.text.x=element_text(angle=315, hjust=0))

# missingness by dps post 2014
pct_missing_dps_year = data[variable=='ANC_1st' & year>=2014, .(mean(is.na(funder))), by=c('dps','year')]
p3 = ggplot(pct_missing_dps_year, aes(y=V1, x=year)) + 
	geom_line() + 
	facet_wrap(~dps) + 
	labs(title='Proportion of Health Zone-Months with Missing Funder Information',
		subtitle='Excluding Data before 2014', y='Proportion', x='') + 
	theme_bw(base_size=14) + 
	theme(axis.text.x=element_text(angle=315, hjust=0))

# --------------------------------------------------


# --------------------------------------------------
# Finish cleaning data

# subset to post-2014
data = data[year>=2014]

# collapse out subpopulations 
subpops = c('14yrsAndOlder', '1to5yrs', '2to11mos', '6to13yrs', 
		'under5', '5andOlder', 'Under5', '0to11mos', 'pregnantWomen')
for(s in subpops) data[, variable:=gsub(s, '', variable)]
byVars = c('dps','health_zone','date','variable','funder')
data = data[, .(mean=sum(mean)), by=byVars]
data[str_sub(variable,-1)=='_', variable:=str_sub(variable,1,-2)]

# identify missing funders
data[is.na(funder), funder:='Not Specified']

# aggregate to funder level
byVars = c('date','variable','funder')
aggF = data[, .(mean=sum(mean)), by=byVars]
aggF = dcast.data.table(aggF, date+funder~variable)
# --------------------------------------------------


# --------------------------------------------------
# Set up to graph

# mapped colors
Paired = brewer.pal(12,'Paired')
colors = c('GF'='black', 'PMI'=Paired[1], 'GF/PMI'=Paired[2],
		'WB'=Paired[3], 'GF/WB'=Paired[4], 
		'DFID'=Paired[5], 'Others'=Paired[6], 'Not Specified'='grey55')
	
# order of funders
o = rev(names(colors))
# --------------------------------------------------


# --------------------------------------------------
# Analyze indicators by funder (including 'missing')

# fraction of bednets
p4 = ggplot(aggF, aes(y=ITN_received, x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='fill') + 
	scale_fill_manual(values=colors) + 
	labs(title='Bednets Distributed to Facilities', 
		y='Fraction of All ITNs Distributed', x='', fill='Funder') + 
	theme_bw()

# number of bednets
p4n = ggplot(aggF, aes(y=ITN_received, x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='stack') + 
	scale_fill_manual(values=colors) + 
	labs(title='Bednets Distributed to Facilities', 
		y='Number of ITNs Distributed', x='', fill='Funder') + 
	theme_bw()

# fraction of RDTs conducted
p5 = ggplot(aggF, aes(y=RDT_completed, x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='fill') + 
	scale_fill_manual(values=colors) + 
	labs(title='RDTs Conducted', 
		y='Fraction of All Cases Treated', x='', fill='Funder') + 
	theme_bw()

# number of RDTs conducted
p5n = ggplot(aggF, aes(y=RDT_completed, x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='stack') + 
	scale_fill_manual(values=colors) + 
	labs(title='RDTs Conducted', 
		y='Number of Cases Treated', x='', fill='Funder') + 
	theme_bw()

# fraction of community RDTs conducted
p6 = ggplot(aggF, aes(y=SSCRDT_completed, x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='fill') + 
	scale_fill_manual(values=colors) + 
	labs(title='RDTs Conducted by Community Health Workers', 
		y='Fraction of All Cases Treated', x='', fill='Funder') + 
	theme_bw()

# number of community RDTs conducted
p6n = ggplot(aggF, aes(y=SSCRDT_completed, x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='stack') + 
	scale_fill_manual(values=colors) + 
	labs(title='RDTs Conducted by Community Health Workers', 
		y='Number of Cases Treated', x='', fill='Funder') + 
	theme_bw()

# fraction of cases treated
p7 = ggplot(aggF, aes(y=(mildMalariaTreated+severeMalariaTreated), x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='fill') + 
	scale_fill_manual(values=colors) + 
	labs(title='Malaria Cases Treated with ACTs', 
		y='Fraction of All Cases Treated', x='', fill='Funder') + 
	theme_bw()

# number of cases treated
p7n = ggplot(aggF, aes(y=(mildMalariaTreated+severeMalariaTreated), x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='stack') + 
	scale_fill_manual(values=colors) + 
	labs(title='Malaria Cases Treated with ACTs', 
		y='Number of Cases Treated', x='', fill='Funder') + 
	theme_bw(base_size=18)

# fraction of community cases treated
p8 = ggplot(aggF, aes(y=SSCACT, x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='fill') + 
	scale_fill_manual(values=colors) + 
	labs(title='Malaria Cases Treated with ACTs by Community Health Workers', 
		y='Fraction of All Cases Treated', x='', fill='Funder') + 
	theme_bw()
	
# number of community cases treated
p8n = ggplot(aggF, aes(y=SSCACT, x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='stack') + 
	scale_fill_manual(values=colors) + 
	labs(title='Malaria Cases Treated with ACTs by Community Health Workers', 
		y='Number of Cases Treated', x='', fill='Funder') + 
	theme_bw()
	
# fraction of stockouts
p9 = ggplot(aggF, aes(y=(stockOutASAQ+stockOutSP+stockOutartLum), x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='fill') + 
	scale_fill_manual(values=colors) + 
	labs(title='Stockouts of ASAQ, SP or AL', 
		y='Fraction of All Stockouts', x='', fill='Funder') + 
	theme_bw()

# number of stockouts
p9n = ggplot(aggF, aes(y=(stockOutASAQ+stockOutSP+stockOutartLum), x=date, fill=factor(funder, o))) + 
	geom_bar(stat='identity',position='stack') + 
	scale_fill_manual(values=colors) + 
	labs(title='Stockouts of ASAQ, SP or AL', 
		y='Number of Stockouts', x='', fill='Funder') + 
	theme_bw()

# ITNs per suspected case
p10 = ggplot(aggF, aes(y=ITN_received/suspectedMalaria, x=date, color=factor(funder, o))) + 
	geom_line() + 
	geom_point() + 
	scale_color_manual(values=colors) + 
	labs(title='ITNs Distributed per Suspected Case', 
		y='ITNs Distributed per Suspected Case', x='', color='Funder') + 
	theme_bw()

# RDTs per suspected case
p11 = ggplot(aggF, aes(y=RDT_completed/suspectedMalaria, x=date, color=factor(funder, o))) + 
	geom_line() + 
	geom_point() + 
	scale_color_manual(values=colors) + 
	labs(title='RDTs Completed per Suspected Case', 
		y='RDTs Completed per Suspected Case', x='', color='Funder') + 
	theme_bw()

# cases treated
p12 = ggplot(aggF, aes(y=(mildMalariaTreated+severeMalariaTreated)/(newCasesMalariaMild+newCasesMalariaSevere), x=date, color=factor(funder, o))) + 
	geom_line() + 
	geom_point() + 
	scale_color_manual(values=colors) + 
	labs(title='Cases Treated per Case Confirmed', 
		y='Cases Treated per Case Confirmed', x='', color='Funder') + 
	theme_bw()
# --------------------------------------------------


# --------------------------------------------------
# Save
pdf(outFile, height=5.5, width=9)
p4
p4n
p5
p5n
p6
p6n
p7
p7n
p8
p8n
p9
p9n
p1
p2
p3
p10
p11
p12
dev.off()
# --------------------------------------------------
