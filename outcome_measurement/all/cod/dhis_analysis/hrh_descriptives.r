# -----------------------------------
# David Phillips
# 
# 10/2/2018
# Descriptive graphs of human resources from DHIS
# -----------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(quantreg)
library(stringr)
library(ggplot2)
library(scales)
library(RColorBrewer)
# ------------------


# --------------------------------------------------------------
# Parameters and settings

# whether to re-prep all the data or just load the interim file
reprep_data = FALSE
# --------------------------------------------------------------


# ------------------------------------------------------------------------
# Files and directories

# switch for the cluster
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')

# data directory
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

# codebook file
codebookFile = paste0(dir, 'catalogues/data_elements_cod.csv')

# input file where the given variables can be found
inFile = paste0(dir, 'pre_prep/merged/base_services_drc_01_2015_09_2018.rds')

# interim file to avoid reloading
interimFile = paste0(dir, 'prepped/hrh_data.rds')

# output files
outFileFacilities = paste0(dir, '../visualizations/snis_hrh_facilities.pdf')
outFileOutliers = paste0(dir, '../visualizations/snis_hrh_outliers.pdf')
outFile = paste0(dir, '../visualizations/snis_hrh.pdf')
# ------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------
# Load/prep data

# load codebook
codebook = fread(codebookFile)

# identify stockout variables
hrhCodebook = codebook[grepl('agent', tolower(iconv(element)))]
variables = hrhCodebook$element_id

# load basic services data (warning: slow)
if(reprep_data) { 
	data = readRDS(inFile)

	# subset to the specified variable(s) post 2016
	data = data[element_id %in% variables & year>=2017]

	# clean up cadre names
	data[element=='A 4.7 Médecin généraliste Agents', cadre:='General Practitioner']
	data[element=='A 4.7 Infirmier A2 Agents', cadre:='Nurse (A1, A2 or L2)']
	data[element=='A 4.7 Technicien de labo A2/A1/L2 - agents', cadre:='Lab Technician']
	data[element=='A 4.7 Autre personnel Agents', cadre:='Other Staff']
	data[element=='A 4.7 Infirmier A1 Agents', cadre:='Nurse (A1, A2 or L2)']
	data[element=='A 4.7 Infirmier L2 Agents', cadre:='Nurse (A1, A2 or L2)']
	data[element=='A 4.7 Accoucheur(se)/Sage-femme - agents', cadre:='Midwife']
	data[element=='A 4.7 Nutritionnistes A2 /A1/ L2 Agents', cadre:='Nutritionist']

	# collapse
	data[, value:=as.numeric(as.character(value))]
	byVars = c('date','org_unit','dps','health_zone','health_area','level','cadre')
	data = data[, .(value=sum(value)), by=byVars]

	# exclude August 2018
	data = data[date<'2018-08-01']

	# clean up province names
	data[, dps:=str_sub(dps,4)]
	data[, dps:=gsub(' Province', '', dps)]

	# clean up level names
	data[, level:=gsub('_', ' ', level)]
	data[, level:=str_to_title(level)]

	# save interim file
	saveRDS(data, interimFile)
}
if (!reprep_data) data = readRDS(interimFile)
# ---------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Remove outliers

# set up
idVars = c('date','org_unit','health_zone','cadre','dps','health_area','level','candidate')
outlierPlots = list()

# loop over levels
i=1
for(l in unique(data$level)) {
	print(l)
	
	# subset
	if (!is.na(l)) tmp = data[level==l]
	if (is.na(l)) tmp = data[is.na(level)]
	facs = unique(tmp$org_unit)
	s = seq(length(facs))
	
	# run chunks of facilities
	chunkSize = 50
	chunks = cut_interval(s, length=chunkSize, labels=F)
	for(c in unique(chunks)) { 
		
		# subset to chunk 
		tmp2 = tmp[org_unit %in% facs[which(chunks==c)]]
		
		# remove facilities with <=2 months
		tmp2[, n_months:=length(unique(date)), by=c('org_unit','cadre')]
		tmp2 = tmp2[n_months>2]
		
		# skip if the whole chunk was <=2 months
		if (nrow(tmp2)==0) next 
		
		# mean-center to avoid fixed effects
		tmp2[, mean:=mean(value), by=c('org_unit','cadre')]
		tmp2[, value_mc:=value-mean]
		
		# run regression
		lmFit = rq(value_mc ~ date, data=tmp2)
		
		# identify candidates
		tmp2[, pred:=predict(lmFit)]
		tmp2[, resid:=resid(lmFit)]
		tmp2[, median:=median(resid)]
		tmp2[, sd:=sd(resid)]
		f = 14
		tmp2[, candidate:= resid>(median+(f*sd)) | resid<(median-(f*sd))]
		candidateFacilities = unique(tmp2[candidate==TRUE]$org_unit)
		if (length(candidateFacilities)==0) next
		if (i==1) candidateOutliers = tmp2[candidate==TRUE, idVars, with=FALSE]
		if (i>1) candidateOutliers = rbind(candidateOutliers, tmp2[candidate==TRUE, idVars, with=FALSE])
		
		# graph
		outlierPlots[[i]] = ggplot(tmp2[org_unit %in% candidateFacilities], aes(y=value,x=date)) + 
			geom_line(aes(color=cadre)) + 
			geom_point(color='black') + 
			geom_point(data=tmp2[candidate==TRUE], color='red') + 
			geom_line(aes(y=pred+mean, color=cadre), linetype='dashed') + 
			facet_wrap(~org_unit, scales='free') + 
			labs(title=paste0('Detected Outliers among Facilities ', 
					(c*chunkSize)-chunkSize, '-', c*chunkSize, ' (chunk ', c, ')'), 
				subtitle=paste('Level:', l) , y='Number of Health Care Workers', x='', 
				color='Cadre', caption='Dashed lines: linear fit\nRed points: candidate outliers') + 
			theme_bw()
		i=i+1
	}
}

# remove
data = merge(data, candidateOutliers, by=idVars[idVars!='candidate'], all.x=TRUE)
data[, original_value:=value]
data[candidate==TRUE, value:=NA]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Aggregate

# national level
aggN = data[, .(value=sum(value, na.rm=TRUE), mean=mean(value,na.rm=TRUE), median=median(value,na.rm=TRUE),
			q25=quantile(value, .25, na.rm=TRUE), q75=quantile(value, .75, na.rm=TRUE),
			n_facilities=length(unique(org_unit))), by=c('date','cadre')]
			
# dps level
aggD = data[, .(value=sum(value, na.rm=TRUE), mean=mean(value,na.rm=TRUE), median=median(value,na.rm=TRUE), 
			n_facilities=length(unique(org_unit))), by=c('dps', 'date','cadre')]

# health facility level
aggL = data[, .(value=sum(value, na.rm=TRUE), mean=mean(value,na.rm=TRUE), median=median(value,na.rm=TRUE), 
			n_facilities=length(unique(org_unit))), by=c('level', 'date','cadre')]
# -------------------------------------------------------------------------


# ---------------------------------------------
# Set up to graph
c = brewer.pal(12, 'Paired')
colors = c('General Practitioner'=c[1], 'Nurse (A1, A2 or L2)'=c[4], 
		'Lab Technician'=c[8], 'Midwife'=c[10], 'Nutritionist'=c[6], 
		'Other Staff'=c[12])
sizeRange1 = c(.5,3.5)
sizeRange2 = c(.5,1.5)
# ---------------------------------------------


# ------------------------------------------------------
# Graph counts

# national counts
countPlots = list()
i=1
countPlots[[i]] = ggplot(aggN, aes(y=value/1000, x=date, color=cadre)) + 
	geom_line() + 
	geom_point(aes(size=n_facilities)) + 
	scale_color_manual(values=colors) + 
	scale_size_continuous(range=sizeRange1) + 
	labs(title='National-Level Counts', y='Number of Health Care Workers (in Thousands)', 
		x='', color='Cadre', size='Number of\nFacilities\nReporting') + 
	theme_bw()

# level counts
i=i+1
countPlots[[i]] = ggplot(aggL, aes(y=value, x=date, color=cadre)) + 
	geom_line() + 
	geom_point(aes(size=n_facilities)) + 
	scale_color_manual(values=colors) + 
	scale_size_continuous(range=sizeRange2) + 
	facet_wrap(~level, scales='free_y') + 
	labs(title='Facility Platform Counts', y='Number of Health Care Workers', 
		x='', color='Cadre', size='Number of\nFacilities\nReporting') + 
	theme_bw()

# dps counts
dpss = unique(aggD$dps)
chunkSize=9
chunks = cut_interval(seq(length(dpss)), length=chunkSize, labels=F)
for(c in seq(max(chunks))) { 
	i=i+1
	countPlots[[i]] = ggplot(aggD[dps %in% dpss[chunks==c]], aes(y=value, x=date, color=cadre)) + 
		geom_line() + 
		geom_point(aes(size=n_facilities)) + 
		scale_color_manual(values=colors) + 
		scale_size_continuous(range=sizeRange2) + 
		facet_wrap(~dps, scales='free_y') + 
		labs(title=paste0('DPS Counts (DPS\'s ', (c*chunkSize)-chunkSize+1, '-', c*chunkSize, ')'), 
			y='Number of Health Care Workers', 
			x='', color='Cadre', size='Number of\nFacilities\nReporting') + 
		theme_bw()
}
# ------------------------------------------------------


# ------------------------------------------------------
# Graph rates

# national rates
ratePlots = list()
i=1
ratePlots[[i]] = ggplot(aggN, aes(y=mean, x=date, color=cadre)) + 
	geom_line() + 
	geom_point(aes(size=n_facilities)) + 
	scale_color_manual(values=colors) + 
	scale_size_continuous(range=sizeRange1) + 
	labs(title='National-Level Means', y='Health Care Workers per Facility', 
		x='', color='Cadre', size='Number of\nFacilities\nReporting') + 
	theme_bw()

# level rates
i=i+1
ratePlots[[i]] = ggplot(aggL, aes(y=mean, x=date, color=cadre)) + 
	geom_line() + 
	geom_point(aes(size=n_facilities)) + 
	scale_color_manual(values=colors) + 
	scale_size_continuous(range=sizeRange2) + 
	facet_wrap(~level) + 
	labs(title='Facility Platform Means', y='Health Care Workers per Facility', 
		x='', color='Cadre', size='Number of\nFacilities\nReporting') + 
	theme_bw()

# dps rates
dpss = unique(aggD$dps)
chunkSize=9
chunks = cut_interval(seq(length(dpss)), length=chunkSize, labels=F)
for(c in seq(max(chunks))) { 
i=i+1
ratePlots[[i]] = ggplot(aggD[dps %in% dpss[chunks==c]], aes(y=mean, x=date, color=cadre)) + 
	geom_line() + 
	geom_point(aes(size=n_facilities)) + 
	scale_color_manual(values=colors) + 
	scale_size_continuous(range=sizeRange2) + 
	facet_wrap(~dps) + 
	labs(title=paste0('DPS Means (DPS\'s ', (c*chunkSize)-chunkSize+1, '-', c*chunkSize, ')'), 
		y='Health Care Workers per Facility', 
		x='', color='Cadre', size='Number of\nFacilities\nReporting') + 
	theme_bw()
}
# ------------------------------------------------------


# ------------------------------------------------------
# Facility counts
facPlots = list()
i=1
facs = unique(data$org_unit)
chunkSize=9
chunks = cut_interval(seq(length(facs)), length=chunkSize, labels=F)
for(c in seq(max(chunks))) { 
	facPlots[[i]] = ggplot(data[org_unit %in% facs[chunks==c]], aes(y=value, x=date, color=cadre)) + 
		geom_line() + 
		geom_point() + 
		scale_color_manual(values=colors) + 
		facet_wrap(~org_unit, scales='free_y') + 
		labs(title=paste0('Facility Counts (Facilities ', (c*chunkSize)-chunkSize+1, '-', c*chunkSize, ')'), 
			y='Number of Health Care Workers', x='', color='Cadre') + 
		theme_bw()
	i=i+1
}
# ------------------------------------------------------


# --------------------------------------------------
# Save

# descriptives
pdf(outFile, height=5.5, width=10)
for(i in seq(length(ratePlots))) print(ratePlots[[i]])
for(i in seq(length(countPlots))) print(countPlots[[i]])
dev.off()

# outlier plots
pdf(outFileOutliers, height=5.5, width=9)
for(i in seq(length(outlierPlots))) { 
	print(outlierPlots[[i]])
	cat(paste0('\r ',round(i/length(outlierPlots)*100),'%'))
	flush.console()
}
dev.off()

# facility plots (random sample of 100)
set.seed(1)
pdf(outFileFacilities, height=5.5, width=9)
for(i in sample(seq(length(facPlots)), 100)) { 
	print(facPlots[[i]])
	cat(paste0('\r ',round(i/length(facPlots)*100),'%'))
	flush.console()
}
dev.off()
# --------------------------------------------------

