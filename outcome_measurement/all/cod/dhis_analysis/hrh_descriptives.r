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

# output file
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
	data[, value:=as.numeric(value)]
	byVars = c('date','org_unit','dps','health_zone','health_area','level','cadre')
	data = data[, .(value=sum(value)), by=byVars]

	# save interim file
	saveRDS(data, interimFile)
}
if (!reprep_data) data = readRDS(interimFile)
# ---------------------------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Remove outliers

# set up
idVars = c('date','org_unit','health_zone','cadre','dps','health_area','level',,'candidate')
outlierPlots = list()

# loop over levels
i=1
for(l in unique(data$level)) {
	print(l)
	
	# subset
	tmp = data[level==l]
	facs = unique(tmp$org_unit)
	s = seq(length(facs))
	
	# run chunks of facilities
	chunks = cut_interval(s, length=50, labels=F)
	for(c in unique(chunks)) { 
		
		# subset to chunk 
		tmp2 = tmp[org_unit %in% facs[which(chunks==c)]]
		
		# run regression
		lmFit = lm(value ~ org_unit * cadre * date, data=tmp2)
		
		# identify candidates
		tmp2[, pred:=predict(lmFit)]
		tmp2[, resid:=resid(lmFit)]
		tmp2[, median:=median(resid)]
		tmp2[, sd:=sd(resid)]
		f = 3.25
		tmp2[, candidate:= resid>(median+(f*sd)) | resid<(median-(f*sd))]
		candidateFacilities = unique(tmp2[candidate==TRUE]$org_unit)
		if (i==1) candidateOutliers = tmp2[candidate==TRUE, idVars]
		if (i>1) candidateOutliers = rbind(candidateOutliers, tmp2[candidate==TRUE, idVars])
		
		# graph
		outlierPlots[[i]] = ggplot(tmp2[org_unit %in% candidates], aes(y=value,x=date)) + 
			geom_line(aes(color=cadre)) + 
			geom_point(color='black') + 
			geom_point(data=tmp2[candidate==TRUE], color='red') + 
			geom_line(aes(y=pred, color=cadre), linetype='dashed') + 
			facet_wrap(~org_unit, scales='free') + 
			theme_bw()
		i=i+1
	}
}

# remove
data = merge(data, candidateOutliers, by=idVars)
data[candidate==TRUE, value:=NA]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Aggregate

# national level
aggN = data[, .(value=sum(value)), by=c('date','cadre')]

# dps level
aggD = data[, .(value=sum(value)), by=c('dps', 'date','cadre')]

# health facility type level
aggT = data[, .(value=sum(value)), by=c('org_unit_type', 'date','cadre')]
# -------------------------------------------------------------------------


# ---------------------------------------------
# Set up to graph
c = brewer.pal(12, 'Paired')
colors = c('General Practitioner'=c[1], 'Nurse (A1, A2 or L2)'=c[4], 
		'Lab Technician'=c[8], 'Midwife'=c[10], 'Nutritionist'=c[6])
# ---------------------------------------------


# ------------------------------------------------------
# Graph

ggplot(data, aes(y=value, x=factor(date))) + 
	geom_violin() + 
	theme_bw()

ggplot(aggN, aes(y=value, x=date, color=cadre)) + 
	geom_line() + 
	geom_point() + 
	theme_bw()

# ------------------------------------------------------


# --------------------------------------------------
# Save
pdf(outFile, height=5.5, width=9)
for(i in seq(length(meanPlots))) print(meanPlots[[i]])
for(i in seq(length(pctPlots))) print(pctPlots[[i]])
dev.off()
# --------------------------------------------------

