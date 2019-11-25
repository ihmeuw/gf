# ----------------------------------------------------
# David Phillips
# 
# 11/9/2019
# Compare total health expenditure to number of cases
# ----------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(colormap)
# --------------------


# ------------------------------------------------------------------------------------------
# Files and directories

# root directory
impactDir = 'J:/Project/Evaluation/GF/impact_evaluation/mortality/'

# input file downlaoded from GBD results tool on 11/9/2019
gbdFile = paste0(impactDir, 'prepped_data/IHME-GBD_2017_DATA-1cc0261b-1.csv')

# resource tracking files with expenditure
rtDir = 'J:/Project/Evaluation/GF/resource_tracking/'
gfFile = paste0(rtDir, '_gf_files_gos/combined_prepped_data/final_expenditures.rds')
gheFile = paste0(rtDir, '_ghe/combined_prepped_data/all_ghe.rds')
dahFile = paste0(rtDir, '_odah/prepped_data/other_dah_actuals_all.rds')

# output file
outFile = 'J:/Project/Evaluation/GF/vfm/visualizations/efficiency/expenditure_per_case.pdf'
# ------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# load/prep GBD estimates

# load data
gbdData = fread(gbdFile)

# setset to IHME/PATH countries
countries = c('Uganda', 'Guatemala', 'Democratic Republic of the Congo', 'Senegal')
gbdData = gbdData[location %in% countries]
gbdData[location=='Uganda', loc_name:='uga']
gbdData[location=='Guatemala', loc_name:='gtm']
gbdData[location=='Democratic Republic of the Congo', loc_name:='cod']
gbdData[location=='Democratic Republic of the Congo', location:='DRC']
gbdData[location=='Senegal', loc_name:='sen']

# subset to age-standardized rates
gbdData = gbdData[age=='All Ages' & metric=='Number']

# subset to both sexes combined
gbdData = gbdData[sex=='Both']

# subset to incidence and mortality
gbdData = gbdData[measure=='Prevalence']

# subset to 2000+
gbdData = gbdData[year>=2000]

# prep cause variable
gbdData[cause=='HIV/AIDS', disease:='hiv']
gbdData[cause=='Malaria', disease:='malaria']
gbdData[cause=='Tuberculosis', disease:='tb']
# ---------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------------------------------
# Load/prep resource tracking data

# load files
gfData = readRDS(gfFile)
dahData = readRDS(dahFile)
gheData = readRDS(gheFile)

# collapse to country-year level
gfData[, year:=year(start_date)]
byVars = c('loc_name','year','disease')
gfData = gfData[, .(expenditure=sum(expenditure,na.rm=T)), by=byVars]
dahData = dahData[channel!='GFATM', .(disbursement=sum(disbursement, na.rm=TRUE)), by=byVars]
gheData = gheData[, .(disbursement=sum(disbursement, na.rm=TRUE)), by=byVars]

# prep iso codes
dahData[, loc_name:=tolower(loc_name)]

# set names
setnames(gfData, 'expenditure', 'global_fund')
setnames(dahData, 'disbursement', 'other_donors')
setnames(gheData, 'disbursement', 'government')
# -------------------------------------------------------------------------------------------------------------------


# ----------------------------------------------------
# Merge data

# merge financial data
rtData = merge(gfData, dahData, by=byVars, all=TRUE)
rtData = merge(rtData, gheData, by=byVars, all=TRUE)

# check
# ggplot(rtData, aes(y=global_fund, x=year)) + 
	# geom_line(color='green') +
	# geom_line(aes(y=other_donors), color='red') +
	# geom_line(aes(y=government), color='blue') +
	# facet_grid(disease~loc_name)
	
# merge gbd data
data = merge(gbdData, rtData, by=byVars)

# set NA to zero because there's no other choice
data[is.na(global_fund), global_fund:=0]
data[is.na(other_donors), other_donors:=0]
data[is.na(government), government:=0]

# compute THE per case
data[, the_per_case:=(global_fund+other_donors+government)/val]
data[, the_per_case_lower:=(global_fund+other_donors+government)/lower]
data[, the_per_case_upper:=(global_fund+other_donors+government)/upper]
data[, dah_per_case:=(global_fund+other_donors)/val]
data[, dah_per_case_lower:=(global_fund+other_donors)/lower]
data[, dah_per_case_upper:=(global_fund+other_donors)/upper]
# ----------------------------------------------------


# ----------------------------------------------------
# Set up to graph
data[, disease:=toupper(disease)]
# ----------------------------------------------------


# --------------------------------------------------------------------------------------------------------------
# Display numbers
display = copy(data)
display[val>=1000 & val<1000000, val_formatted:=paste0(round(val/1000), 'K')]
display[val>=1000000, val_formatted:=paste0(round(val/1000000), 'M')]
display[val<1000, val_formatted:=round(val)]
display[, dah_per_case_formatted:=paste0('$', round(dah_per_case,1), ' (', val_formatted, ')')]
display = display[year==max(year), c('location','cause','dah_per_case_formatted'), with=FALSE]
display = dcast(display, location~cause)
display
# --------------------------------------------------------------------------------------------------------------


# ----------------------------------------------------
# Graph THE per case
pdf(outFile, height=5.5, width=9.5)
ggplot(data, aes(y=the_per_case, x=year)) + 
	geom_line(aes(y=dah_per_case, linetype='Donor Spending')) + 
	geom_line(aes(linetype='Donor Spending Plus\nGovernment Expenditure*')) + 
	facet_grid(disease~location, scales='free') + 
	scale_linetype_manual('', values=c('Donor Spending'='solid', 
		'Donor Spending Plus\nGovernment Expenditure*'='dashed')) + 
	labs(title='Total Health Expenditure per Prevalent Case', 
		y='USD per Prevalent Case', x='', 
		caption='Not including out-of-pocket or private, prepaid health spending\n*Government expenditure not available in all years') + 
	theme_minimal(base_size=14) + 
	theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), 
		plot.title=element_text(size=14), plot.subtitle=element_text(size=11), 
		axis.title.y=element_text(size=11), plot.caption=element_text(size=8))
dev.off()
# ----------------------------------------------------
