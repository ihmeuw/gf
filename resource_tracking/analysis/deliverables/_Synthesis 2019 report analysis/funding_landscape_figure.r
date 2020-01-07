# -------------------------------------------------------------------------
# David Phillips
# 
# 12/17/2019
# Make a nice graph of the 8-country funding landscape for synthesis report
# -------------------------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(viridis)
# --------------------


# ------------------------------------------------------------------------------------------
# Files and directories

# root directory
dir = 'J:/Project/Evaluation/GF/resource_tracking'

# input file
inFile = paste0(dir, '/_odah/raw_data/IHME_DAH_DATABASE_1990_2018_Y2019M04D24.CSV')

# output file
outFile = paste0(dir, '/visualizations/deliverables/_Synthesis 2019/funding_landscape.pdf')

# whether to show RSSH
showRSSH = FALSE
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Load/prep data

# load
fullData = fread(inFile)

# subset to PCE countries
countries = c('COD','GTM','SEN','UGA','SDN','KHM','MMR','MOZ')
data = fullData[recipient_isocode %in% countries]

# format
vars = names(data)[grepl('dah_18', names(data))]
for(v in vars) data[, (v):=as.numeric(get(v))]

# combine channels
data[grepl('BIL',channel) & channel!='BIL_USA', channel:='Other Bilateral Aid']
data[channel %in% c('BMGF','US_FOUND') | grepl('NGO', channel), channel:='NGOs & Foundations']
data[grepl('DB',channel) | grepl('WB',channel) | 
	channel %in% c('GAVI','PAHO','WHO','EC') | grepl('UN',channel), 
	channel:='Dev. Banks, UN & WHO']
data[channel=='BIL_USA', channel:='US Bilateral Aid']
data[channel=='GFATM', channel:='Global Fund']
data[,sum(hiv_dah_18+mal_dah_18+tb_dah_18,na.rm=T),by=channel][order(V1)]

# collapse channels and drop sources/other dah columns
byVars = c('recipient_isocode','year','channel')
data = data[, .(hiv_dah_18=sum(hiv_dah_18, na.rm=T), 
	mal_dah_18=sum(mal_dah_18, na.rm=T), 
	tb_dah_18=sum(tb_dah_18,na.rm=T), 
	swap_hss_total_dah_18=sum(swap_hss_total_dah_18,na.rm=T)), 
	by=byVars]
if (!showRSSH) data$swap_hss_total_dah_18 = NULL
	
# subset to 2000+
data = data[year>=2000]

# melt
data = melt(data, id.vars=byVars)
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Set up to graph

# disease labels
data[variable=='hiv_dah_18', variable:='HIV']
data[variable=='mal_dah_18', variable:='Malaria']
data[variable=='tb_dah_18', variable:='TB']
if (showRSSH) data[variable=='swap_hss_total_dah_18', variable:='RSSH*']

# order Global Fund last
data[, channel := factor(channel, levels=c('Dev. Banks, UN & WHO', 'Other Bilateral Aid', 
	'NGOs & Foundations', 'US Bilateral Aid', 'Global Fund'))]

# titles/captions
c=NULL
if (showRSSH) c = '*RSSH categorized using FGH methodology: \nInstitute for Health Metrics and Evaluation. Financing Global Health 2018:\nCountries and Programs in Transition. Seattle, WA, 2019'
y = 'USD Disbursement (in millions)'
t = 'Development Assistance for Health in PCE Countries'
s = '2000-2017'

# ------------------------------------------------------------------------------------------


# ---------------------------------------------------------------
# Graph
pdf(outFile, height=5.5, width=8)
ggplot(data, aes(y=value/1000, x=year, fill=channel)) + 
	geom_area() + 
	facet_wrap(~variable, scales='free_y', ncol=1) + 
	scale_fill_viridis(discrete=TRUE) + 
	labs(title=t,subtitle=s, y=y, x='', caption=c, fill='') + 
	theme_minimal(base_size=16) + 
	theme(plot.caption=element_text(size=8)) 
dev.off()
# ---------------------------------------------------------------


# -------------------------------------------------
# Look up numbers for text

# % global fund since 2010
gf = sum(data[year>=2010 & variable!='RSSH*' & channel=='Global Fund']$value)
total = sum(data[year>=2010 & variable!='RSSH*']$value)
round(gf/total*100, 1)

# % global fund since 2010 HIV
gf = sum(data[year>=2010 & variable=='HIV' & channel=='Global Fund']$value)
total = sum(data[year>=2010 & variable=='HIV']$value)
round(gf/total*100, 1)

# % global fund since 2010 TB
gf = sum(data[year>=2010 & variable=='TB' & channel=='Global Fund']$value)
total = sum(data[year>=2010 & variable=='TB']$value)
round(gf/total*100, 1)

# % global fund since 2010 malaria
gf = sum(data[year>=2010 & variable=='Malaria' & channel=='Global Fund']$value)
total = sum(data[year>=2010 & variable=='Malaria']$value)
round(gf/total*100, 1)
# -------------------------------------------------
