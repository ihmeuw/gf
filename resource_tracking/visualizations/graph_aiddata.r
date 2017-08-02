# --------------------------------------------------
# David Phillips
#
# 8/1/2017
# Map/graph Uganda and DRC dah data from aiddata.org
# --------------------------------------------------

# TO DO
# make this work for DRC too
# take into account redistricting

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
# --------------------


# -----------------------------------------------------------------------------
# Files and directories

# data directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/resource_tracking/')

# which country?
iso3 = 'uga'

# input files
inFile = paste0(dir, iso3, '/prepped/uga_aiddata.csv')

# mapping from district names to numbers
idsFile = paste0(dir, '../mapping/', iso3, '/', iso3, '_geographies_map.csv')

# alternate names for districts
altsFile = paste0(dir, '../mapping/', iso3, '/', iso3, '_alternate_dist_names.csv')

# shape data
shapeFile = paste0(dir, '../mapping/', iso3, '/', iso3, '_dist112_map.rdata')

# output files
outFile = paste0(dir, iso3, '/visualizations/', iso3, '_aiddata.pdf')
# -----------------------------------------------------------------------------


# -------------------------------------------------------------------------------------------
# Load/prep data

# load data
data = fread(inFile)

# subset observations
data = data[!is.na(nyears) & !is.na(annual_commitments)]

# expand projects to project-years
data = data[rep(seq(.N), nyears)]
data[, year:=seq(.N), by=c('project_id','gazetteer_adm_name')]
data[, year:=transactions_start_year+year-1]

# parse out locations from gazetteer_adm_name
vars = c('planet','continent','country','province','district','subdistrict1','subdistrict2')
data[, (vars):=tstrsplit(gazetteer_adm_name, '|', fixed=TRUE)]

# fix errors in data
data[district=='St. Kizito Hospital - Matany', subdistrict1:='St. Kizito Hospital - Matany']
data[district=='St. Kizito Hospital - Matany', district:='Napak']
data[district=='Kisozi', subdistrict1:='Kisozi']
data[district=='Kisozi', district:='Gomba']
data[district=='Mutundwe', subdistrict1:='Mutundwe']
data[district=='Mutundwe', district:='Wakiso']
data[district=='Muyenga', subdistrict1:='Muyenga']
data[district=='Muyenga', district:='Kampala']
data[province=='Agwiciri', subdistrict1:='Agwiciri']
data[province=='Agwiciri', district:='Apac']
data[province=='Agwiciri', province:='Northern']
data[province=='Kampala', district:='Kampala']
data[province=='Kampala', province:='Central']
data[province=='Kyabakuza', subdistrict1:='Kyabakuza']
data[province=='Kyabakuza', district:='Masaka']
data[province=='Kyabakuza', province:='Central']
data[province=='Nabirumba', subdistrict1:='Nabirumba']
data[province=='Nabirumba', district:='Kamuli']
data[province=='Nabirumba', province:='Eastern']
data[province=='Namugongo Point', subdistrict1:='Namugongo Point']
data[province=='Namugongo Point', district:='Wakiso']
data[province=='Namugongo Point', province:='Central']

# assume missing location is national level
data[is.na(province) & is.na(district) & is.na(subdistrict1) & 
		is.na(subdistrict2) & is.na(latitude), country:='Uganda']
		
# collapse to location-year-donor and location-year
data[is.na(subdistrict1) & is.na(subdistrict2), latitude:=NA]
data[is.na(subdistrict1) & is.na(subdistrict2), longitude:=NA]
byvars = c('year','country','province','district',
		'subdistrict1','subdistrict2','latitude','longitude')
donorData = data[, list(annual_commitments=sum(annual_commitments)), by=c(byvars,'donors')]
data = data[, list(annual_commitments=sum(annual_commitments)), by=byvars]

# express in millions
donorData[, annual_commitments:=annual_commitments/1000000]
data[, annual_commitments:=annual_commitments/1000000]

# identify level
data[!is.na(subdistrict1) | !is.na(subdistrict2), level:='Sub-District']
data[(is.na(subdistrict1) & is.na(subdistrict2)) & !is.na(district), level:='District']
data[(is.na(subdistrict1) & is.na(subdistrict2) & is.na(district)) & !is.na(province), level:='Region']
data[(is.na(subdistrict1) & is.na(subdistrict2) & is.na(district) & is.na(province)) & !is.na(country), level:='National']
# -------------------------------------------------------------------------------------------


# ----------------------------
# Load/prep shape data

# load shape data
load(shapeFile)

# prep shape data
map = data.table(fortify(map))
# ----------------------------


# ----------------------------------------------------------------------------
# Merge data to shape data

# load district codes
ids = fread(idsFile)

# prep ids
ids[, dist112:=as.character(dist112)]
ids[, region4:=as.character(region4)]

# correct district/region names
data[, district:=gsub(' District', '', district)]
data[, province:=gsub(' Region', '', province)]
donorData[, district:=gsub(' District', '', district)]
donorData[, province:=gsub(' Region', '', province)]

# confirm no mismatching district names
miss = data[!district %in% ids$dist112_name & !is.na(district)]$district
if (length(miss)!=0) {
	stop('Error: some district names not in map!')
}
miss = data[!province %in% ids$region4_name & !is.na(province)]$province
if (length(miss)!=0) {
	stop('Error: some province names not in map!')
}

# add ids to data
distids = unique(ids[, c('dist112_name','dist112'), with=FALSE])
provids = unique(ids[, c('region4_name','region4'), with=FALSE])
data = merge(data, distids, by.x='district', by.y='dist112_name', all.x=TRUE)
data = merge(data, provids, by.x='province', by.y='region4_name', all.x=TRUE)

# subset data to different levels
distdata = data[!is.na(district) & is.na(subdistrict1) & is.na(subdistrict2)]
distdata = distdata[, c('dist112','district','year','annual_commitments'), with=FALSE]

# expand map by years
distmap = copy(map)
minyear = min(data$year)
maxyear = max(data$year)
distmap = distmap[rep(seq(.N), maxyear-minyear+1)]
distmap[, year:=rep(seq(minyear, maxyear), each=nrow(map))]

# add data to map
distmap = merge(distmap, distdata, by.x=c('id','year'), 
					by.y=c('dist112','year'), all.x=TRUE)
# ----------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------
# Set up to graph

# clean up donor names
donorData[donors=='Global Fund for HIV, TB and Malaria', donors:='GFATM'] 
donorData[donors=='United Nations Development Programme|Sweden|Norway|World Health Organization|Ireland', donors:='UNDP'] 
donorData[donors=='Norway|European Union', donors:='Norway'] 
donorData[donors=='International Development Association', donors:='IDA'] 
donorData[donors=='United Nations Populations Fund', donors:='UNPF'] 
donorData[donors=='United Nations Children\'s Fund|United Nations Development Programme', donors:='UNICEF'] 
donorData[donors=='Denmark/DANIDA', donors:='Denmark'] 
donorData[donors=='United Kingdom|Germany', donors:='UK & Germany'] 
donorData[donors=='United States of America', donors:='USA'] 

# set aside subdistrict data for points
subdata = data[!is.na(subdistrict1) | !is.na(subdistrict2)]

# map colors
mapColors = brewer.pal(10, 'RdYlBu')

# scatterplot colors
pointColors = c('National'='#3C3530', 'Region'='#AACD6E', 'District'='#F16B6F', 'Sub-District'='#77AAAD')

# bar colors
barColors = brewer.pal(length(unique(donorData$donors)), 'Paired')
barColors[barColors=='#FFFF99'] = 'grey50'
names(barColors)=unique(donorData$donors)[order(unique(donorData$donors))]
# ---------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------
# Make graphs

# map district-level commitments and lower in pairs of two
years = unique(data[!is.na(district)]$year)
years = years[order(years)]
idx = which(years %in% years[c(TRUE, FALSE)])
mapList = lapply(idx, function(i) {	
	ggplot(distmap[year %in% years[c(i,i+1)]], aes(x=long, y=lat, group=group, fill=annual_commitments)) + 
		geom_polygon() + 
		geom_point(data=subdata[year %in% years[c(i,i+1)]], 
			aes(x=longitude, y=latitude, color=annual_commitments, group=NULL)) + 
		facet_wrap(~year) + 
		theme_minimal() + 
		geom_path(color='grey65', size=.05) + 
		coord_fixed(ratio=1) + 
		scale_x_continuous('', breaks=NULL) + scale_y_continuous('', breaks=NULL) + 
		labs(title='Aid Data Records of Annual Commitments', subtitle='District-Level and Below') + 
		scale_fill_gradientn(paste('Annual\nCommitments\n(Millions $)'), colours=mapColors, na.value='white') + 
		scale_color_gradientn(paste('Annual\nCommitments\n(Millions $)'), colours=mapColors) + 
		theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
})
	
# time series by district
tsPlots = lapply(unique(ids$dist112_name), function(d) {
	ggplot(data=NULL, aes(y=annual_commitments, x=year)) + 
		geom_bar(data=donorData[district==d | is.na(district)], 
			aes(fill=donors, shape=NULL, color=NULL), position='stack', stat='identity') + 
		geom_point(data=data[district==d | is.na(district)], 
			aes(, shape=level), size=2.25) + 
		labs(title='Aid Data Records of Annual Commitments', subtitle=paste(d,'District'), 
			y='Annual Commitments (Millions $)', x='') + 
		scale_fill_manual('Donor', values=barColors) + 
		scale_shape_manual('Level', values=c('National'=15, 'Region'=3, 'District'=19,'Sub-District'=17)) + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5)) 
})


# ---------------------------------------------------------------------------------------------


# -------------------------------------------------
# Store graphs
pdf(outFile, height=6, width=9) 
for(i in seq(length(mapList))) print(mapList[[i]])
for(i in seq(length(tsPlots))) print(tsPlots[[i]])
dev.off()
# -------------------------------------------------
