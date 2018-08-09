# -----------------------------------------------------------------------------
# David Phillips
# 
# 7/19/2018
# Analysis of resources in and out of TB priority departments
# -----------------------------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(raster)
library(RColorBrewer)
library(ggplot2)
# --------------------


# --------------------------------------------------------------------------
# Files and directories

# input directories
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/'
mapDir = 'J:/Project/Evaluation/GF/mapping/gtm/'

# input file
inFile = paste0(dir, 'total_resource_tracking_data.csv')

# list of priority municipalities for TB
priorityFile = paste0(mapDir, 'high_priority_muni.csv')

# muni-level shapefile
shapeFile = paste0(mapDir, 'GTM_munis_only.shp')

# output file
outFile = paste0(dir, '../../gtm/visualizations/tb_priority_departments.pdf')
# --------------------------------------------------------------------------


# ----------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# subset to sicoin TB data only
data = data[data_source=='sicoin' & disease=='tb']

# subset to disease-years with subnational data only
data[adm2==100, loc_name:='gtm']
data[, n_locs:=length(unique(loc_name)), by=c('disease','year')]
data[, has_subnational:=ifelse(n_locs>1,1,0)]
data = data[has_subnational==1]

# collapse to source-muni-year level
byVars = c('adm2','loc_name','financing_source','year')
valueVars = c('budget','disbursement','expenditure')
data = data[, lapply(.SD, sum), by=byVars, .SDcols=valueVars]
# ----------------------------------------------------------------------


# ----------------------------------------------------------------------
# Identify priority municipalities

# load list of priority munis
munis = fread(priorityFile)

# prep muni lists
munis[, adm2_name:=tolower(adm2_name)]
data[, loc_name:=tolower(loc_name)]
munis[adm2_name=='cabanas', adm2_name:='cabañas']
munis[adm2_name=='guatemala city', adm2_name:='guatemala']
munis = munis[high_priority==1, c('adm2_name', 'high_priority')]

# identify priority munis in data
data = merge(data, munis, by.x='loc_name', by.y='adm2_name', all.x=TRUE)
data[is.na(high_priority), high_priority:=0]

# fix problems from alternate spellings
data[, high_priority:=max(high_priority),by='adm2']

# aggregate by priority or not
byVars = c('high_priority','financing_source','year')
priorityAgg = data[, lapply(.SD, sum), by=byVars, .SDcols=valueVars]
# ----------------------------------------------------------------------


# ---------------------------------------------------------------------------------- 
# Prep shapefile

# load
shapeData = shapefile(shapeFile)

# fortify
mapData = data.table(fortify(shapeData, region='Codigo'))

# merge
wide = dcast(data, adm2+high_priority~financing_source+year, value.var=valueVars)
mapData = merge(mapData, wide, by.x='id', by.y='adm2', all.x=TRUE)

# reshape mapData long for easier graphing
idVars = c('id','long','lat','order','hole','piece','group','high_priority')
mapData = melt(mapData, id.vars=idVars)

# parse variable
mapData[, c('variable','financing_source','year'):=tstrsplit(variable, '_', fixed=TRUE)]
mapData[, year:=as.numeric(year)]
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# Set up to graph

# label high/low priority
priorityAgg[high_priority==1, priority_label:='High Priority Municipalities']
priorityAgg[high_priority==0, priority_label:='Other Municipalities']

# label sources
priorityAgg[financing_source=='donacions', financing_source:='All External Donors']
priorityAgg[financing_source=='gf', financing_source:='Global Fund']
priorityAgg[financing_source=='ghe', financing_source:='Government']
mapData[financing_source=='donacions', financing_source:='All External Donors']
mapData[financing_source=='gf', financing_source:='Global Fund']
mapData[financing_source=='ghe', financing_source:='Government']

# map colors
colors = brewer.pal(8,'BrBG')
border = 'grey65'
highlight = 'red'
# ----------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------
# Graph

# map of budget by source highlighting priority departments
idx = which(mapData$variable=='budget' & mapData$year==2016)
p1 = ggplot(mapData[idx], aes(x=long, y=lat, group=group, fill=value/1000000)) + 
	geom_polygon() + 
	geom_path(color=border, size=.005) + 
	geom_path(data=mapData[idx][high_priority==1], color=highlight, size=.05) + 
	# scale_fill_gradientn('', colors=colors) + 
	coord_fixed() + 
	facet_wrap(~financing_source) + 
	labs(title='Subnational Budget Allocation - 2016', fill='Budget\n(In Millions of USD)', caption='Priority Municipalities in Red') + 
	theme_void()

# trend of spending in/out of priority departments
p2 = ggplot(priorityAgg, aes(x=year, y=budget/1000000, color=priority_label)) + 
		geom_line(size=1.5) + 
		geom_point(size=3.5, color='grey45') + 
		facet_wrap(~financing_source) + 
		labs(title='', y='Budget (In Millions of USD)', color='') + 
		theme_bw()
# -----------------------------------------------------------------------------------


# --------------------------------
# Save graphs
pdf(outFile, height=5.5, width=8)
p1
p2
dev.off()
# --------------------------------
