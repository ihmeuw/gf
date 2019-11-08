# -----------------------------------
# David Phillips
# 
# 3/25/2019
# Analyze specific effect sizes by health zone
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

library(rgeos) # for gsimplify
source('./impact_evaluation/drc/set_up_r.r')
source('./core/standardizeHZNames.R')

# load model results
load(outputFile5b)

# bring in untransformed data
untransformed = readRDS(outputFile2c)

# shape file location
shapeFile = paste0(dir, '/mapping/cod/health_zones_who/health2.shp')
# -----------------------------------------------

x='ITN'
y='ITN_rate'

# x='mildMalariaTreated'
# y='mildMalariaTreated_rate'

# -------------------------------------------------------------------------------------
# Set up second half estimates

# subset to coefficients of interest
outputs = c('SSCACT', 'ITN', 'RDT', 'SP', 'mildMalariaTreated', 'severeMalariaTreated')
subset = summaries[op=='~' & rhs %in% outputs]

# subset to just one arrow for development
subset = subset[rhs==x & lhs==y]

# exponentiate coefficients on log-transformed variables
# to make it "per 100 additional nets, x% increase in cvg"
mean_x = untransformed[, .(mean_x=mean(get(x), na.rm=T)), by=health_zone]
subset = merge(subset, mean_x, by='health_zone', all.x=TRUE)
subset[, unit := 1+(100/mean_x)]
subset[, est_100:=((unit^est)-1)*100] # for each 100 bednets...
subset[, est_1pct:=((1.01^est)-1)*100] # for each 1% more bednets...

# look up the national coverage per 10 nets
total_x = mean(untransformed[,sum(get(x),na.rm=T),by=floor(date)]$V1)
est = means[op=='~' & rhs==x & lhs==y]$est/10
lower = (means[op=='~' & rhs==x & lhs==y]$est-(1.96*means[op=='~' & rhs==x & lhs==y]$se))/10
upper = (means[op=='~' & rhs==x & lhs==y]$est+(1.96*means[op=='~' & rhs==x & lhs==y]$se))/10
unit = 1+(10/total_x)
if(x=='ITN') { 
((unit^est)-1)*sum(untransformed[date==2017]$population) # this displays the estimate for ITNs
((unit^lower)-1)*sum(untransformed[date==2017]$population) # this displays the estimate for ITNs
((unit^upper)-1)*sum(untransformed[date==2017]$population) # this displays the estimate for ITNs
}
# if(x=='mildMalariaTreated') {
# ((unit^est)-1)*sum(untransformed[date==2017]$incidence) # this displays the estimate for treatment
# ((unit^lower)-1)*sum(untransformed[date==2017]$incidence) # this displays the estimate for treatment
# ((unit^upper)-1)*sum(untransformed[date==2017]$incidence) # this displays the estimate for treatment
# }
# -------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Bring in shapefile

# load
shapeData = shapefile(shapeFile)
mapDatatmp = shapeData@data
shapeData = gSimplify(shapeData, tol=0.01, topologyPreserve=TRUE)
shapeData = as(shapeData, 'SpatialPolygonsDataFrame')
shapeData@data = mapDatatmp

# fortify
map = data.table(fortify(shapeData, region='Name'))

# standardize health zone names
setnames(subset, 'health_zone','health_zone_combined')
subset[, c('health_zone', 'dps'):=tstrsplit(health_zone_combined, '_', fixed=TRUE)]
map[, health_zone:=standardizeHZNames(id)]
subset[health_zone=='boma-mangbetu', health_zone:='boma']

# test before merge
m = unique(map$health_zone)
s = unique(subset$health_zone)
m[!m %in% s] # in summary but not map
s[!s %in% m] # in map but not summary

# exclude duplicate health zones from summaries for now
dups = duplicated(subset$health_zone)
excl = subset$health_zone[dups]
subset = subset[!health_zone %in% excl]

# merge
mapData = merge(map, subset, by.x='health_zone', by.y='health_zone', all.x=TRUE)
# ------------------------------------------------------------------------------------------


# -----------------------------------------------
# Set up to map

# labels
mapData[lhs=='ITN_rate_cumul', variable:='ITN Coverage']

# titles
title1 = 'Percent Increase in ACT Coverage\n per 1% Increase in ACTs Distributed'
# -----------------------------------------------


# -----------------------------------------------
# Map

# map percentage increase in coverage from percentage increase in distribution
p1 = ggplot(data=mapData[lhs==y], aes(x=long, y=lat, group=group, fill=est_1pct)) + 
  geom_polygon() + 
  geom_path(color='grey50', size=0.2, alpha=0.2) +
  scale_fill_viridis(direction=-1) + 
  labs(title=title1, fill='%') + 
  coord_equal() +
  theme_void(base_size=12) + 
  theme()
# -----------------------------------------------


# ------------------------------------
# Save
print(paste('Saving:', outputFile6d)) 
pdf(outputFile6d, height=6, width=10)
p1
dev.off()
archive(outputFile6d)
# ------------------------------------
