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
load(outputFile5a)

# load nodeTable for graphing
# nodeTable = fread('./impact_evaluation/drc/visualizations/nodetable_first_half.csv')

# shape file location
shapeFile = paste0(dir, '/mapping/cod/health_zones_who/health2.shp')

# ensure there are no extra variables introducted from nodeTable
# nodeTable = nodeTable[variable %in% names(data)]
# -----------------------------------------------


# -----------------------------------------------
# Set up first half estimates

# subset to coefficients of interest
summaries = summaries[op=='~' & grepl('_received_',lhs) & grepl('exp|other_dah|ghe', rhs)]

# unrescale
tmp = unique(melt(scaling_factors1, value.name='scaling_factor'))
summaries = merge(summaries, tmp, by.x='rhs', by.y='variable', all.x=TRUE)
summaries = merge(summaries, tmp, by.x='lhs', by.y='variable', all.x=TRUE)
summaries[is.na(scaling_factor.x), scaling_factor.x:=1]
summaries[is.na(scaling_factor.y), scaling_factor.y:=1]
summaries[, est_unrescaled:=est/scaling_factor.x*scaling_factor.y]

# pool funders together, weighting by investment size
# reshape data long
long = melt(data, id.vars=c('orig_health_zone','health_zone','date'))

# aggregate to total across whole time series (unrescaling not necessary)
long = long[, .(value=sum(value)), by=variable]

# merge to means
pooled_summaries = merge(summaries, long, by.x='rhs', by.y='variable', all.x=TRUE)

# take the weighted average across funders
pooled_summaries[grepl('exp|other_dah|ghe', rhs), rhs:='Pooled Investment']
byVars = c('health_zone','lhs','rhs')
pooled_summaries = pooled_summaries[, .(est_unrescaled=weighted.mean(est_unrescaled, value)), by=byVars]

# reshape outputs wide
pooled_summaries = dcast(pooled_summaries, health_zone~lhs, value.var='est_unrescaled')
# -----------------------------------------------


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
setnames(pooled_summaries, 'health_zone','health_zone_combined')
pooled_summaries[, c('health_zone', 'dps'):=tstrsplit(health_zone_combined, '_', fixed=TRUE)]
map[, health_zone:=standardizeHZNames(id)]
pooled_summaries[health_zone=='boma-mangbetu', health_zone:='boma']

# test before merge
m = unique(map$health_zone)
s = unique(pooled_summaries$health_zone)
m[!m %in% s] # in summary but not map
s[!s %in% m] # in map but not summary

# exclude duplicate health zones from summaries for now
dups = duplicated(pooled_summaries$health_zone)
excl = pooled_summaries$health_zone[dups]
pooled_summaries = pooled_summaries[!health_zone %in% excl]

# merge
mapData = merge(map, pooled_summaries, by.x='health_zone', by.y='health_zone', all.x=TRUE)

# melt outputs long
idVars = names(mapData)[!names(mapData) %in% c('ACT_received_cumulative','ITN_received_cumulative','RDT_received_cumulative')]
mapData = melt(mapData, id.vars=idVars)
# ------------------------------------------------------------------------------------------


# -----------------------------------------------
# Set up to map

# labels
mapData[variable=='ACT_received_cumulative', variable:='ACT Shipment Efficiency']
mapData[variable=='ITN_received_cumulative', variable:='ITN Shipment Efficiency']
mapData[variable=='RDT_received_cumulative', variable:='RDT Shipment Efficiency']

# titles
title = 'Efficiency\nActivities per Additional Dollar Invested'
# -----------------------------------------------


# -----------------------------------------------
# Map

# map each indicator with separate legend
p1 = ggplot(data=mapData[variable=='ACT Shipment Efficiency'], aes(x=long, y=lat, group=group, fill=value)) + 
  geom_polygon() + 
  geom_path(color='grey50', size=0.2, alpha=0.2) +
  scale_fill_viridis(direction=-1) + 
  labs(title='ACT Shipment Efficiency', fill='') + 
  coord_equal() +
  theme_void(base_size=12)  

p2 = ggplot(data=mapData[variable=='ITN Shipment Efficiency'], aes(x=long, y=lat, group=group, fill=value)) + 
  geom_polygon() + 
  geom_path(color='grey50', size=0.2, alpha=0.2) +
  scale_fill_viridis(direction=-1) + 
  labs(title='ITN Shipment Efficiency', fill='') + 
  coord_equal() +
  theme_void(base_size=12)  

p3 = ggplot(data=mapData[variable=='RDT Shipment Efficiency'], aes(x=long, y=lat, group=group, fill=value)) + 
  geom_polygon() + 
  geom_path(color='grey50', size=0.2, alpha=0.2) +
  scale_fill_viridis(direction=-1) + 
  labs(title='RDT Shipment Efficiency', fill='') + 
  coord_equal() +
  theme_void(base_size=12)  
# -----------------------------------------------


# ------------------------------------
# Save
pdf(outputFile6d, height=6, width=10)
grid.arrange(p1,p2,p3,top=textGrob(title,gp=gpar(fontsize=18)),ncol=3)
dev.off()
archive(outputFile6d)
# ------------------------------------
