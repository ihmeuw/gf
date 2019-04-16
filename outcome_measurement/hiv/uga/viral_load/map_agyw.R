# map function for agyw

# set working directory
setwd(paste0(j, '/Project/Evaluation/GF/mapping/uga/'))

# uganda shapefile
DistMap = shapefile('uga_dist112_map.shp')

#------------------------------
# create a regional shape file 

# import the ten regions that are included in phia
regions = fread(paste0(j, "/Project/Evaluation/GF/mapping/uga/uga_geographies_map.csv"))
regions = unique(regions[ ,.(region=region10_alt, district_name=dist112_name)])

# put the regions in the same order as the shape file
regions = regions[match(DistMap@data$dist112_n, regions$district_name)]
id = regions$region

# create coordinates for the old and new plots
RegMap  = unionSpatialPolygons(DistMap, id)

#------------------------------
# output file for graphs
outFile = paste0(dir, 'output/phia_vl_dashboard_same_time_period.pdf')

# ----------------------------------------------------------------------

# -------------------------------------------------------------------------------------------
# Set up to graph

#-------------------------------
# fortify the maps 
reg_coord = fortify(RegMap)