# ---------------------------------------------------------
# David Phillips
#
# 11/21/2017
# Function that graphs the PHIA-VLD comparisons
# Intended to be run by compare_phia_to_vl_dashboard.r
# Inputs:
# dir - directory where the output should go
# nothing else; pulls objects from memory
# Outputs:
# nothing; saves to outFile
# ---------------------------------------------------------


# --------------------------------------------
# Start function
graphVL = function(dir=NULL) { 
  # ----------------------------------------
  
  # ----------------------------------------------------------------------
  # Files and directories
  
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
  outFile = paste0(dir, 'output/phia_vl_dashboard.pdf')
  
  # ----------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------------
  # Set up to graph
  
  # fortify the maps 
  reg_coord = fortify(RegMap)
  dist_coord = fortify(DistMap, region='dist112')
  
  # get names associated with ids
  dist_names = cbind(id=DistMap@data$dist112, district=DistMap@data$dist112_na)
  
  # round 
  regData = regData[ , lapply(.SD, round, 1), .SDcols=2:12, by=region]
  distData = distData[ , lapply(.SD, round, 1), .SDcols=3:13, by=.(district, region)]
  distData = merge(distData, dist_names, by='district')
  
  # reshape long
  long = melt(regData, id.vars='region')
  
  # merge to map and melt data
  mapDataReg = merge(reg_coord, regData, by.x='id', by.y='region', all.x=TRUE)
  mapDataReg = melt(mapDataReg, id.vars=c('long','lat', 'id','group','order','hole','piece'))
  mapDataReg = data.table(mapDataReg)
  
  # clean up variable labels
  long[variable=='phia_vls', variable:='PHIA']
  long[variable=='vld_suppression_hat', variable:='National Dashboard*']
  
  mapDataReg[variable=='phia_vls', variable:='Viral Load Suppression\nPHIA']
  mapDataReg[variable=='phia_vls_lower', variable:='Lower']
  mapDataReg[variable=='phia_vls_upper', variable:='Upper']
  mapDataReg[variable=='samples', variable:='N Samples']
  mapDataReg[variable=='vl_suppressed', variable:='N Samples Suppressed']
  mapDataReg[variable=='vld_suppression', variable:='Viral Load Suppression\nUganda VL Dashboard']
  
  mapDataReg[variable=='art_coverage_2011', variable:='ART Coverage 2011 AIS']
  mapDataReg[variable=='art_coverage', variable:='ART Coverage']

  mapDataReg[variable=='vld_suppression_adj', variable:='Viral load Suppression\n Adjusted for ART Coverage']  
  mapDataReg[variable=='ratio', variable:='Ratio of PHIA to VLD']  
  mapDataReg[variable=='vld_suppression_hat', variable:='Reported Viral Load Suppression\nNational Dashboard*']

  #----------------------------------
  # district map
  mapDataDist = merge(dist_coord, distData, by='id', all.x=TRUE)

  
  
  # -------------------------------------------------------------------------------------------
  # create labels for the regional map 
  
  # identify centroids and label them
  names = data.table(coordinates(RegMap))
  setnames(names, c('long', 'lat'))
  names[ , region:=unique(reg_coord$id)]
  
  # merge in the ratios for complete labels
  names = merge(names, regData, by='region')
  setnames(names, 'region', 'id')
  
  # replace labels with hyphens to match phia graphics
  names$id = gsub(names$id, pattern='_', replacement='-')
  names[grep('^Central', id), id:=(gsub(id, pattern='-', replacement=' '))]
  names[grep('^West', id), id:=(gsub(id, pattern='-', replacement=' '))]
  
  #-----------------------
  # p1 label - no adjustment 
  p1_labels = names[ ,.(id, long, lat, phia_vls, vld_suppression)]
  p1_labels[ , phia:=paste0(id, ': ', phia_vls, '%' )]
  p1_labels[ , vl:=paste0(id, ': ', vld_suppression, '%' )]
  p1_labels[ ,c('phia_vls', 'vld_suppression'):=NULL]
  p1_labels = melt(p1_labels, id.vars = c('id', 'long', 'lat'))
  
  p1_labels[variable=='phia', variable:='Viral Load Suppression\nPHIA']
  p1_labels[variable=='vl', variable:='Viral Load Suppression\nUganda VL Dashboard']
  
  #----------------------
  # p2 label - ART coverage 
  
  ais = readRDS(paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/prepped/ais_data.rds'))
  setnames(ais, 'region', 'id')
  
  my_fun = function(x) {
    x = 100*x
    x = round(x, 1)
    return(x)
  }
  
  ais = ais[ , lapply(.SD, my_fun), .SDcols=2:4, by=id]
  
  
  mapDataAIS= merge(reg_coord, ais, by='id')
  mapDataAIS = melt(mapDataAIS, id.vars=c('long','lat', 'id','group','order','hole','piece'))
  

  ais$id = gsub(ais$id, pattern='_', replacement='-')
  ais[grep('^Central', id), id:=(gsub(id, pattern='-', replacement=' '))]
  ais[grep('^West', id), id:=(gsub(id, pattern='-', replacement=' '))]
  
  names_ais = names[ ,.(id, long, lat)]
  names_ais = merge(names_ais, ais, by='id')
  names_ais = melt(names_ais, id.vars=c('id', 'long', 'lat'))
  
  mapDataAIS$variable = factor( mapDataAIS$variable, c('art_coverage_2011', 'art_coverage', 'art_coverage_gbd'), 
                                c('2011 ART Coverage\nAIDS Indicator Survey', '2016 ART Coverage\nAIS PHIA Adjusted', '2016 ART Coverage\nAIS GBD Adjusted'))
  

  names_ais$variable = factor(names_ais$variable, c('art_coverage_2011', 'art_coverage', 'art_coverage_gbd'), 
                                c('2011 ART Coverage\nAIDS Indicator Survey', '2016 ART Coverage\nAIS PHIA Adjusted', '2016 ART Coverage\nAIS GBD Adjusted'))
  
  #----------------------
  
  
  
  adj =  labels = names[ ,.(id, long, lat, phia, vl_adj)]
  labels = melt(labels, id.vars = c('id', 'long', 'lat'))
  
  labels[variable=='phia', variable:='Viral Load Suppression\nPHIA']
  labels[variable=='vl_hat', variable:='Reported Viral Load Suppression\nNational Dashboard*']
  
  
  labels = names[ ,.(id, long, lat, phia, vl_hat)]
  labels = melt(labels, id.vars = c('id', 'long', 'lat'))
  
  labels[variable=='phia', variable:='Viral Load Suppression\nPHIA']
  labels[variable=='vl_hat', variable:='Reported Viral Load Suppression\nNational Dashboard*']
  
  
  #---------------------------
  # colors
  
  colors = c('#CAF270', '#73D487', '#30B097', '#288993', '#41607A', '#453B52')
  mapColors = colorRampPalette(colors)
  mapColors = mapColors(10)
  ratio_colors = brewer.pal(6, 'BuGn')
  art_colors = brewer.pal(8, 'Spectral')
  
  
  # -------------------------------------------------------------------------------------------
  # Make graphs
  
  # create graphs of original data 

  # map PHIA and VLD side-by-side before adjustment 
  vars1 = c('Viral Load Suppression\nPHIA', 'Viral Load Suppression\nUganda VL Dashboard')
  p1 = ggplot(mapDataReg[variable %in% vars1], aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    facet_wrap(~variable) + 
    scale_fill_gradientn('VLS %', colours=ratio_colors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='Comparison of PHIA and Uganda VL Dashboard before adjustment',  subtitle='August 2016 - March 2017') + 
    theme_minimal(base_size=16) + 
    theme(plot.caption=element_text(size=10)) +
    geom_label_repel(data = p1_labels, aes(label = value, x = long, y = lat, group = value), inherit.aes=FALSE, size=5)
  
  
  # ART coverage 
  p2 = ggplot(mapDataAIS, aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    facet_wrap(~variable) + 
    scale_fill_gradientn('ART Coverage (%)', colours=art_colors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='ART Coverage Estimates') + 
    theme_minimal(base_size=16) + 
    theme(plot.caption=element_text(size=10)) +
    geom_label_repel(data = names_ais, aes(label = value, x = long, y = lat, group = value), inherit.aes=FALSE, size=5)
  

  
  
  
  
  
  
  # map PHIA and VLD side-by-side
  vars1 = c('Reported Viral Load Suppression\nNational Dashboard*', 'Viral Load Suppression\nPHIA')
  p1 = ggplot(mapDataReg[variable %in% vars1], aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    facet_wrap(~variable) + 
    geom_path(color='grey95', size=.05) + 
    scale_fill_gradientn('%', colours=mapColors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='August 2016 - March 2017', caption='*Adjusted for ART coverage using 2011 AIS and PHIA 2016') + 
    theme_minimal(base_size=16) + 
    theme(plot.caption=element_text(size=10)) +
    geom_label_repel(data = labels, aes(label = value, x = long, y = lat, group = value), inherit.aes=FALSE, size=5)
  
  # bar graphs
  vars2 = c('National Dashboard*', 'PHIA')
  p2 = ggplot(long[variable %in% vars2], aes(x=region10_name, y=value, fill=variable)) + 
    geom_bar(stat='identity', position='dodge') + 
    scale_fill_manual('Data Source', values=colors[c(6,4)]) + 
    labs(y='Viral Load Suppression (%)', x='', caption='*Adjusted for ART coverage') + 
    theme_bw(base_size=14) + 
    theme(plot.caption=element_text(size=10)) 
  
  # scatterplot
  min = min(regData$phia_vls,regData$vld_suppression_adj)
  max = max(regData$phia_vls,regData$vld_suppression_adj)
  # predData[lwr<min, lwr:=min]
  p3 = ggplot(regData, aes(x=phia_vls, y=vld_suppression_adj)) + 
    geom_ribbon(data=predData, aes(ymin=lwr, ymax=upr, x=phia_vls), fill='grey75', alpha=.3, inherit.aes=FALSE) + 
    geom_line(data=predData, aes(y=fit), color='red', linetype='longdash', size=1.25) + 
    geom_abline(slope=1, intercept=0) + 
    geom_point(color=colors[4], size=4.5, alpha=.7, stroke=0) + 
    scale_fill_manual('Data Source', values=colors[c(6,4)]) + 
    labs(title='Viral Load Suppression', subtitle='Comparison of Sources', 
         y='National Dashboard*', x='PHIA', caption='*Adjusted for ART coverage') + 
    coord_cartesian(ylim=c(min,max), xlim=c(min,max)) + # zoom in rather than clip the axes to keep the ribbon
    theme_bw(base_size=14) + 
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), plot.caption=element_text(size=10))	
  
  # maps showing uncertainty
  
  
  # map VLD raw and corrected at district level
  p5a = ggplot(mapDataDist, aes(x=long, y=lat, group=group, fill=vld_suppression_adj)) + 
    geom_polygon() + 
    geom_path(color='grey95', size=.05) + 
    scale_fill_gradientn('%', colours=mapColors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='Original') + 
    theme_minimal(base_size=16) + 
    theme(plot.title=element_text(hjust=0.5))
  p5b = ggplot(mapDataDist, aes(x=long, y=lat, group=group, fill=vld_suppression_hat)) + 
    geom_polygon() + 
    geom_path(color='grey95', size=.05) + 
    scale_fill_gradientn('%', colours=mapColors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='Corrected') + 
    theme_minimal(base_size=16) + 
    theme(plot.title=element_text(hjust=0.5))
  
  # graph showing changing equity over time
  p6 = ggplot(distDataAnnual, aes(x=factor(year), y=vld_suppression_hat)) +
    geom_violin(fill=colors[4], alpha=.7) + 
    labs(title='Equity', subtitle='Geographic Spread', y='Viral Load Suppression', 
         x=sprintf('< Density of Districts >')) + 
    theme_minimal(base_size=16) + 
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), 
          axis.title.x=element_text(size=11))
  
  # map of districts over time
  p7 = ggplot(mapDataDistAnnual, aes(x=long, y=lat, group=group, fill=vld_suppression_hat)) + 
    geom_polygon() + 
    geom_path(color='grey95', size=.05) + 
    scale_fill_gradientn('%', colours=mapColors) + 
    coord_fixed(ratio=1) + 
    facet_wrap(~year) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    theme_minimal(base_size=16) + 
    theme(plot.title=element_text(hjust=0.5))
  
  # map of district-level % change
  p7 = ggplot(mapDataDistChange, aes(x=long, y=lat, group=group, fill=roc)) + 
    geom_polygon() + 
    geom_path(color='grey95', size=.05) + 
    scale_fill_gradientn('% Change', colours=mapColors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='Percent Change in Viral Load Suppression', subtitle='2016-2017') + 
    theme_minimal(base_size=16) + 
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
  # -------------------------------------------------------------------------------------------
  
  
  # -----------------------------------------------------------
  # Save
  pdf(outFile, height=6, width=9)
  
  
  
  
  p1
  p2
  p3
  grid.arrange(p5a, p5b, ncol=2, top='Viral Load Suppression')
  p6
  p7
  dev.off()
  # -----------------------------------------------------------
  
  
  # --------------
  # End function
}
# ------------------