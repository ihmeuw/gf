# ---------------------------------------------------------
# David Phillips
# Caitlin O'Brie-Carelli

# 10/24/2018
# Function to graphs the PHIA-VLD comparisons for overlapping time period
# overlap: August 2016 - March 2017

#------------------------------------
# Intended to be run by compare_phia_to_vl_dashboard.r
# Inputs:
# dir - directory where the output should go
# nothing else; pulls objects from memory
# Outputs:
# nothing; saves to outFile
# ---------------------------------------------------------


# --------------------------------------------
# Start function
graphVL_time1 = function(dir=NULL) { 
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
  outFile = paste0(dir, 'output/phia_vl_dashboard_same_time_period.pdf')
  
  # ----------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------------
  # Set up to graph
  
  #-------------------------------
  # fortify the maps 
  reg_coord = fortify(RegMap)
  dist_coord = fortify(DistMap, region='dist112')
  #-------------------------------
  
  # get names associated with ids
  dist_names = cbind(id=DistMap@data$dist112, district=DistMap@data$dist112_na)
  
  # round 
  regData = regData[ , lapply(.SD, round, 1), .SDcols=2:12, by=region]
  distData = distData[ , lapply(.SD, round, 1), .SDcols=3:13, by=.(district, region)]
  distData = merge(distData, dist_names, by='district')
  
  #-------------------------------
  # reshape long for bar and time series graphs 
  long = melt(regData, id.vars='region')
  
  # clean up variable labels
  long[variable=='phia_vls', variable:='PHIA']
  long[variable=='vld_suppression_adj', variable:='National Dashboard*']
  
  long$region = gsub(long$region , pattern='_', replacement='-')
  long[grep('^Central', region), region:=(gsub(region, pattern='-', replacement=' '))]
  long[grep('^West', region), region:=(gsub(region, pattern='-', replacement=' '))]
  
  #------------------------------
  # merge to map and melt data
  mapDataReg = merge(reg_coord, regData, by.x='id', by.y='region', all.x=TRUE)
  mapDataReg = melt(mapDataReg, id.vars=c('long','lat', 'id','group','order','hole','piece'))
  mapDataReg = data.table(mapDataReg)
  
  #-----------------------------
  # change labels on regional data
  mapDataReg[variable=='phia_vls', variable:='Viral Suppression\nPHIA']
  mapDataReg[variable=='phia_vls_lower', variable:='PHIA Lower']
  mapDataReg[variable=='phia_vls_upper', variable:='PHIA Upper']
  mapDataReg[variable=='samples', variable:='N Samples']
  mapDataReg[variable=='suppressed', variable:='N Samples Suppressed']
  mapDataReg[variable=='vld_suppression', variable:='Viral Suppression\nUganda VL Dashboard']
  
  mapDataReg[variable=='art_coverage_2011', variable:='ART Coverage\n2011 AIS']
  mapDataReg[variable=='art_coverage', variable:='ART Coverage\n2016 adjusted']

  mapDataReg[variable=='vld_suppression_adj', variable:='Viral Suppression\n Adjusted for ART Coverage']  
  mapDataReg[variable=='ratio', variable:='Ratio of PHIA to VLD']  
  mapDataReg[variable=='vld_suppression_hat', variable:='Viral Suppression\nNational Dashboard Adjusted for PHIA*']

  #----------------------------------
  # district map
  mapDataDist = merge(dist_coord, distData, by='id', all.x=TRUE)
  
  # reset the names for the comparative maps for VL Dashboard adjustments
  setnames(mapDataDist, c('vld_suppression_adj', 'vld_suppression_hat'),
           c('VL Dashboard\nAdjusted for ART Coverage', 'VL Dashboard\nAdjusted for ART Coverage and PHIA'))
  
  # shape district data long
  id_vars = c("id", "long", "lat", "order",
              "hole", "piece", "group", "district", "region")  
  mapDataDist = data.table(melt(mapDataDist, id.vars=id_vars))

  # --------------------------------------------------------------------
  # create labels for the regional maps
  
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
  # p1 label - viral suppression in the original data sources, no adjustment 
  p1_labels = names[ ,.(id, long, lat, phia_vls, vld_suppression)]
  p1_labels[ , phia:= paste0(id, ': ', phia_vls, '%' )]
  p1_labels[ , vl:= paste0(id, ': ', vld_suppression, '%' )]
  p1_labels[ ,c('phia_vls', 'vld_suppression'):=NULL]
  p1_labels = melt(p1_labels, id.vars = c('id', 'long', 'lat'))
  
  p1_labels[variable=='phia', variable:='Viral Suppression\nPHIA']
  p1_labels[variable=='vl', variable:='Viral Suppression\nUganda VL Dashboard']
  
  #----------------------
  # p2 label - national and subnational ART coverage; three sources 
  
  # upload the ART coverage data table (saved from prepped data)
  ais = readRDS(paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/phia_2016/prepped/ais_data.rds'))
  setnames(ais, 'region', 'id')
  
  # round the percentages 
  my_fun = function(x) {
    x = 100*x
    x = round(x, 1)
    return(x)
  }
  
  ais = ais[ , lapply(.SD, my_fun), .SDcols=2:4, by=id]
  
  # merge the data and shape it long
  mapDataAIS= merge(reg_coord, ais, by='id')
  mapDataAIS = melt(mapDataAIS, id.vars=c('long','lat', 'id','group','order','hole','piece'))

  ais$id = gsub(ais$id, pattern='_', replacement='-')
  ais[grep('^Central', id), id:=(gsub(id, pattern='-', replacement=' '))]
  ais[grep('^West', id), id:=(gsub(id, pattern='-', replacement=' '))]
  
  # create labels for the ART coverage map
  names_ais = names[ ,.(id, long, lat)]
  names_ais = merge(names_ais, ais, by='id')
  names_ais = melt(names_ais, id.vars=c('id', 'long', 'lat'))

  
  mapDataAIS$variable = factor( mapDataAIS$variable, c('art_coverage_2011', 'art_coverage', 'art_coverage_gbd'), 
                                c('2011 ART Coverage\nAIDS Indicator Survey', '2016 ART Coverage\nAIS adjusted using PHIA', 
                                  '2016 ART Coverage\nAIS adjusted using GBD'))
  
  names_ais$variable = factor(names_ais$variable, c('art_coverage_2011', 'art_coverage', 'art_coverage_gbd'), 
                                c('2011 ART Coverage\nAIDS Indicator Survey', '2016 ART Coverage\nAIS adjusted using PHIA', '2016 ART Coverage\nAIS adjusted using GBD'))
  
  setnames(names_ais, 'value', 'old')
  names_ais[variable=='2011 ART Coverage\nAIDS Indicator Survey', value:=paste0(id, ': ', old, '%' )]
  names_ais[variable=='2016 ART Coverage\nAIS adjusted using PHIA', value:=paste0(id, ': ', old, '%' )]
  names_ais[variable=='2016 ART Coverage\nAIS adjusted using GBD', value:=paste0(id, ': ', old, '%' )]
  names_ais[ ,old:=NULL]
  
  #----------------------
  # p7 labels - final product comparing phia and vl adjusted for phia
  
  names_final = names[ ,.(id, long, lat, phia_vls, vld_suppression_hat)]
  names_final = melt(names_final, id.vars=c('id', 'long', 'lat'))
  names_final$variable = factor(names_final$variable, c('phia_vls', 'vld_suppression_hat'), 
                                c('Viral Suppression\nPHIA',  
                                'Viral Suppression\nNational Dashboard Adjusted for PHIA*'))
  names_final[ , value:=paste0(id, ': ', value, '%')]
  
  #-----------------------
  # p8 labels - final product with lower and upper bounds
  
  names_final2 = names[ ,.(id, long, lat, phia_vls_lower, phia_vls_upper, phia_vls, vld_suppression_hat)]
  names_final2 = melt(names_final2, id.vars=c('id', 'long', 'lat'))
  names_final2$variable = factor(names_final2$variable, c('phia_vls_lower', 'phia_vls_upper',
                                'phia_vls', 'vld_suppression_hat'), 
                                c('PHIA Lower', 'PHIA Upper', 'Viral Suppression\nPHIA',  
                                'Viral Suppression\nNational Dashboard Adjusted for PHIA*'))
  names_final2[ , value:=paste0(id, ': ', value, '%')]
  

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
  vars1 = c('Viral Suppression\nPHIA', 'Viral Suppression\nUganda VL Dashboard')
  p1 = ggplot(mapDataReg[variable %in% vars1], aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    facet_wrap(~variable) + 
    scale_fill_gradientn('VS %', colours=ratio_colors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='Comparison of PHIA and Uganda VL Dashboard before adjustment*',  subtitle='August 2016 - March 2017', 
         caption='*As reported: PHIA VS represents all PLHIV; VL Dashboard represents PLHIV enrolled in care') + 
    theme_minimal(base_size=16) + 
    theme(plot.caption=element_text(size=10)) +
    geom_label_repel(data = p1_labels, aes(label = value, x = long, y = lat, group = value), inherit.aes=FALSE, size=3)
  
  # ART coverage 
  p2 = ggplot(mapDataAIS, aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    facet_wrap(~variable) + 
    scale_fill_gradientn('On ART (%)', colours=art_colors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='ART Coverage Estimates', subtitle='Scaled using 2016 national PHIA and GBD estimates') + 
    theme_minimal(base_size=16) + 
    theme(plot.caption=element_text(size=10)) +
    geom_label_repel(data = names_ais, aes(label = value, x = long, y = lat, group = value), inherit.aes=FALSE, size=3)
  
  # bar graph - phia compared to national dashboard adjusted for art coverage 
  vars3 = c('National Dashboard*', 'PHIA')
  p3 = ggplot(long[variable %in% vars3], aes(x=region, y=value, fill=variable)) + 
    geom_bar(stat='identity', position='dodge') + 
    ylim(0, 100) +
    labs(title="Viral suppression among people on ART", y='Virally suppressed (%)', x='', caption='*Adjusted for ART coverage (% suppressed x % on ART)',
         subtitle='August 2016 - March 2017', fill='VS (%)') + 
    theme_bw(base_size=14) + 
    theme(plot.caption=element_text(size=10)) 
  
  # scatterplot - phia compared to dashboard adjusted for art coverage
  min = min(regData$phia_vls,regData$vld_suppression_adj)
  max = max(regData$phia_vls,regData$vld_suppression_adj)
  # predData[lwr<min, lwr:=min]
  p4 = ggplot(regData, aes(x=phia_vls, y=vld_suppression_adj)) + 
    geom_ribbon(data=predData, aes(ymin=lwr, ymax=upr, x=phia_vls), fill='grey75', alpha=.3, inherit.aes=FALSE) + 
    geom_line(data=predData, aes(y=fit), color='red', linetype='longdash', size=1.25) + 
    geom_abline(slope=1, intercept=0) + 
    geom_point(color=colors[4], size=4.5, alpha=.7, stroke=0) + 
    scale_fill_manual('Data Source', values=colors[c(6,4)]) + 
    labs(title='Viral Suppression (%): Data Source Comparison',
         y='National Dashboard*', x='PHIA', caption='*Adjusted for ART coverage (% suppressed x % on ART)') + 
    coord_cartesian(ylim=c(min,max), xlim=c(min,max)) + # zoom in rather than clip the axes to keep the ribbon
    theme_bw(base_size=14) + 
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), plot.caption=element_text(size=10))	
  
  # scatterplot - phia compared to dashboard adjusted to reflect phia
  # predData[lwr<min, lwr:=min]
  p5 = ggplot(regData, aes(x=phia_vls, y=vld_suppression_hat)) + 
    geom_ribbon(data=predData, aes(ymin=lwr, ymax=upr, x=phia_vls), fill='grey75', alpha=.3, inherit.aes=FALSE) + 
    geom_line(data=predData, aes(y=fit), color='red', linetype='longdash', size=1.25) + 
    geom_abline(slope=1, intercept=0) + 
    geom_point(color=colors[4], size=4.5, alpha=.7, stroke=0) + 
    scale_fill_manual('Data Source', values=colors[c(6,4)]) + 
    labs(title='Viral Suppression (%): Data Source Comparison', subtitle='Comparison of Sources: VS (%)', 
         y='National Dashboard*', x='PHIA', caption='*Adjusted for ART coverage and PHIA viral suppression using LME') + 
    coord_cartesian(ylim=c(min,max), xlim=c(min,max)) + # zoom in rather than clip the axes to keep the ribbon
    theme_bw(base_size=14) + 
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5), plot.caption=element_text(size=10))	
  
  # bar graph - VLD adjusted to reflect PHIA
  long[variable=='vld_suppression_hat', variable:='National VL Dashboard*']
  vars6 = c('National VL Dashboard*', 'PHIA')
  
  p6 =  ggplot(long[variable %in% vars6], aes(x=region, y=value, fill=variable)) + 
    geom_bar(stat='identity', position='dodge') + 
    ylim(0, 100) +
    labs(title="Viral suppression among PLHIV", y='Virally suppressed (%)', x='', caption='*Adjusted for ART coverage and PHIA viral suppression using LME',
         subtitle='August 2016 - March 2017', fill='VS (%)') + 
    theme_bw(base_size=14) + 
    theme(plot.caption=element_text(size=10)) 

    
  #---------------------------------------------
  # MAPS 
  
  # map PHIA and VLD adjusted to reflect PHIA
  vars7 = c('Viral Suppression\nNational Dashboard Adjusted for PHIA*', 'Viral Suppression\nPHIA')
  p7 = ggplot(mapDataReg[variable %in% vars7], aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    facet_wrap(~variable) + 
    scale_fill_gradientn('VS (%)', colours=mapColors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title = 'Comparison of PHIA and Uganda VL Dashboard after adjustment'  , subtitle='August 2016 - March 2017', 
         caption='*Adjusted to reflect VS among all PLHIV using AIS 2011 and PHIA 2016') + 
    theme_minimal(base_size=16) + 
    theme(plot.caption=element_text(size=10)) +
    geom_label_repel(data = names_final, aes(label = value, x = long, y = lat, group = value), inherit.aes=FALSE, size=3)
  
  # map showing uncertainty
  
  vars8 = c('PHIA Lower', 'Viral Suppression\nPHIA', 'PHIA Upper', 'Viral Suppression\nNational Dashboard Adjusted for PHIA*' )
  bounds = mapDataReg[variable %in% vars8]
  bounds$variable = factor(bounds$variable, c('PHIA Lower', 'Viral Suppression\nPHIA', 
                       'PHIA Upper', 'Viral Suppression\nNational Dashboard Adjusted for PHIA*'),
                       c('PHIA Lower', 'Viral Suppression\nPHIA', 
                         'PHIA Upper', 'Viral Suppression\nNational Dashboard Adjusted for PHIA*'))

  p8 = ggplot(bounds, aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    facet_wrap(~variable) + 
    scale_fill_gradientn('VS (%)', colours=mapColors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title = 'Comparison of PHIA and Uganda VL Dashboard after adjustment'  , subtitle='August 2016 - March 2017', 
         caption='*Adjusted to reflect VS among all PLHIV using AIS 2011 and PHIA 2016') + 
    theme_minimal() + 
    theme(plot.caption=element_text(size=10)) +
    geom_label_repel(data = names_final2, aes(label = value, x = long, y = lat, group = value), inherit.aes=FALSE, size=2)
  
  #--------------------------
  # District level plots

  compare_colors = brewer.pal(11, 'RdYlBu')
  
  # map VLD raw and corrected at district level
  vars9 = c('VL Dashboard\nAdjusted for ART Coverage', 'VL Dashboard\nAdjusted for ART Coverage and PHIA')
  p9 = ggplot(mapDataDist[variable %in% vars9], aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    geom_path(color='grey95', size=.05) + 
    facet_wrap(~variable) +
    scale_fill_gradientn('VLS (%)', colours=compare_colors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='Comparison: VL Dashboard Adjusted for ART Coverage and for ART Coverage & PHIA*', subtitle='August 2016 - March 2017',
         caption='*Linear mixed model was used to compare National Dashboard VLS to PHIA VLS for the same time period') + 
    theme_minimal(base_size=16) + 
    theme(plot.title=element_text(hjust=0.5))
  
  
  # -----------------------------------------------------------
  # Save
  pdf(outFile, height=6, width=12)
  
  p1
  p2
  p3
  p4
  p5
  p6
  p7
  p8
  p9
  
  dev.off()
  # -----------------------------------------------------------
  
  # --------------
  # End function
}
# ------------------