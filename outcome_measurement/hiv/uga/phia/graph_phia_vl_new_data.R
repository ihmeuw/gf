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
graphVL_time2 = function(dir=NULL) { 
  # ----------------------------------------
  
  # ----------------------------------------------------------------------
  # Files and directories
  
  # set working directory
  setwd(paste0(j, '/Project/Evaluation/GF/mapping/uga/'))
  
  # uganda shapefile
  DistMap = shapefile('uga_dist112_map.shp')
  
  # outfile 
  outFile = paste0(dir, 'output/phia_vl_dashboard_prospective.pdf')

  # -------------------------------------------------------------------------------------------
  # Set up to graph
  
  #-------------------------------
  # fortify the maps 

  dist_coord = fortify(DistMap, region='dist112')
  
  #-------------------------------
  # get names associated with ids
  dist_names = cbind(id=DistMap@data$dist112, district=DistMap@data$dist112_na)
  
  #-------------------------------
  # apply the ratios and round
  
  distDataAll[, vld_suppression_hat:=vld_suppression_adj/ratio]
  distDataAnn[, vld_suppression_hat:=vld_suppression_adj/ratio]
  
  # round 
  distDataAll = distDataAll[ , lapply(.SD, round, 1), .SDcols=3:13, by=.(district, region)]
  distDataAnn = distDataAnn[ , lapply(.SD, round, 1), .SDcols=4:14, by=.(district, region, year)]
   
  distDataAll = merge(distDataAll, dist_names, by='district')
  distDataAnn = merge(distDataAnn, dist_names, by='district')
  #------------------------------
  
  #----------------------------------
  # district map
  mapDataDist = merge(dist_coord, distDataAll, by='id', all.x=TRUE)
  
  # reset the names for the comparative maps for VL Dashboard adjustments
  setnames(mapDataDist, c('vld_suppression_adj', 'vld_suppression_hat'),
           c('VL Dashboard\nAdjusted for ART Coverage', 'VL Dashboard\nAdjusted for ART Coverage and PHIA*'))
  
  # shape district data long
  id_vars = c("id", "long", "lat", "order",
              "hole", "piece", "group", "district", "region")  
  mapDataDist = data.table(melt(mapDataDist, id.vars=id_vars))
  
  #----------------------------------
  # district map annual data 
  
  dist_coord_ann = data.table(rbind(dist_coord, dist_coord))
  dist_coord_ann[ , year:=rep(2017:2018, each=nrow(dist_coord))]
  
  mapAnn = merge(dist_coord_ann, distDataAnn, by=c('id', 'year'), all.x=TRUE)
  
  id_vars = c("id", "long", "lat", "order",
              "hole", "piece", "group", "district", "region", 'year')  
  mapAnn = data.table(melt(mapAnn, id.vars=id_vars))

  #---------------------------
  # percent change by district
  
  # calculate rates of change
  change = distDataAnn[  , .(vld_suppression, vld_suppression_hat), by=.(district, year, id)]
  change17 = change[year==2017]
  change18 = change[year==2018]
  
  setnames(change17, 'vld_suppression', 'o2017')
  setnames(change18, 'vld_suppression', 'o2018')
  setnames(change17, 'vld_suppression_hat', 'h2017')
  setnames(change18, 'vld_suppression_hat', 'h2018')
  change17[ ,year:=NULL]
  change18[ ,year:=NULL]
  change = merge(change17, change18, by=c('district', 'id'))
  
  change[ ,og_roc:=(o2018 - o2017)]
  change[ ,roc:=(h2018 - h2017)]
  
  # map the rate of change
  mapChange = merge(dist_coord, change, by='id', all.x=TRUE)
  
  # map the rate of change in original versus all PLHIV
  id_vars = c("id", "long", "lat", "order",
              "hole", "piece", "group", "district")  
  mapChange2 = data.table(melt(mapChange, id.vars=id_vars))
  
  mapChange2[variable=='og_roc', variable:='2017 - 2018 Rate of Change, VL Dashboard']
  mapChange2[variable=='roc', variable:='2017 - 2018 Rate of Change, VL Dashboard Adjusted for PHIA*']  
  #---------------------------
  # colors
  
  colors = c('#CAF270', '#73D487', '#30B097', '#288993', '#41607A', '#453B52')
  mapColors = colorRampPalette(colors)
  mapColors = mapColors(10)
  ratio_colors = brewer.pal(6, 'BuGn')
  art_colors = brewer.pal(8, 'Spectral')
  compare_colors = brewer.pal(11, 'RdYlBu')
  lavender = brewer.pal(9, 'Blues')
  
  # -------------------------------------------------------------------------------------------
  # Make graphs
  
  # map VL adjusted for PHIA
  vars1 = c('VL Dashboard\nAdjusted for ART Coverage and PHIA*')
  p1 = ggplot(mapDataDist[variable %in% vars1], aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() +
    scale_fill_gradientn('VS (%)', colours=compare_colors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title = 'Viral suppression in PLHIV, Uganda'  , subtitle='January 2017 - June 2018', 
         caption='*Adjusted to reflect VS among all PLHIV using AIS 2011 ART coverage and PHIA 2016') + 
    theme_minimal(base_size=16) + 
    theme(plot.caption=element_text(size=14)) 


  # VL Dashboard before and after adjustment
  vars2 =  c('VL Dashboard\nAdjusted for ART Coverage', 'VL Dashboard\nAdjusted for ART Coverage and PHIA*')
  p2 = ggplot(mapDataDist[variable %in% vars2], aes(x=long, y=lat, group=group, fill=value)) + 
    geom_polygon() + 
    facet_wrap(~variable) + 
    scale_fill_gradientn('VS (%)', colours=art_colors) + 
    coord_fixed(ratio=1) + 
    scale_x_continuous('', breaks = NULL) + 
    scale_y_continuous('', breaks = NULL) + 
    labs(title='Uganda Viral Load Dashboard before and after PHIA adjustment*',  subtitle='January 2017 - June 2018', 
         caption='*Adjusted using LME to reflect PHIA 2016') + 
    theme_minimal(base_size=16) + 
    theme(plot.caption=element_text(size=10)) 


  #--------------------------
  # Annual plots

  # viral suppression facet wrapped by year
   p3 = ggplot(mapAnn[variable=='vld_suppression_hat'], aes(x=long, y=lat, group=group, fill=value)) +
    geom_polygon() +
    geom_path(color='grey95', size=.05) +
    scale_fill_gradientn('VS (%)', colours=compare_colors) +
    coord_fixed(ratio=1) +
    facet_wrap(~year) +
    labs(title='Viral suppression among PLHIV, Uganda', caption='Projected using PHIA 2016') +
    scale_x_continuous('', breaks = NULL) +
    scale_y_continuous('', breaks = NULL) +
    theme_minimal(base_size=16) +
    theme(plot.title=element_text(hjust=0.5))
  
  # equity by geographic spread
  p4 = ggplot(distDataAnn, aes(x=factor(year), y=vld_suppression_hat)) +
    geom_violin(fill=colors[4], alpha=.7) +
    labs(title='Equity', subtitle='Geographic Spread', y='Viral Suppression',
         x=sprintf('< Density of Districts >')) +
    theme_minimal(base_size=16) +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
          axis.title.x=element_text(size=11))
  
  # map of district-level % change
  p5 = ggplot(mapChange, aes(x=long, y=lat, group=group, fill=roc)) +
    geom_polygon() +
    geom_path(color='grey95', size=.05) +
    scale_fill_gradientn('% Change', colours=mapColors) +
    coord_fixed(ratio=1) +
    scale_x_continuous('', breaks = NULL) +
    scale_y_continuous('', breaks = NULL) +
    labs(title='Percent Change in Viral Suppression', subtitle='2017-2018') +
    theme_minimal(base_size=16) +
    theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))
  
  
  # rate of change in the dashboard compared to rate of change in projected estimates
  vars6 = c('2017 - 2018 Rate of Change, VL Dashboard', '2017 - 2018 Rate of Change, VL Dashboard Adjusted for PHIA*')
  p6 = ggplot(mapChange2[variable %in% vars6], aes(x=long, y=lat, group=group, fill=value)) +
  geom_polygon() +
  geom_path(color='grey95', size=.05) +
  scale_fill_gradientn('VS (%)', colours=lavender) +
  coord_fixed(ratio=1) +
  facet_wrap(~variable) +
  labs(title='Viral suppression among PLHIV, Uganda', caption='*Projected using PHIA 2016') +
  scale_x_continuous('', breaks = NULL) +
  scale_y_continuous('', breaks = NULL) +
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
  
  dev.off()
  # -----------------------------------------------------------
  
  
  # --------------
  # End function
}
# ------------------
