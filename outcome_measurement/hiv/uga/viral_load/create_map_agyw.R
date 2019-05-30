graph_region = function(x, region=TRUE) { 
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
  # fortify the maps 
  reg_coord = fortify(RegMap)
  
  #-------------------------------
 
  if (region==TRUE) { 
    
    # all age categories
    regData_age = dt[ , lapply(.SD, sum), .SDcols=12:18, by=.(region, sex, age, year)]
    
    # agyw and same-age men
    regData_15_24 = dt[(age=='15 - 19' | age=='20 - 24') & (year==2017 | year==2018)]
    regData_15_24 = regData_15_24[, lapply(.SD, sum), .SDcols=12:18, by=.(id = region, sex, year)]
    regData_15_24[ , vl_suppression:=round(100*(suppressed/valid_results), 1)] 
    
    # double the coordinates
    reg_coord_year = data.table(rbind(reg_coord, reg_coord))
    reg_coord_year[ ,year:=rep(2017:2018, each=nrow(reg_coord))]
    reg_coord_year[ ,sex:='Female']
    
    rm = data.table(rbind(reg_coord, reg_coord))
    rm[ ,year:=rep(2017:2018, each=nrow(reg_coord))]
    rm[ ,sex:='Male']
    reg_coord_year = rbind(rm, reg_coord_year)

    #------------------------------
    # merge to map and melt data
    mapDataReg = merge(regData_15_24, reg_coord_year, by=c('id', 'year', 'sex'), all.x=TRUE)
    mapDataReg = melt(mapDataReg, id.vars=c('long','lat', 'id','group','order','hole','piece', 'year', 'sex'))
    mapDataReg = data.table(mapDataReg)
    
    # --------------------------------------------------------------------
    # create labels for the regional maps
    
    # identify centroids and label them
    names = data.table(coordinates(RegMap))
    setnames(names, c('long', 'lat'))
    names[ , id:=unique(reg_coord$id)]
    
    # merge in the ratios for complete labels
    names = merge(names, regData_15_24, by='id')
    
    # replace labels with hyphens to match phia graphics
    names$id = gsub(names$id, pattern='_', replacement='-')
    names[grep('^Central', id), id:=(gsub(id, pattern='-', replacement=' '))]
    names[grep('^West', id), id:=(gsub(id, pattern='-', replacement=' '))]
    
    # create the final labels
    names[ , label:=paste0(id, ': ', vl_suppression, '%')]
    
    # finalize labels
    labels_2017 = names[year==2017, .(long, lat, sex, label)]
    labels_2018 = names[year==2018, .(long, lat, sex, label)]
    
    # --------------------------------------

    p1 = ggplot(mapDataReg[year==2018 & variable=='vl_suppression'], aes(x=long, y=lat, group=group, fill=value)) + 
      geom_polygon() + 
      scale_fill_gradientn('VS %', colours=brewer.pal(9, 'Greens')) + 
      coord_fixed(ratio=1) + 
      scale_x_continuous('', breaks = NULL) + 
      scale_y_continuous('', breaks = NULL) + 
      facet_wrap(~sex) + 
      labs(title='2018 viral suppression, females and males 15 - 24', 
           caption='Source: Uganda Viral Load Dashboard') + 
      theme_minimal(base_size=20) + 
      theme(plot.caption=element_text(size=14)) +
      geom_label_repel(data = labels_2018, aes(label = label, x = long, y = lat, group = label), inherit.aes=FALSE, size=4) 
    
    p2 = ggplot(mapDataReg[year==2017 & variable=='vl_suppression'], aes(x=long, y=lat, group=group, fill=value)) + 
      geom_polygon() + 
      facet_wrap(~sex) + 
      scale_fill_gradientn('VS %', colours=brewer.pal(9, 'Blues')) + 
      coord_fixed(ratio=1) + 
      scale_x_continuous('', breaks = NULL) + 
      scale_y_continuous('', breaks = NULL) + 
      labs(title='2017 viral suppression, females and males 15 - 24', 
           caption='Source: Uganda Viral Load Dashboard') + 
      theme_minimal(base_size=20) + 
      theme(plot.caption=element_text(size=14)) +
      geom_label_repel(data = labels_2017, aes(label = label, x = long, y = lat, group = label), inherit.aes=FALSE, size=4) 
    
    
    pdf(paste0(outDir, 'age_maps.pdf'), width=12, height =9)
    p1
    p2
    dev.off()
    
    } else {print("District level?")}
}
  
  
  