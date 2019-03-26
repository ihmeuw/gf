

#----------------------------------------
# upload the shape files 

# upload both the dps and health zone shape files 
shape_dir = paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/")
dps_shapefile = paste0( shape_dir, "gadm36_COD_shp/gadm36_COD_1.shp")
hz_shapefile = paste0( shape_dir, "health2/health2.shp")

drcShape_dps = shapefile(dps_shapefile)
drcShape_hz = shapefile(hz_shapefile)

# plot the shape files to be ensure they worked
plot(drcShape_dps)

#---------------------------------------
# merge the shape file and data on name

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
coordinates = data.table(fortify(drcShape_dps, region='NAME_1')) 
coordinates$id = gsub('-', ' ', coordinates$id)
coordinates$id = gsub('�???quateur', 'Equateur', coordinates$id)
coordinates$id = gsub('Haut Uélé', 'Haut Uele', coordinates$id)
coordinates$id = gsub('Bas Uélé', 'Bas Uele', coordinates$id)
coordinates$id = gsub('Kasaï', 'Kasai', coordinates$id)
coordinates$id = gsub('Maï Ndombe', 'Mai Ndombe', coordinates$id)

dt$dps = gsub('Maindombe', 'Mai Ndombe', dt$dps)

# check to make sure all dps are included - output should be 0
dps = dt[ ,unique(dps)]
ids = coordinates[ ,unique(id)]
dps[!dps %in% ids]

# coordinates by year for faceting (repeat 2 times for 2 years of data)
coordinates_ann = rbind(coordinates, coordinates)
coordinates_ann[, year:=rep(2017:2018, each=nrow(coordinates))]

# --------------------------------------------------------------------
# create labels for the regional maps

# identify centroids and label them
names = data.table(coordinates(drcShape_dps))
setnames(names, c('long', 'lat'))
names[ , region:=unique(coordinates$id)]

# create labels
names[region=='Kinshasa' | region=='Haut Katanga' | region=='Lualaba', 
      label:=paste0(region, ': PEPFAR DPS')]

#----------------------------
# facilities reporting
# facilities reporting and patients on art by sex
fac = dt[ ,.(facilities_reporting=length(unique(org_unit))), by=dps]

pts = dt[date=='2018-04-01']
pts = pts[element=="Patients still on ART in the structure" | element=="Patients on ART in the Podi"]
pts = pts[ , .(value=sum(value, na.rm=T)), by=dps]
fac = merge(fac, pts, by='dps', all=T)
setnames(fac, 'dps', 'id')

# merge with the coordinates
coord_fac = merge(fac, coordinates, by='id', all=T)

#----------------------------------------

 pdf(paste0(dir, 'outputs/pnls/arv_initial_maps.pdf'), width=12, height=7)

# Number of facilities reporting 
ggplot(coord_fac, aes(x=long, y=lat, group=group, fill=facilities_reporting)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  scale_fill_gradientn(colors=brewer.pal(9, 'Blues')) +
  labs(title="Number of facilities reporting by DPS", 
       subtitle='2017 - August 2018',
       fill="Facilities reporting") +
  theme(plot.title=element_text(size=22, vjust=-1), plot.subtitle=element_text(size=18),
        legend.title=element_text(size=18), legend.text=element_text(size=16)) +
  geom_label_repel(data = names, aes(label = label, x = long, y = lat, group = region), inherit.aes=FALSE, size=4)

# PLHIV enrolled on ART
ggplot(coord_fac, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  scale_fill_gradientn(colors=brewer.pal(9, 'Greens')) +
  labs(title="PLHIV enrolled on ART in health facilities and PODI",  
       subtitle = 'April 2018; not all facilities report every month (some DPS missing)',
       caption="Source: SNIS - PNLS Canevas Unique FOSA", 
       fill="PLHIV on ART") +
  theme(plot.title=element_text(size=22, vjust=-1), plot.subtitle=element_text(size=18),
        legend.title=element_text(size=18), legend.text=element_text(size=16),
        plot.caption=element_text(size=18)) +
geom_label_repel(data = names, aes(label = label, x = long, y = lat, group = region), inherit.aes=FALSE, size=4)

#---------------------------------------------
# Rationalization
coord_fac[id=='Haut Katanga' | id=='Lualaba', rationalization:='PEPFAR']
coord_fac[id=='Kinshasa', rationalization:='Shared']
coord_fac[is.na(rationalization), rationalization:='Global Fund' ]

# add labels
names[region=='Haut Katanga' | region=='Lualaba' ,funder:=paste0(region, ': PEPFAR')]
names[region=='Kinshasa' ,funder:=paste0(region, ': Shared')]
names[is.na(funder),funder:=paste0(region, ': GF')]

tri_colors = c('#a50026', '#fdae61', '#abd9e9')

ggplot(coord_fac, aes(x=long, y=lat, group=group, fill=factor(rationalization))) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  labs(title="Funders by DPS: Rationalization", fill="Funder") +
  scale_fill_manual(values=rev(tri_colors)) +
  theme(plot.title=element_text(size=22, vjust=-1), plot.subtitle=element_text(size=18),
        legend.title=element_text(size=18), legend.text=element_text(size=16),
        plot.caption=element_text(size=18)) +
  geom_label_repel(data = names, aes(label = region, x = long, y = lat, group = region), inherit.aes=FALSE, size=4)


#--------------------------------
# viral load
vl = dt[element== "PLHIV who received a viral load test" | element=="PLHIV on ARVs who received a viral load test after six months" ,.(value=sum(value)), by=.(dps, year=year(date))]
setnames(vl, 'dps', 'id')

# merge with the coordinates
coord_vl = merge(vl, coordinates_ann, by=c('id', 'year'), all=T)

pdf(paste0(dir, 'outputs/pnls/vl_map.pdf'), width=12, height=7)

# Number of facilities reporting 
ggplot(coord_vl, aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  scale_fill_gradientn(colors=brewer.pal(9, 'Reds')) +
  facet_wrap(~year) +
  labs(title="Number of viral load tests performed", 
       subtitle='2017 - August 2018',
       fill="Tests") +
  theme(plot.title=element_text(size=22, vjust=-1), plot.subtitle=element_text(size=18),
        legend.title=element_text(size=18), legend.text=element_text(size=16), 
        strip.text=element_text(size=16)) +
  geom_label_repel(data = names, aes(label = label, x = long, y = lat, group = region), 
                   inherit.aes=FALSE, size=4)


coord_vl_gf = coord_vl
coord_vl_gf[id=='Kinshasa' | id=='Lualaba' | id=='Haut Katanga', value:=NA]

ggplot(coord_vl_gf[year==2018], aes(x=long, y=lat, group=group, fill=value)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  theme_void() +
  scale_fill_gradientn(colors=brewer.pal(9, 'Blues')) +
  facet_wrap(~year) +
  labs(title="Number of viral load tests performed: Global Fund DPS", 
       fill="Tests") +
  theme(plot.title=element_text(size=22, vjust=-1), plot.subtitle=element_text(size=18),
        legend.title=element_text(size=18), legend.text=element_text(size=16), 
        strip.text=element_text(size=16)) 


dev.off()

#---------------------------------------------
# all variables in a single data set

# shape the data wide


# merge on district id - all time total, annual totals, sex stratified totals
coordinates = merge(coordinates, ratio_table, by="id", all.x=TRUE)
coordinates_year = merge(coordinates_ann, ratio_year, by=c('id', 'year'), all.x=TRUE)
coordinates_female = merge(coordinates_ann, ratio_female, by=c('id','year'), all.x=TRUE)
coordinates_male = merge(coordinates_ann, ratio_male, by=c('id','year'), all.x=TRUE)



art = dt[ ,.(value=sum(value)), by=.(element, dps)]


# ARV Maps

library


# Number of facilities reporting by year

# Number of facilities reporting by level in 2018

# ART enrollment

# On IPT

# TB/HIV

# Viral load testing 