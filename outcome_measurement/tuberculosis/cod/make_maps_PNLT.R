# ----------------------------------------------
# Audrey Batzel
# 8/7/2018
# Map visualizations of PNLT data
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------
rm(list=ls())
library(raster)
library(rgeos)
library(data.table)
library(ggplot2)
library(maptools)
setwd('C:/local/gf/')
source('./core/standardizeDPSnames_function.R')
# ----------------------------------------------

# ----------------------------------------------
##set up J Drive 
# ----------------------------------------------
if (Sys.info()[1] == 'Windows') {
  username <- "abatzel"
  root <- "J:/"
} else {
  username <- Sys.getenv("USER")
  root <- "/home/j/"
}
# ----------------------------------------------

# ----------------------------------------------
##set set up the directories to read/export files: 
# ----------------------------------------------
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j/')
shape_dir <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/gadm36_COD_shp/")

export_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLT_data/')

data_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/")

## input files
pnlt_data_dep <- "PNLT_prepped_data.csv"
pnlt_data_case_outcomes <- "PNLT_case_outcomes_2017.csv"
dps_shapefile <-  "gadm36_COD_1.shp"

## load shapefile and data
drcShape <- shapefile(paste0(shape_dir, dps_shapefile)) # shapefile with all DPS (use a different one for health zones)
dt <- read.csv(paste0(data_dir, pnlt_data_case_outcomes))
dt <- as.data.table(dt)
# ----------------------------------------------

# ----------------------------------------------
# fortify the shapefile to get coordinates and convert it to a data table / standardize dps names for merging
# ----------------------------------------------
coordinates = as.data.table(fortify(drcShape, region='NAME_1'))
coordinates$id <- standardizeDPSnames(coordinates$id)
# this name doesn't save properly so you may have to re-copy and paste the name from the shapefile
coordinates$id <- gsub("ã???equateur", "equateur", coordinates$id)

dt$dps <- as.character(dt$dps)
dt$dps <- standardizeDPSnames(dt$dps)
#-------> look into why this is an issue ?
dt$dps <- gsub("eequateur", "equateur", dt$dps)
# ----------------------------------------------

# ---------------------------------------------- 
dt$X <- NULL
id_vars <- c("dps", "quarter", "TB_type", "data_year", "file_year", "sheet")

# add healed and treatment completed to get treatment completed
dt[is.na(healed), healed:= 0]
dt[is.na(trt_complete), trt_complete:= 0]

dt[, trt_complete:= healed + trt_complete]
dt[, healed:=NULL]

dt[, trt_comp_ratio:= trt_complete/cas_eval] # is this right for treatment completion ratio?

# melt dt
dt_long <- melt.data.table(dt, id.vars= id_vars , variable.name = "case_outcome", variable.factor = FALSE)

# combine kongo-central-est and kongo-central-ouest into kongo-central
dt_long[dps=="kongo-central-est", dps:="kongo-central"]
dt_long[dps=="kongo-central-ouest", dps:="kongo-central"]

dt_long[, value:= sum(value, na.rm=T), by=c(id_vars, 'case_outcome')]
dt_long <- unique(dt_long)

# #combine kongo-central-est and kongo-central-ouest into kongo-central
# dt_kongo <- dt[dps %in% c("kongo-central-est", "kongo-central-ouest"), lapply(.SD,sum, na.rm=T), .SDcols = colnames(dt)[3:50]]
#      # need to edit the above so that if both are NA the answer will be set to 
# dt_kongo$dps <- "kongo-central"
# dt_kongo$year <- 2018
# 
# myfxn <- function(vector){
#   return(all(is.na(vector)))
# }
# t <- try[, lapply(.SD, myfxn), .SDcols=colnames(try)]
# sdcols<- colnames(t)
# t <- t[1==FALSE,]
# 
# test <- rbindlist(list(dt_kongo, dt), fill=TRUE)
# ----------------------------------------------------------------------
# Set up to graph
graphData <- merge(coordinates, dt_long, by.x='id', by.y='dps', all=TRUE, allow.cartesian=TRUE)

  plot <- ggplot() + geom_polygon(data=graphData[case_outcome=="trt_comp_ratio",], aes(x=long, y=lat, group=group, fill=case_outcome)) + 
             coord_equal() + ##so the two shapefiles have the same proportions 
             geom_path(data=graphData, aes(x=long, y=lat, group=group), color="black", size=0.2, alpha=0.2) +
             # geom_map(map=admin_dataset, data=admin_dataset,
             #          aes(map_id=id,group=group), size=1, color="#4b2e83", alpha=0) + 
             # geom_polygon(data=admin_dataset, aes(x=long, y=lat, group=group), color="#4e0589", alpha=0) + 
             scale_fill_gradient2(low='#fee0d2', mid='#fb6a4a', high='#99000d',             # blues - (low='#9aeaea', mid='#216fff', high='#0606aa',
                                  na.value = "grey70",space = "Lab", midpoint = 1000, ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  breaks=6) + 
             #theme_void()) +  labs(title= paste0("DRC: ", indicators, " ", subpop, " by Province, ", y), fill=paste0('Doses (in ', as.integer(units),"s)"))
             theme_void()
    #          theme(plot.title = element_text(size = 18), legend.title=element_text(size=16), legend.text=element_text(size=10), plot.caption=element_text(size=12))) +  
    # labs(title= paste0("Stock-outs of ACTs at Health Facilities ", y), fill=paste0('Days per facility')) +
    # labs(caption="Source: Programme National de Lutte contre le Paludisme (PNLP)"
        
  
  print(plot)
  


# colors
cols1 = rev(brewer.pal(6, 'RdYlBu'))
cols2 = brewer.pal(6, 'RdYlGn')
border = 'grey65'

# legend limits so both countries are on same scale
limsprev = range(data$prev, na.rm=TRUE)*100
limsitn = range(data$itn, na.rm=TRUE)*100
limsantmal = range(data$antmal, na.rm=TRUE)*100
# ----------------------------------------------------------------------
ugaprev = ggplot(data[iso3=='UGA'], aes(y=y, x=x, fill=prev*100)) + 
  geom_tile() + 
  geom_path(data=shapeData[iso3=='UGA'], aes(x=long, y=lat, group=group)
            , color=border, size=.05, inherit.aes=FALSE) + 
  scale_fill_gradientn('PfPR', colors=cols1, 
                       na.value='white') + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='Uganda') + 
  theme_minimal(base_size=16) + 
  theme(plot.title=element_text(hjust=.5)) 

codprev = ggplot(data[iso3=='COD'], aes(y=y, x=x, fill=prev*100)) + 
  geom_tile() + 
  geom_path(data=shapeData[iso3=='COD'], aes(x=long, y=lat, group=group)
            , color=border, size=.05, inherit.aes=FALSE) + 
  scale_fill_gradientn('PfPR', colors=cols1, 
                       na.value='white') + 
  coord_fixed(ratio=1) + 
  scale_x_continuous('', breaks = NULL) + 
  scale_y_continuous('', breaks = NULL) + 
  labs(title='DRC') + 
  theme_minimal(base_size=16) + 
  theme(plot.title=element_text(hjust=.5), plot.margin=unit(rep(-1,4), 'cm')) 
# ----------------------------------------------
# put maps together
p1 = arrangeGrob(codprev, ugaprev, ncol=2)
p2 = arrangeGrob(coditn, ugaitn, ncol=2)
p3 = arrangeGrob(codantmal, ugaantmal, ncol=2)
# ----------------------------------------------
# Save graphs
pdf(graphFile, height=6, width=9)
grid.newpage()
grid.draw(p1)
grid.newpage()
grid.draw(p2)
grid.newpage()
grid.draw(p3)
dev.off()