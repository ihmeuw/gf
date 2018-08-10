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
##set set up the directories to read/export files: 
# ----------------------------------------------
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j/')
shape_dir <- paste0(j, "Project/Evaluation/GF/outcome_measurement/cod/drc_shapefiles/gadm36_COD_shp/")

export_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/visualizations/PNLT_data/')

data_dir <- paste0(j, "/Project/Evaluation/GF/outcome_measurement/cod/prepped_data/PNLT/")

## input files
pnlt_data <- "PNLT_prepped_data.csv"
dps_shapefile <-  "gadm36_COD_1.shp"

## load shapefile and data
drcShape <- shapefile(paste0(shape_dir, dps_shapefile)) # shapefile with all DPS (use a different one for health zones)
dt <- read.csv(paste0(data_dir, pnlt_data))
dt <- as.data.table(dt)
# ----------------------------------------------

# ----------------------------------------------
## change dps names so that matching on dps name works correctly
# ----------------------------------------------
# ********NOTE: these don't save properly because of the weird characters, so you have to re-copy and paste the names
coordinates = as.data.table(fortify(drcShape, region='NAME_1'))
coordinates$id <- gsub("Bas-Uélé", "Bas-Uele", coordinates$id)
coordinates$id <- gsub("�???quateur", "Equateur", coordinates$id) #re-copy and paste this one
coordinates$id <- gsub("Haut-Uélé", "Haut-Uele", coordinates$id)
coordinates$id <- gsub("Kasaï", "Kasai", coordinates$id)
coordinates$id <- gsub("Kasaï-Central", "Kasai Central", coordinates$id)
coordinates$id <- gsub("Kasaï-Oriental", "Kasai Oriental", coordinates$id)
coordinates$id <- gsub("Maï-Ndombe", "Mai-Ndombe", coordinates$id)

coordinates$id <- tolower(coordinates$id)
# ----------------------------------------------  


# ----------------------------------------------  
id_vars <- c("dps")
dt$dps <- as.character(dt$dps)

#combine kongo-central-est and kongo-central-ouest into kongo-central
dt_kongo <- dt[dps %in% c("kongo-central-est", "kongo-central-ouest"), lapply(.SD,sum, na.rm=T), .SDcols = colnames(dt)[3:50]]
     # need to edit the above so that if both are NA the answer will be set to 
dt_kongo$dps <- "kongo-central"
dt_kongo$year <- 2018

myfxn <- function(vector){
  return(all(is.na(vector)))
}
t <- try[, lapply(.SD, myfxn), .SDcols=colnames(try)]
sdcols<- colnames(t)
t <- t[1==FALSE,]

test <- rbindlist(list(dt_kongo, dt), fill=TRUE)
# ----------------------------------------------------------------------
# Set up to graph

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