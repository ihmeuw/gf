# Francisco Rios Casas
# shapefile creation
# Extracting administrative data

# set up
library(sp)
library(raster)
library(sf)
library(ggplot2)
library(data.table)

# load RSD

#sen0 <- readRDS("C:\\Users\\frc2\\Documents\\Data\\gadm36_SEN_0_sp.rds")
#sen1 <- readRDS("C:\\Users\\frc2\\Documents\\Data\\gadm36_SEN_1_sp.rds")
#sen2 <- readRDS("C:\\Users\\frc2\\Documents\\Data\\gadm36_SEN_2_sp.rds")

# load shapefiles
shp0 <- st_read("C:\\Users\\frc2\\Documents\\data\\mapping\\shp\\gadm36_SEN_0.shp")
shp1 <- st_read("C:\\Users\\frc2\\Documents\\data\\mapping\\shp\\gadm36_SEN_1.shp")
shp2 <- st_read("C:\\Users\\frc2\\Documents\\data\\mapping\\shp\\gadm36_SEN_2.shp")
shp3 <- st_read("C:\\Users\\frc2\\Documents\\data\\mapping\\shp\\gadm36_SEN_3.shp")
shp4 <- st_read("C:\\Users\\frc2\\Documents\\data\\mapping\\shp\\gadm36_SEN_4.shp")

# to plot
# ggplot() + geom_sf(data=shp1)

# extract Region names for admin1
regions <- as.vector(levels(shp1$NAME_1))

# extract Department names for admin2
departments <- as.vector(levels(shp2$NAME_2))

# extract Arrondissement for admin3
arrondissements <- as.vector(levels(shp3$NAME_3))

# extract Communes from admin4
communes <- as.vector(levels(shp4$NAME_4))

# create datatable with region, department, arrondissements, communes
st_geometry(shp4) <- NULL
guide <- shp4[,c("NAME_1","NAME_2","NAME_3", "NAME_4")]

# set names of columns
setnames(guide, 
         c("NAME_1", "NAME_2", "NAME_3", "NAME_4"),
         c("Region", "Department", "Arrondissement", "Commune"))

# save as a csv to preserve encoding
write.csv(guide, "senegal_admin_regions.csv")
