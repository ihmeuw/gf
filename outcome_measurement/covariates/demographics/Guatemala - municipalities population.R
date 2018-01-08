# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2017-12-13

# WorldPop data on Guatemala
# Data from the WorldPop project for Guatemala is used to obtain an estimation of the population in each municipality for 2015.

# ----------------------------------------------
# Dependencies

library(rgdal)
library(raster)
library(sp)
library(colorRamps)
library(ggmap)
library(ggplot2)

# ----------------------------------------------
# Load data

WorldPopGT <- raster("/DATA/WorldPop/GTM_ppp_v2b_2015/GTM_ppp_v2b_2015.tif")

# The municipalities shape file obtained from SEGEPLAN website is most probably outdated. I've used a shapefile in geojson format obtained from IGN (national geographic institute of Guatemala) which seems to be more up to date. This file was retrieve in mid 2017 and has been uploaded to the internet archive so it is accesible. The link is mentioned below:

# Old data from SEGEPLAN data. This only contains 334 municipalities. 
gtmMunis = readOGR("/DATA/municipios_gtm/municipios_GTM.shp", encoding = "utf-8")

# This data comes from IGN GIS system. It contains 340 Municipalities. It seems to be less outdated.
# This file can be found in the archive: https://archive.org/download/IGNCartografiaBasicaDivisionPoliticaAdministrativaMunicipios/IGN-cartografia_basica-Division%20politica%20Administrativa%20%28Municipios%29.geojson
gtmMunisIGN = readOGR("./PCE/Outcome Measurement Data/GIS/GT-IGN-cartografia_basica-Division politica Administrativa (Municipios).geojson", encoding="utf-8")

# ----------------------------------------------
# Exploring data
# A raw visual overview of the country. Spent a while tweaking the color scale in order to be able to visually recognize very low counts and high counts.
plot(log10(WorldPopGT), colNA="black", col=colorRampPalette(c("#121212", "#123620", "#108650", "#80D6C0", "#DFDF3B"),0.5)(110))

# Estimating, for instance, the population in Guatemala department. Result is 3.4 million people.
print(sum(extract(WorldPopGT, gtmMunis[gtmMunis$Cod_Dep==1,], fun = sum, na.rm = TRUE ) ))
print(sum(extract(WorldPopGT, gtmMunisIGN[gtmMunisIGN$COD_DEPT__ %in% c("01"),], fun = sum, na.rm = TRUE ) ))

# What about the population of the entire country? 15.9 million. National statistics institute of Guatemala projections estimate this number to be 16176034 for 2015. I would believe more in this data since it is based on other covariates, while INE projections are based on a raw population growth rate, which is probably estimated from official data on live births and deaths. 

tempdata = as.array(WorldPopGT)
sum(tempdata, na.rm = T)

# Mixing googlemaps and the worldpop raster 
# A snippet to explore this data with google maps images. Just change the location and zoom values:

gmap = get_map(location=c(-90.50,14.60), zoom=12, scale=1, maptype = "roadmap", source="google", color="bw")
gmapbbox = attr(gmap, "bb")
subpop <- crop(WorldPopGT, extent(gmapbbox$ll.lon, gmapbbox$ur.lon, gmapbbox$ll.lat, gmapbbox$ur.lat))
subpopDF = data.frame(rasterToPoints(subpop))
colnames(subpopDF) = c("lon", "lat", "z")
bm = ggmap(gmap)
bm = bm + geom_raster(data = subpopDF, aes(y=lat, x=lon, fill=z), alpha=0.6) + 
    scale_fill_gradientn(colours=rev(c("#FF1202FF", "#FDAD22FF", "#FDFD22FF", "#00000077")), values = c(0,0.1,0.5,1) ) + coord_cartesian()
plot(bm)

# ----------------------------------------------
# Muncipalities population extraction

# Now let's estimate the population by municipality, which is what brought me here in the first place:

# This takes a while, so I'm ignoring the old and outdated SEGEPLAN shapefile data.
# munisPop = extract(WorldPopGT, gtmMunis[gtmMunis$Codigo > 0 & gtmMunis$Cod_Dep<23,], fun = sum, na.rm = TRUE )
munisPopIGN = extract(WorldPopGT, gtmMunisIGN[!(gtmMunisIGN$COD_DEPT__ %in% c(NA)) & !(gtmMunisIGN$COD_MUNI__ == 0),], fun = sum, na.rm = TRUE )

# Putting the data in the municipalities database and exporting to a CSV:
#gtmMunis[gtmMunis$Codigo > 0 & gtmMunis$Cod_Dep<23, "Poblacion"] = munisPop
gtmMunisIGN[!(gtmMunisIGN$COD_DEPT__ %in% c(NA)) & !(gtmMunisIGN$COD_MUNI__ == 0), "Poblacion"] = munisPopIGN
write.csv(gtmMunisIGN[, names(gtmMunisIGN)], "./PCE/Outcome Measurement Data/Covariates/Demographics/Guatemala_Municipios_IGN2017_worldpop2015.csv")


# ----------------------------------------------
# Births data 
nacs14 = read_sav("C:/DATOS/GTVitales y Censo/Nacimientos 2014.sav")
dtnacs14 = data.table(nacs14)
WPBirthsGT <- raster("/DATOS/WorldPop/Guatemala 1km births/GTM_births_pp_v2_2015.tif")

munisNacs = extract(WPBirthsGT, gtmMunisIGN[!(gtmMunisIGN$COD_DEPT__ %in% c(NA)) & !(gtmMunisIGN$COD_MUNI__ == 0),], fun = sum, na.rm = TRUE )
dtmunisNacs = data.table(NacsWP = munisNacs, Codigo = gtmMunisIGN[!(gtmMunisIGN$COD_DEPT__ %in% c(NA)) & !(gtmMunisIGN$COD_MUNI__ == 0), ]$COD_MUNI__)
dtmunisNacs[,Codigo:=as.numeric(as.character(Codigo))]

# From worldpop data:
#> sum(munisNacs)
#[1] 429374.1
#> nrow(nacs14)
#[1] 386195
#> 429374/386195
#[1] 1.111806
nacs = merge(dtnacs14[,.(nacs=.N),by=(Muprem+Deprem*100)], dtmunisNacs, by.x="Muprem", by.y = "Codigo")
summary(nacs[,nacs-NacsWP.V1])
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-4792.18  -209.33   -56.60  -119.67    48.09  4065.58 
hist(nacs[,nacs-NacsWP.V1])

# munisPob = munisPopIGN
nacs = merge(munisPob[,.(Muprem = COD_MUNI__, Poblacion)], nacs, by="Muprem")
nacs = merge(dtnacs14[,.(nacsI = sum(PuebloPM %in% c(1,2,3)) / sum(PuebloPM>0) ),by=(Muprem+Deprem*100)], nacs, by="Muprem")

