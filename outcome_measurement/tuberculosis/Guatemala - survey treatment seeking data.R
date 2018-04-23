# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2018-04-18
# Treatment seeking data from ENSMI and ENCOVI.


# ----------------------------------------------
# Dependencies:
library(data.table)
library(ggplot2)
library(ggmap)
library(haven)
library(sp)

# Requirements:
source("./PCE/gf/core/GT_helper_functions.R")

# Load data
ensmi_children.dt = data.table(read_stata("PCE/Outcome Measurement Data/MULTI/ENSMI/GTM_DHS6_2014_2015_CH_Y2017M03D01.DTA"))
ensmi_gpdata = read_stata("PCE/Outcome Measurement Data/MULTI/ENSMI/GTM_DHS6_2014_2015_GPS_GUGE71FL_Y2017M03D28.DTA")

# Exploring ensmi GPS data
plot = ggplot(data=gtmMunisIGN@data)  + geom_polygon(data = gtmMunisIGN, aes(x=long, y = lat, group = group), fill = "#CCDDCC33", color = "#55665577") + expand_limits(x = gtmMunisIGN$long, y = gtmMunisIGN$lat) + coord_quickmap()
# Overlay the departments
plot = plot + geom_polygon(data = gtmDeptosIGN.map.df, aes(long, lat, group=group), fill="#00000000", color="#77776666", size=1) + theme_void() + geom_point(data=ensmi_gpdata[ensmi_gpdata$longnum != 0, c("latnum", "longnum")], aes(x=longnum, y=latnum), colour = "#FF6633BB", size=3)
plot
# Explore ensmi GPS data with ggmap
geocode("guatemala")
gtmap = get_map("guatemala", zoom = 7)
ggmap(gtmap) + geom_point(aes(x = longnum, y = latnum),
                                data = ensmi_gpdata)

# Explore ensmi children data
# Prepare cough and fever vars and treatment seeking
sapply(colnames(ensmi_children.dt)[which(str_sub(colnames(ensmi_children.dt), 1, 2) == "h3")], function (x) attr(ensmi_children.dt[[x]], "label"))
vars_cough_txseek = paste0("h32", letters[1:24])
ensmi_children.dt$cough_txseek = apply(ensmi_children.dt[, c("h32a", "h32b")] == 1, 1, any)
clusters_cough = ensmi_children.dt[, .(cough = sum(h31 == 2, na.rm=T), seektx = sum(cough_txseek & h31==2, na.rm=T)), by = v001]
setkey(clusters_cough, "v001")

clusters = SpatialPoints(ensmi_gpdata[ensmi_gpdata$longnum != 0, c("longnum", "latnum", "dhsclust")])
wgs84CRS = CRS("+init=epsg:4326")
proj4string(clusters) <- wgs84CRS
clustermunis = over(clusters, spTransform(gtmMunisIGN[,c("COD_MUNI__", "NOMBRE__")], CRSobj = wgs84CRS))
clustermunis = cbind(ensmi_gpdata[ensmi_gpdata$longnum != 0, c("dhsclust")], clustermunis)
setnames(clustermunis, c("cluster", "municode", "name"))
clusters_cough = merge(clusters_cough, clustermunis, by.x = "v001", by.y = "cluster")
# Proportion of treatment seeking by municipality for kids < 6 years old with cough or fever.
plot = gtmap_muni(clusters_cough[, .(values = 100*sum(seektx, na.rm=T)/sum(cough, na.rm=T)),by=municode])
plot + theme_void() + labs(fill= "Rate", title="Percentage of treatment seeking in case of cough/fever \nfor children < 6 years old.", subtitle="Source: ENSMI 2014-15") # + geom_point(data = ensmi_children.dt[,sum(v005),by=v001], aes(x=longnum, y = latnum))
ggsave(paste0(dataPath, "Graficas/Gt_ENSMI15_tx_seeking_cough_children.png"), height=8, width=8)

summary(clusters_cough[, .(values = 100*sum(seektx, na.rm=T)/sum(cough, na.rm=T)),by=municode]$values)

# Testing another approach: 
test_caseid = ensmi_children.dt[, .(cough = uniqueN(caseid[h31 == 2 & !is.na(h31)]), 
                                    seektx = uniqueN(caseid[cough_txseek & h31==2  & !is.na(h31) & !is.na(cough_txseek)])), by = v001]
test_caseid_m = merge(test_caseid, clustermunis, by.x = "v001", by.y = "cluster")
test_caseid = test_caseid_m[,.(values= sum(100*seektx, na.rm=T)/sum(cough, na.rm=T)),by=municode]
plot = gtmap_muni(test_caseid)
plot + theme_void() + labs(fill= "Rate", title="Percentage of treatment seeking in case of cough/fever \nfor women with children < 6 years old.", subtitle="Source: ENSMI 2014-15")
ggsave(paste0(dataPath, "Graficas/Gt_ENSMI15_tx_seeking_cough_women.png"), height=8, width=8)

todo = ensmi_children.dt[, .(cough_ch = sum(h31 == 2, na.rm=T), seektx_ch = sum(cough_txseek & h31==2, na.rm=T),
                      cough_wm = uniqueN(caseid[h31 == 2 & !is.na(h31)]), 
                      seektx_wm = uniqueN(caseid[cough_txseek & h31==2  & !is.na(h31) & !is.na(cough_txseek)])), 
                      by = v001]
todo = merge(todo, clustermunis, by.x = "v001", by.y = "cluster")
todo = todo[,.( cough_ch = sum(cough_ch), seektx_ch = sum(seektx_ch), 
         cough_wm = sum(cough_wm), seektx_wm = sum(seektx_wm)  ), by=municode]
write.csv(todo, paste0(dataPath, "Outcome Measurement Data/MULTI/ENSMI/GT - cough and fever tx seeking.csv"))
