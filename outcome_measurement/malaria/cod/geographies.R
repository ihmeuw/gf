
geographies <- unique(dt_wide[, c("province", "dps", "health_zone", "year")])
geographies$dps <- tolower(geographies$dps)

orderBy <- c("province", "health_zone", "dps")
setorderv(geographies, orderBy)

geographies[province=="SK", province := "SUD KIVU"]
geographies[province=="38", province := "BDD"]
geographies[province=="4481", province := "BDD"]
geographies[province=="NordKivu", province := "Nord Kivu"]

geographies[dps=="mai - ndo", dps := "mai-ndombe"]
geographies[dps=="sud uban", dps := "sud ubangi"]
geographies[dps=="nord uban", dps := "nord ubangi"]
geographies[dps=="mbuji-may", dps := "mbuji-mayi"]
geographies[dps=="mbuji may", dps := "mbuji-mayi"]
geographies[dps=="nordkivu", dps := "nord kivu"]
geographies[dps=="haut lomami", dps := "haut-lomami"]
geographies[dps=="tanganika", dps := "tanganyika"]
geographies[dps=="kkt", dps := "kikwit"]
geographies[dps=="haut katanga", dps := "haut-katanga"]

geographies[province=="Kinshasa" & is.na(dps), dps := "nsele"]


geographies <- unique(geographies)

geographies_wide <- dcast(geographies, province + dps + health_zone ~ year, value.var = "year")
geographies_wide <- as.data.table(geographies_wide)
setorderv(geographies_wide, c("province", "health_zone"))


write.csv(geographies_wide, paste0(dir_prepped, "geographies.csv"))


