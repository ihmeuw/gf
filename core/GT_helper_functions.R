# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2017-11-14
# Helper functions for Guatemala data analysis
# This requires sourcing first GT_load_data.R 
#
#   source("gf/core/GT_load_data.R")
#
# ----Dependencies------------------------------
library(stringdist)
library(stringr)
library(data.table)

# ----getMuniCodeByName-------------------------
# This function converts a string to a number. It tries to find a municipality 
# by name and returns its numerical code.
deptosGT             = unique(munisGT[, c("DEPTO__", "COD_DEPT__") ])
vocalesTildes        = c("á"="a", "é"="e", "í"="i", "ó"= "o", "ú"="u")
deptosGT$lookupDepto = str_replace_all(str_to_lower(deptosGT$DEPTO__), vocalesTildes)
munisGT$lookupMuni   = str_replace_all(str_to_lower(munisGT$NOMBRE__), vocalesTildes)

deptoNameToCodeCache  = data.frame(original_name = character(), clean_name = character(), code  = integer(), stringsAsFactors = FALSE)
muniNameToCodeCache  = data.frame(original_name = character(), clean_name = character(), depto_orig_name = character(), depto_code = integer(), code  = integer(), stringsAsFactors = FALSE)



getMuniCodeByName <- function (nombreMuni_, nombreDepto_, field = "COD_MUNI__") {
    if (is.na(nombreMuni_)) {
        warning(paste("Found an NA in municipality input nombreDepto was:", nombreDepto_))
        muni = 0
    }
    else {
        nombreMuni  = str_replace_all(str_to_lower(nombreMuni_), vocalesTildes)
        nombreDepto = str_replace_all( str_to_lower(nombreDepto_), vocalesTildes)
        
        deptoCache = deptoNameToCodeCache[deptoNameToCodeCache$clean_name == nombreDepto, "code"]
        
        if (length(deptoCache)==0) {
            depto   = deptosGT[which.min(stringdist(nombreDepto, deptosGT$lookupDepto, method = "cosine")), "COD_DEPT__"]
            deptoNameToCodeCache[nrow(deptoNameToCodeCache)+1,] <<- list(original_name = nombreDepto_, clean_name = nombreDepto, code  = depto)
        }
        else {
            depto   = deptoCache[1]
        }
        
        muniCache = muniNameToCodeCache[muniNameToCodeCache$clean_name == nombreMuni & muniNameToCodeCache$depto_code == depto, "code"]
        
        if (length(muniCache) == 0) {
            deptoMunis  = munisGT[munisGT$COD_DEPT__ == depto,]
            muni        = deptoMunis[which.min(stringdist(nombreMuni, deptoMunis$lookupMuni, method = "cosine")), field]
            muniNameToCodeCache[nrow(muniNameToCodeCache)+1,] <<- list(original_name = nombreMuni_, clean_name = nombreMuni, depto_orig_name = nombreDepto_, code_depto  = depto, code = muni)
        }
        else {
            muni = muniCache
        }
        
    }
    muni
}

getDeptoCodeByName <- function (nombreDepto_) {
    if (is.na(nombreDepto_)) {
        warning(paste("Found an NA in department input"))
        depto = 0
    }
    else {
        nombreDepto = str_replace_all( str_to_lower(nombreDepto_), vocalesTildes)
        
        deptoCache = deptoNameToCodeCache[deptoNameToCodeCache$clean_name == nombreDepto, "code"]
        
        if (length(deptoCache)==0) {
            depto       = deptosGT[which.min(stringdist(nombreDepto, deptosGT$lookupDepto, method = "cosine")),]
            depto       = depto$COD_DEPT__
            deptoNameToCodeCache[nrow(deptoNameToCodeCache)+1,] <<- list(original_name = nombreDepto_, clean_name = nombreDepto, code  = depto)
        }
        else {
            depto = deptoCache
        }
    }
    depto
}

# ----------Population-------------------
# After doing some math with the exponential growth equation, I have got these:
# P_0 = (A-B)/(d*A) ^ ( Y_A / (Y_A-1) )
# k = log((A - B) / P_0) / d
# Where P_0 and k are the population at Year 0 and the coefficient of exponential growth, respectively.
# A is the population at year Y_A and B is the population at year Y_B. We assume that Y_A > Y_B

dt.munisGT[, P_10_12 := (Poblacion2010^(1/2010) / Poblacion2012^(1/2012))^(2010*2012/(2012-2010))] 
dt.munisGT[, P_12_15 := (Poblacion2012^(1/2012) / Poblacion2015^(1/2015))^(2015*2012/(2015-2012))]
dt.munisGT[, k_10_12 := log(Poblacion2012/P_10_12)/2012] 
dt.munisGT[, k_12_15 := log(Poblacion2015/P_12_15)/2015]
setkey(dt.munisGT, COD_MUNI__)
GTMuniPopulation <- function (code, year) {
    parameters = dt.munisGT[J(code), .(P_10_12, P_12_15, k_10_12, k_12_15)]
    ifelse(year>2012, 
           parameters$P_12_15 * exp(parameters$k_12_15 * year),
           parameters$P_10_12 * exp(parameters$k_10_12 * year))
}
# Example usage:
# GTMuniPopulation(c(101, 201, 301, 401, 402), c(2013, 2013, 2013, 2013, 2013))
# [1] 1016192.75   23072.88   47429.26  124803.77   24614.68

ApplyQuintiles <- function(x) {
    cut(x, breaks=c(quantile(x, probs = seq(0, 1, by = 0.20))), 
        labels=c("0-20","20-40","40-60","60-80","80-100"))
}

# ---------Gt municipality map visualizations--------------
# Function to generate a Guatemala municipalities map visualization. 
# Data should be indexed by a "municode" column containing municipalities codes.
# The variable to plot should be named "values"
gtmap_muni <- function(data) {
    gtmMunisDataCopy = cbind(gtmMunisIGN@data)
    gtmMunisIGN@data$id = rownames(gtmMunisIGN@data)
    gtmMunisIGN@data = merge(gtmMunisIGN@data, data, by.x = "COD_MUNI__", by.y="municode", all.x=TRUE, sort=FALSE)
    gtmMunisIGN.map.df = fortify(gtmMunisIGN)
    gtmDeptosIGN.map.df = fortify(gtmDeptosIGN)
    
    plot = ggplot(data=gtmMunisIGN@data, aes(fill=values)) + geom_map(aes(map_id=id), colour = "#44554444", map = gtmMunisIGN.map.df) + expand_limits(x = gtmMunisIGN.map.df$long, y = gtmMunisIGN.map.df$lat) + coord_quickmap() + geom_polygon(data = gtmDeptosIGN.map.df, aes(long, lat, group=group), fill="#00000000", color="#FFFFFF66", size=1)
    
    gtmMunisIGN@data = gtmMunisDataCopy
    plot
}
