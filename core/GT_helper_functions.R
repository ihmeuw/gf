# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2017-11-14
# Explore TB outcome data from MOH notifications databases.


# ----Dependencies------------------------------
library(stringdist)
library(stringr)

# ----Read some data----------------------------
munisGT = read.csv("PCE/Outcome Measurement Data/Covariates/Demographics/Guatemala_Municipios_IGN2017_worldpop2015.csv", encoding = "UTF-8")

# ----getMuniCodeByName-------------------------
# This function converts a string to a number. It tries to find a municipality 
# by name and returns its numerical code.
deptosGT             = unique(munisGT[, c("DEPTO__", "COD_DEPT__") ])
vocalesTildes        = c("á"="a", "é"="e", "í"="i", "ó"= "o", "ú"="u")
deptosGT$lookupDepto = str_replace_all(str_to_lower(deptosGT$DEPTO__), vocalesTildes)
munisGT$lookupMuni  = str_replace_all(str_to_lower(munisGT$NOMBRE__), vocalesTildes)

getMuniCodeByName <- function (nombreMuni, nombreDepto, field = "COD_MUNI__") {
    if (is.na(nombreMuni)) {
        warning(paste("Found an NA in municipality input nombreDepto was:", nombreDepto))
        muni = NA
    }
    else {
        nombreMuni  = str_replace_all(str_to_lower(nombreMuni), vocalesTildes)
        nombreDepto = str_replace_all( str_to_lower(nombreDepto), vocalesTildes)
        
        depto       = deptosGT[which.min(stringdist(nombreDepto, deptosGT$lookupDepto, method = "cosine")),]
        deptoMunis  = munisGT[munisGT$COD_DEPT__ == depto$COD_DEPT__,]
        muni        = deptoMunis[which.min(stringdist(nombreMuni, deptoMunis$lookupMuni, method = "cosine")), field]
    }
    muni
}

getDeptoCodeByName <- function(nombreDepto) {
    if (is.na(nombreDepto)) {
        warning(paste("Found an NA in municipality input nombreDepto was:", nombreDepto))
        codigo = NA
    }
    else {
        nombreDepto = str_replace_all( str_to_lower(nombreDepto), vocalesTildes)
        depto       = deptosGT[which.min(stringdist(nombreDepto, deptosGT$lookupDepto, method = "cosine")),]
        codigo      = depto$COD_DEPT__
    }
    codigo
}