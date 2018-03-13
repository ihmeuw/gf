# ----------------------------------------------
# Guillermo Ambrosio (CIEAR)
#
# 2017-11-14
# Helper functions for Guatemala data analysis

dataFilePrefix = "./PCE/"

# ----Dependencies------------------------------
library(stringdist)
library(stringr)

# ----Read some data----------------------------
if (!exists("munisGT")) {
  munisGT = read.csv(paste0(dataFilePrefix, "Covariates and Other Data/Demographics/Guatemala_Municipios_IGN2017_worldpop2015.csv"), encoding = "UTF-8")
  dt.munisGT = data.table(munisGT)
}
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
        muni = NA
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
        depto = NA
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
