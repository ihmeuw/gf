# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: 2d. Use multiple-imputation to fill in missing values for Senegal TB
# DATE: September 25, 2019
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

source('./impact_evaluation/sen/set_up_r.r')

# load data
DTmdr <- readRDS(outputFile2c) # MDR-TB outcome data

#missmap(DTmdr)

# convert resultat to factor and numeric
DTmdr$resultat <- as.factor(DTmdr$resultat)
DTmdr$resultat <- as.numeric(DTmdr$resultat)

# perform inputations
m.out <- amelia(DTmdr, m=5, idvars = c('id', 'structure', 'region', 'sexe', 'date_diag', 'date_trait', 'annee'), noms=c('regime'), ords = c('resultat'))

# save imputations in long dataset
write.amelia(m.out, file.stem = mdroutputFile2e, format = "csv", separate = FALSE, orig.data = FALSE)

# read-in long dataset
DTm <- fread(paste0(mdroutputFile2e, '.csv'))

# aggregate data across the different imputations
DTmi <- DTm[, lapply(.SD, mean, na.rm=TRUE), 
            by=c('id', 'structure', 'region', 'date_diag', 'date_trait', 'annee', 'age', 'sexe'), 
            .SDcols=c('resultat')]

# data clean up
DTmi$resultat <- round(DTmi$resultat)

# extract datable from each of the imputation rounds
#DTm <- m.out$imputations[[1]]

# add factor level back to the dataset
DTmi$resultat <- as.factor(DTmi$resultat)
levels(DTmi$resultat) <- c("Abandon", "Deces", "Echec", "Gueris")

# save
saveRDS(DTmi, outputFile2e)

# save a time-stamped version for reproducibility
archive(outputFile2e)
