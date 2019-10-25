# ----------------------------------------------------------
# AUTHOR: Francisco Rios Casas
# PURPOSE: 2d. Use multiple-imputation to fill in missing values for Senegal TB
# DATE: September 25, 2019
# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------------------

source('./impact_evaluation/sen/set_up_r.r')

# load data
DTmdr <- readRDS(outputFile2c) # MDR-TB outcome data

missmap(DTmdr)

# convert resultat to factor and numeric
DTmdr$resultat <- as.factor(DTmdr$resultat)
DTmdr$resultat <- as.numeric(DTmdr$resultat)

# perform inputations
m.out <- amelia(DTmdr, m=1, idvars = c('id', 'structure', 'region', 'sexe', 'date_diag', 'date_trait', 'annee'), noms=c('regime'), ords = c('resultat'))

# extract datable from each of the imputation rounds
DTm <- m.out$imputations[[1]]

# add factor level back to the dataset
DTm$resultat <- as.factor(DTm$resultat)
levels(DTm$resultat) <- c("Abandon", "Deces", "Echec", "Gueris")

# save
saveRDS(DTm, outputFile2e)

# save a time-stamped version for reproducibility
archive(outputFile2e)