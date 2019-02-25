#####################################################
# Extramuros analysis for geo prioritization paper
# J Ross
# February 2019
####################################################

library(data.table)
library(ggplot2)

indir <- ("H:/PCE analysis/GTM TB prioritization/data/")
outdir <- ("H:\PCE analysis\GTM TB prioritization")

dt <- fread(paste0(indir, "Extramuros2017.csv"))
