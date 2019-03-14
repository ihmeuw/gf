# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# This runs the SEM dose-response model
# ------------------------------------------------

source('./impact_evaluation/_common/set_up_r.r')


# ---------------------------
# Load data
set.seed(1)
load(outputFile5c)
# ---------------------------


# -------------------------
# Run series of unrelated linear models

# linkage 1 regressions


# linkage 2 regressions
lmFit4 = lm(newCasesMalariaMild_rate ~ ITN_rate + lag_mildMalariaTreated_rate + health_zone + date, data)
lmFit5 = lm(newCasesMalariaSevere_rate ~ ITN_rate + lag_severeMalariaTreated_rate + health_zone + date, data)
lmFit6 = lm(malariaDeaths_rate ~ newCasesMalariaMild_rate + newCasesMalariaSevere_rate + lag_mildMalariaTreated_rate + lag_severeMalariaTreated_rate + health_zone + date, data)

tmp = summary(lmFit4)$coefficients
tmp[!grepl('health_zone',rownames(tmp)),]
tmp = summary(lmFit5)$coefficients
tmp[!grepl('health_zone',rownames(tmp)),]
tmp = summary(lmFit6)$coefficients
tmp[!grepl('health_zone',rownames(tmp)),]
# -------------------------


# ----------------------------------------------
# Define model object
# DECISIONS
source('./impact_evaluation/models/drc_malaria_impact1.r')

# swap in health zone dummies where health_zone is specified (for convenience)
# model = gsub('health_zone', paste(unique(data$health_zone)[-1],collapse='+'), model)
# ----------------------------------------------


# --------------------------------------------------------------
# Run model
if ('semFit' %in% ls()) rm('semFit')
# semFit = sem(model, data)
semFits = lapply(unique(data$health_zone), function(h) sem(model, data[health_zone==h]))
for(i in seq(length(semFits))) { 
	tmp = data.table(standardizedSolution(semFits[[i]]))
	tmp[, health_zone:=unique(data$health_zone)[i]]
	if (i==1) summaries = copy(tmp)
	if (i>1) summaries = rbind(summaries, copy(tmp))
}
means = summaries[,.(est.std=mean(est.std),se=mean(se),ci.lower=mean(ci.lower),ci.upper=mean(ci.upper)), 
	by=c('lhs','op','rhs')]
means
# --------------------------------------------------------------

nodeTable = fread('C:/local/gf/impact_evaluation/visualizations/vartable_second_half.csv')
source('./impact_evaluation/visualizations/graphLavaan.r')
semGraph(parTable=means, nodeTable=nodeTable, 
	scaling_factors=NA, standardized=TRUE, 
	lineWidth=1.5, curved=0, tapered=FALSE, 
	boxWidth=2, boxHeight=.5)


# ------------------------------------------------------------------
# Save model output and clean up

# save
save(list=c('data','model','semFits','summaries','means','scaling_factors'), file=outputFile5d)

# save a time-stamped version for reproducibility
date_time = gsub('-|:| ', '_', Sys.time())
outputFile5dArchive = gsub('prepped_data/', 'prepped_data/model_runs/', outputFile5d)
outputFile5dArchive = gsub('.rdata', paste0('_', date_time, '.rdata'), outputFile5dArchive)
save(list=c('data','model','semFit','scaling_factors'), file=outputFile5dArchive)

# clean up in case jags saved some output
if(dir.exists('./lavExport/')) unlink('./lavExport', recursive=TRUE)
# ------------------------------------------------------------------
