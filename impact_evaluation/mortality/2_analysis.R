# Drivers of morality analysis for Senegal

library(data.table)
library(ggplot2)
library(GGally)
library(boot)
library(stats)
library(lavaan)

# load 
data <- readRDS("J:/Project/Evaluation/GF/impact_evaluation/mortality/prepped_data/tb_pce_data.RDS")

# plot correlations
ggpairs(data[location_name=="Senegal",c('Deaths', 'Incidence', 'mi_ratio'), with=F])

# plot pairwise correlations for each country
# ggpairs(data[location_name=="Democratic Republic of the Congo",c('Deaths', 'Incidence', 'mi_ratio'), with=F])
# ggpairsafs = lapply(lmFits, anova(data[location_name=="Guatemala",c('Deaths', 'Incidence', 'mi_ratio'), with=F])
# ggpairs(data[location_name=="Uganda",c('Deaths', 'Incidence', 'mi_ratio'), with=F])


# transform variables
data[,log_incidence_rate:=log(Incidence)]
data[,logit_mi_ratio:=logit(mi_ratio)]

# graph transformed data
ggpairs(data[location_name=="Senegal",c('Deaths', 'log_incidence_rate', 'logit_mi_ratio'), with=F])

# ----------------------------------------------------
# Get glm estimate
lmFits = lm(Deaths ~ log_incidence_rate + logit_mi_ratio, data = data)
afs = anova(lmFits)
data.table(variable=rownames(afs), explained_variance=afs[['Sum Sq']]/sum(afs[['Sum Sq']]))

# visualizations
