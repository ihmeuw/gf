# ------------------------------------------------
# David Phillips
# 
# 6/4
# find a few examples of health zones that are 
# very close to the national average for small-scale testing
# ------------------------------------------------

source('./impact_evaluation/drc/set_up_r.r')

# ------------------------------------------------
# Load data

# use the full file
outputFile5a_big = gsub('.rdata','_all_semFits.rdata',outputFile5a)
load(outputFile5a_big)
# ------------------------------------------------


# ------------------------------------------------
# Test all individual sem fits for distance from the mean

# merge means to summaries
byVars = c('lhs','op','rhs')
summaries = merge(summaries, means, by=byVars, suffixes=c('','_national'))

# compute difference and ratio
summaries[, diff:=abs(est-est_national)]

# summarize 
hz_distance = summaries[, .(mean_diff=mean(diff, na.rm=T), median_diff=median(diff, na.rm=T)), by='health_zone']
# ------------------------------------------------


# ------------------------------------------------
# Find closest hzs to the average

# rank by mean to avoid vary large differences
hz_distance[, rank:=rank(mean_diff)]

# display
hz_distance[rank<=10][order(rank)]
# ------------------------------------------------


# ------------------------------------------------
# Examine the top ranked

top = hz_distance[rank<6]$health_zone

options(scipen=999, digits=1)
tmp = summaries[health_zone%in%top & op=='~', c(byVars, 'health_zone', 'est', 'est_national'), with=F]
dcast(tmp, lhs+op+rhs+est_national~health_zone, value.var='est')
# ------------------------------------------------
