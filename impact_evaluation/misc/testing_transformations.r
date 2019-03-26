# testing out alternative transformations
source('./impact_evaluation/_common/set_up_r.r')

# -----------------------------------------------------------------
# Load/prep data

# load
data = readRDS(outputFile3b)

# replace data with corrected data
adjVars = names(data)[grepl('_adj',names(data))]
for(v in adjVars) data[, (gsub('_adj','',v)):=get(v)]
data = data[, -adjVars, with=FALSE]

# drop unnecessary variables
dropVars = c('act_coverage','incidence','prevalence','mortality','itn_coverage','year')
data = data[,-dropVars, with=FALSE]

# convert date to numeric
data[, date:=as.numeric(year(date)+((month(date)/12)-1))]
# -----------------------------------------------------------------


# -----------------------------------------------------------------------
# Data transformations and other fixes for Heywood cases

# apply limits
data[ITN>1000, ITN:=NA]
data[SSCACT>50000, SSCACT:=NA]
data[!is.finite(SP_rate), SP_rate:=NA]
data[!is.finite(RDT_rate), RDT_rate:=NA]
data[ACTs_CHWs_rate>1000, ACTs_CHWs_rate:=NA]
data[mildMalariaTreated_rate>2, mildMalariaTreated_rate:=NA]
data[severeMalariaTreated_rate>2.5, severeMalariaTreated_rate:=NA]
data[newCasesMalariaMild_rate>100000, newCasesMalariaMild_rate:=NA]
data[newCasesMalariaSevere_rate>100000, newCasesMalariaSevere_rate:=NA]
data[malariaDeaths_rate>500, malariaDeaths_rate:=NA]

# last-minute prep that shouldn't be necessary after bugs are fixed

# na omit
data = na.omit(data)

# remake ITN_rate now that it can be cumulative
data = data[order(health_zone, date)]
data[,ITN_cumul:=cumsum(ITN_rate*population), by='health_zone']
data[, ITN_rate_cumul:=ITN_cumul/population]
# -----------------------------------------------------------------------


# -----------------------------------------------------------------------

# log-transform
# logVars = c('ITN','RDT','SP','SSCACT','mildMalariaTreated','severeMalariaTreated',
	# 'RDT_rate','SP_rate','ACTs_CHWs_rate','ITN_rate','ITN_rate_cumul',
	# 'newCasesMalariaMild_rate','newCasesMalariaSevere_rate','malariaDeaths_rate')
	
data[, log:=log(malariaDeaths_rate)]
data[!is.finite(log), log:=quantile(data[is.finite(log)]$log,.01,na.rm=T)]

# inverse hyperbolic sine
ihs <- function(x) log(x + sqrt(x ^ 2 + 1))
data[, ihs:=ihs(malariaDeaths_rate)]

p1 = ggplot(data, aes(x=log)) + geom_histogram()
p2 = ggplot(data, aes(x=ihs)) + geom_histogram()
grid.arrange(p1, p2)
# -----------------------------------------------------------------------
