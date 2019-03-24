# -----------------------------------
# David Phillips
# 
# 3/22/2019
# This makes various model diagnostics of the second half SEM
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/_common/set_up_r.r')

# load model results
load(outputFile5d)

# load nodeTable for graphing
nodeTable = fread('./impact_evaluation/visualizations/vartable_second_half.csv')

# ensure there are no extra variables introducted from nodeTable
nodeTable = nodeTable[variable %in% names(data)]
# -----------------------------------------------


# ----------------------------------------------
# Graphs

# health zone-level coefficients
p1 = ggplot(summaries[lhs=='lead_malariaDeaths_rate' & rhs=='severeMalariaTreated_rate'], 
		aes(y=est.std, ymin=ci.lower, ymax=ci.upper, x=reorder(health_zone,est.std))) + 
		geom_pointrange(alpha=.5) +
		geom_hline(yintercept=0) + 
		labs(title='Coefficient Values', subtitle='lead_malariaDeaths_rate ~ severeMalariaTreated_rate', 
			y='Standardized Coefficient', x='Health Zone (sorted by y-axis)') + 
		theme_bw() + 
		theme(axis.text.x=element_blank())

# matrix plot for a random health zone
library(GGally)
h = sample(unique(data$health_zone),1)
mortVars = c("newCasesMalariaMild_rate", "newCasesMalariaSevere_rate", "mildMalariaTreated_rate", 
	"severeMalariaTreated_rate", "ACTs_CHWs_rate", "SP_rate")
tmp = data[health_zone==h, mortVars, with=FALSE]
ggpairs(tmp)
		
# ----------------------------------------------


# -----------------------------------
# Save output
# pdf(outputFile6, height=6, width=9)
p1
p2
# dev.off()

# save a time-stamped version for reproducibility
# date_time = gsub('-|:| ', '_', Sys.time())
# outputFile6Archive = gsub('visualizations/', 'visualizations/archive/', outputFile6)
# outputFile6Archive = gsub('.pdf', paste0('_', date_time, '.pdf'), outputFile6Archive)
# file.copy(outputFile6, outputFile6Archive)
# -----------------------------------
