# --------------------------------------------------
# David Phillips
#
# 6/12/2019
# Make basic graphs of cost per ITN/ACT/RDT shipped over time as a simple fraction
# The current working directory should be the root of this repo
# --------------------------------------------------


# --------------------------------------------------
# Set up R
source('./impact_evaluation/drc/set_up_r.r')
library(RColorBrewer)
outFile = paste0(ieDir, '../visualizations/miscellaneous/efficiency_over_time.pdf')
# --------------------------------------------------


# --------------------------------------------------
# Load/prep datat
load(outputFile4a)

# get year/semester
untransformed[,year:=floor(date)]
untransformed[, semester:=.5*round((date-.01)/.5) ]
byVar = 'year'

# look up cost per bednet as a fraction with and without GHE
itns = untransformed[,.(spend=sum(exp_M1_1+exp_M1_2+other_dah_M1_1), ghe=sum(ghe),
	activity=sum(value_ITN_received), output=sum(value_ITN_consumed)), by=byVar]
itns[, cost_per_activity_excluding_ghe:=spend/activity]
itns[, cost_per_output_excluding_ghe:=spend/output]
itns[, cost_per_activity_including_ghe:=(spend+ghe)/activity]
itns[, cost_per_output_including_ghe:=(spend+ghe)/output]
itns


# look up cost per patient treated as a fraction with and without GHE
acts = untransformed[,.(spend=sum(exp_M2_1+other_dah_M2), ghe=sum(ghe), 
	activity=sum(value_ACT_received), output=sum(value_totalPatientsTreated)), by=byVar]
acts[, cost_per_activity_excluding_ghe:=spend/activity]
acts[, cost_per_output_excluding_ghe:=spend/output]
acts[, cost_per_activity_including_ghe:=(spend+ghe)/activity]
acts[, cost_per_output_including_ghe:=(spend+ghe)/output]
acts



# look up cost per patient tested as a fraction with and without GHE
rdts = untransformed[,.(spend=sum(exp_M2_1+other_dah_M2), ghe=sum(ghe), 
	activity=sum(value_RDT_received), output=sum(value_RDT_completed)), by=byVar]
rdts[, cost_per_activity_excluding_ghe:=spend/activity]
rdts[, cost_per_output_excluding_ghe:=spend/output]
rdts[, cost_per_activity_including_ghe:=(spend+ghe)/activity]
rdts[, cost_per_output_including_ghe:=(spend+ghe)/output]
rdts

# append
itns[, intervention:='ITNs']
acts[, intervention:='ACTs']
rdts[, intervention:='RDTs']
costs = rbind(itns, acts)
costs = rbind(costs, rdts)
costs = melt(costs, id.vars=c('intervention',byVar))

# subset and label
costs = costs[grepl('cost_per',variable)]
costs[, includes_ghe:=ifelse(grepl('including',variable), paste('Spending on', intervention, 'plus GHE on all malaria'), paste('Spending on', intervention, 'not including GHE'))]
costs[, output:=ifelse(grepl('output',variable),'output','activity')]
# --------------------------------------------------


# --------------------------------------------------
# Graph

colors = brewer.pal(6, 'Paired')

p1 = ggplot(costs[intervention=='ITNs' & output=='activity' & grepl('not including GHE', includes_ghe)], 
		aes_string(y='value', x=byVar)) + 
	geom_smooth(size=1.5, alpha=.75, color=colors[2], se=F) + 
	geom_point(size=3, color=colors[2]) + 
	labs(title='Cost per ITN Shipped in USD', 
		subtitle='With and Without All Malaria Government Health Expenditure (GHE)', 
		y='Cost per Unit', x='', caption='Estimated as a direct fraction without use of model') + 
	theme_bw()

p2 = ggplot(costs[intervention=='ACTs' & output=='activity' & grepl('not including GHE', includes_ghe)], 
		aes_string(y='value', x=byVar)) + 
	geom_smooth(size=1.5, alpha=.75, color=colors[4], se=F) + 
	geom_point(size=3, color=colors[4]) +
	labs(title='Cost per ACT Shipped in USD', 
		subtitle='With and Without All Malaria Government Health Expenditure (GHE)', 
		y='Cost per Unit', x='', caption='Estimated as a direct fraction without use of model') + 
	theme_bw()

p3 = ggplot(costs[intervention=='RDTs' & output=='activity' & grepl('not including GHE', includes_ghe)], 
		aes_string(y='value', x=byVar)) + 
	geom_smooth(size=1.5, alpha=.75, color=colors[6], se=F) + 
	geom_point(size=3, color=colors[6]) +
	labs(title='Cost per RDT Shipped in USD', 
		subtitle='With and Without All Malaria Government Health Expenditure (GHE)', 
		y='Cost per Unit', x='', caption='Estimated as a direct fraction without use of model') + 
	theme_bw()
# --------------------------------------------------


# --------------------------------------------------
# --------------------------------------------------
