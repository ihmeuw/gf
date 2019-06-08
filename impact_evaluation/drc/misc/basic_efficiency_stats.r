load(outputFile4a)
untransformed[,year:=floor(date)]

# look up cost per bednet as a fraction with and without GHE
itns = untransformed[,.(spend=sum(exp_M1_1+exp_M1_2+other_dah_M1_1), ghe=sum(ghe),
	activity=sum(value_ITN_received), output=sum(value_ITN_consumed)), by='year']
itns[, cost_per_activity_excluding_ghe:=spend/activity]
itns[, cost_per_output_excluding_ghe:=spend/output]
itns[, cost_per_activity_including_ghe:=(spend+ghe)/activity]
itns[, cost_per_output_including_ghe:=(spend+ghe)/output]
itns


# look up cost per patient treated as a fraction with and without GHE
acts = untransformed[,.(spend=sum(exp_M2_1+other_dah_M2), ghe=sum(ghe), 
	activity=sum(value_ACT_received), output=sum(value_totalPatientsTreated)), by='year']
acts[, cost_per_activity_excluding_ghe:=spend/activity]
acts[, cost_per_output_excluding_ghe:=spend/output]
acts[, cost_per_activity_including_ghe:=(spend+ghe)/activity]
acts[, cost_per_output_including_ghe:=(spend+ghe)/output]
acts



# look up cost per patient tested as a fraction with and without GHE
rdts = untransformed[,.(spend=sum(exp_M2_1+other_dah_M2), ghe=sum(ghe), 
	activity=sum(value_RDT_received), output=sum(value_RDT_completed)), by='year']
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
costs = melt(costs, id.vars=c('intervention','year'))

# subset and label
costs = costs[grepl('cost_per',variable)]
costs[, includes_ghe:=grepl('including',variable)]
costs[, output:=ifelse(grepl('output',variable),'output','activity')]

# graph
ggplot(costs, aes(y=value, x=year, color=includes_ghe, linetype=output)) + 
	geom_line() + 
	geom_point() + 
	facet_wrap(~intervention, scales='free', ncol=1) + 
	labs(title='Cost per Output or Activity in USD', subtitle='With and Without All Malaria GHE', 
		y='Cost per Unit', x='') + 
	theme_bw()	
	
	