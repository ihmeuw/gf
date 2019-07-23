# explore the reasoning behind using cumulative sums in model 1

# set up R
source('./impact_evaluation/drc/set_up_r.r')

outFile = 'J:/Project/Evaluation/GF/impact_evaluation/cod/visualizations/miscellaneous/cumulative_vs_direct.pdf'

# load data
load(outputFile4a)

# ------------------------------------------------------------------------
# ITNs 

# aggregate to national level
itnVars = c('exp_M1_1','exp_M1_2','other_dah_M1_1','exp_M1_1_cumulative','exp_M1_2_cumulative','other_dah_M1_1_cumulative','value_ITN_received','ITN_received_cumulative')
agg = untransformed[, lapply(.SD, sum), by=date, .SDcols=itnVars]

# combine Global Fund categories
agg[, gf_spend:=exp_M1_1+exp_M1_2]
agg[, gf_spend_cumulative:=exp_M1_1_cumulative+exp_M1_2_cumulative]

# graph without cumulative
p1 = ggplot(agg, aes(y=value_ITN_received, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='ITNs Shipped (#)',x='Quarter',title='ITNs Shipped', subtitle='DRC, National Level') + 
	theme_bw()

p2 = ggplot(agg, aes(y=gf_spend, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='Global Fund Expenditure on ITNs ($)',x='Quarter',title='Global Fund Expenditure on ITNs', subtitle='DRC, National Level') + 
	theme_bw()

p3 = ggplot(agg, aes(y=value_ITN_received/gf_spend, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='ITNs Shipped per Dollar Spent by Global Fund',x='Quarter',title='ITNs Shipped per Dollar Spent by Global Fund', subtitle='DRC, National Level') + 
	theme_bw()

# graph with cumulative
p4 = ggplot(agg, aes(y=ITN_received_cumulative, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='Cumulative ITNs Shipped (#)',x='Quarter',title='Cumulative ITNs Shipped', subtitle='DRC, National Level') + 
	theme_bw()

p5 = ggplot(agg, aes(y=gf_spend_cumulative, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='Cumulative Global Fund Expenditure on ITNs ($)',x='Quarter',title='Cumulative Global Fund Expenditure on ITNs', subtitle='DRC, National Level') + 
	theme_bw()

p6 = ggplot(agg, aes(y=ITN_received_cumulative/gf_spend_cumulative, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='Cumulative ITNs Shipped per Cumulative Dollar Spent by Global Fund',x='Quarter',title='Cumulative ITNs Shipped per Cumulative Dollar Spent by Global Fund', subtitle='DRC, National Level') + 
	theme_bw()
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# ACTs

# aggregate to national level
actVars = c('exp_M2_1','other_dah_M2','exp_M2_1_cumulative','other_dah_M2_cumulative','value_ACT_received','ACT_received_cumulative')
agg = untransformed[, lapply(.SD, sum), by=date, .SDcols=actVars]

# combine Global Fund categories
agg[, gf_spend:=exp_M2_1]
agg[, gf_spend_cumulative:=exp_M2_1_cumulative]

# graph without cumulative
p7 = ggplot(agg, aes(y=value_ACT_received, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='ACTs Shipped (#)',x='Quarter',title='ACTs Shipped', subtitle='DRC, National Level') + 
	theme_bw()

p8 = ggplot(agg, aes(y=gf_spend, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='Global Fund Expenditure on ACTs ($)',x='Quarter',title='Global Fund Expenditure on ACTs', subtitle='DRC, National Level') + 
	theme_bw()

p9 = ggplot(agg, aes(y=value_ACT_received/gf_spend, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='ACTs Shipped per Dollar Spent by Global Fund',x='Quarter',title='ACTs Shipped per Dollar Spent by Global Fund', subtitle='DRC, National Level') + 
	theme_bw()

# graph with cumulative
p10 = ggplot(agg, aes(y=ACT_received_cumulative, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='Cumulative ACTs Shipped (#)',x='Quarter',title='Cumulative ACTs Shipped', subtitle='DRC, National Level') + 
	theme_bw()

p11 = ggplot(agg, aes(y=gf_spend_cumulative, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='Cumulative Global Fund Expenditure on ACTs ($)',x='Quarter',title='Cumulative Global Fund Expenditure on ACTs', subtitle='DRC, National Level') + 
	theme_bw()

p12 = ggplot(agg, aes(y=ACT_received_cumulative/gf_spend_cumulative, x=date)) + 
	geom_point() + 
	geom_line() + 
	labs(y='Cumulative ACTs Shipped per Cumulative Dollar Spent by Global Fund',x='Quarter',title='Cumulative ACTs Shipped per Cumulative Dollar Spent by Global Fund', subtitle='DRC, National Level') + 
	theme_bw()
# ------------------------------------------------------------------------

# save
pdf(outFile, height=6, width=9)
grid.arrange(p1,p2)
p3
grid.arrange(p4,p5)
p6

grid.arrange(p7,p8)
p9
grid.arrange(p10,p11)
p12
dev.off()
