# scratch code to quickly get share of spending by intervention
data = readRDS("J:/Project/Evaluation/GF/impact_evaluation/cod/prepped_data/prepped_resource_tracking.RDS")

agg = data[date>=2010 & date<2017, lapply(.SD, sum), .SDcols=names(data)[grepl('exp|dah|ghe',names(data))]]
agg[, ghe:=ghe/1000000]

# display spending on ITNs
agg[, gf_itns:=(exp_M1_1+exp_M1_2)/1000000]
agg[, other_itns:=(other_dah_M1_1+other_dah_M1_2)/1000000]
agg[, c('gf_itns','other_itns')]

agg[, gf_acts:=(exp_M2_1+exp_M2_6)/1000000]
agg[, other_acts:=(other_dah_M2)/1000000]
agg[, c('gf_acts','other_acts','ghe')]

