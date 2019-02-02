# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# Exploratory visualizations to get a good sense of the impact evaluation data
# The current working directory should be the root of this repo (set manually by user)
# ------------------------------------------------

source('./impact_evaluation/_common/set_up_r.r')

# ----------------------------------------------------------------------------
# Load/prep data

# load
data = readRDS(outputFile3)

# test unique identifiers
idVars = c('year','quarter','code','module','intervention','indicator','indicator_type')
test = nrow(data)==nrow(unique(data[,idVars, with=F]))
if (test==FALSE) stop(paste('Something is wrong.', paste(idVars, collapse=' '), 'do not uniquely identify rows.'))

# prep work that shouldn't be necessary once bugs are fixed
data[, date:=year+((quarter-1)/4)]
data[, intervention:=gsub('–','-',intervention)]
data = data[year>2004]
inputs = na.omit(data[, .(budget=sum(budget,na.rm=T), other_dah=sum(other_dah,na.rm=T)), 
	by=c('date','code')])
activities = na.omit(data[indicator_type=='activity', .(value=sum(value,na.rm=T), 
	completeness=mean(completeness,na.rm=T)), by=c('date','indicator_type','indicator')])
outputs = na.omit(data[indicator_type=='output', .(value=sum(value,na.rm=T), 
	completeness=mean(completeness,na.rm=T)), by=c('date','indicator')])
inputs_wide = dcast(inputs, date~code, value.var=c('budget','other_dah'))
activities_wide = dcast(activities, date~indicator, value.var=c('value','completeness'))
outputs_wide = dcast(outputs, date~indicator, value.var=c('value','completeness'))
frame = data.table(date=seq(from=min(data$year), to=max(data$year)+1, by=.25))
wide = merge(frame, inputs_wide, by='date', all.x=TRUE)
for(v in names(wide)) wide[is.na(get(v)), (v):=0]
wide = merge(wide, activities_wide, by='date', all.x=TRUE)
wide = merge(wide, outputs_wide, by='date', all.x=TRUE)
wide$other_dah_M2_6=NULL
wide$other_dah_M3_1=NULL
impVars = sapply(wide, function(x) mean(is.na(x)))
nomissVars = names(impVars[impVars==0])
impVars = impVars[impVars!=0]
impVars = names(impVars)
for(v in impVars) { 
	form = as.formula(paste0(v, '~', paste0(names(wide)[names(wide)!=v],collapse='*')))
	form = as.formula(paste0(v, '~', paste0(nomissVars,collapse='*')))
	lmFit = lm(form, wide)
	preds = predict(lmFit, newdata=wide[,1:11, with=F])
	wide[is.na(get(v)), (v):=preds]
}

# compute cumulative budgets
rtVars = names(wide)
rtVars = rtVars[grepl('budget|other_dah', rtVars)]
for(v in rtVars) wide[, (paste0(v,'_cumulative')):=cumsum(get(v))]
# ----------------------------------------------------------------------------


# ----------------------------------------------
# Set up to graph

# ----------------------------------------------


# ----------------------------------------------
# Make graphs

# time series of inputs
p1a = ggplot(inputs, aes(y=budget, x=year+(quarter/4), color=intervention)) + 
	geom_line() + 
	geom_point() + 
	labs(y='Budget', x='Quarter', color='Intervention') + 
	theme_bw(base_size=16)

# time series of cumulative inputs
p1b = ggplot(inputs, aes(y=budget_cumulative, x=year+(quarter/4), color=intervention)) + 
	geom_line() + 
	geom_point() + 
	labs(y='Cumulative Budget', x='Quarter', color='Intervention') + 
	theme_bw(base_size=16)

# time series of activities
p2 = ggplot(activities, aes(y=value, x=year+(quarter/4), color=indicator)) + 
	geom_line() + 
	geom_point() + 
	labs(y='Quantity', x='Quarter', color='Activity') + 
	theme_bw(base_size=16)

# time series of outputs
p3 = ggplot(outputs, aes(y=value, x=year+(quarter/4), color=indicator)) + 
	geom_line() + 
	geom_point() + 
	labs(y='Quantity', x='Quarter', color='Output') + 
	theme_bw(base_size=16)

# histograms of distributions
p4 = ggplot(inputs, aes(x=budget)) + 
	geom_histogram() + 
	facet_wrap(~intervention, scales='free') + 
	labs(y='Frequency (Quarters)', x='Budget') + 
	theme_bw(base_size=16)

# scatterplot of correlations
p5a = ggplot(data[indicator_type=='activity'], aes(y=value, x=budget)) + 
	geom_point() + 
	facet_wrap(~intervention, scales='free') + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Activity Value', x='Budget') + 
	theme_bw(base_size=16)
p5o = ggplot(data[indicator_type=='outputs'], aes(y=value, x=budget)) + 
	geom_point() + 
	facet_wrap(~intervention, scales='free') + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Output Value', x='Budget') + 
	theme_bw(base_size=16)

# scatterplot of lag-correlations
p6a1 = ggplot(data[indicator_type=='activity'], aes(y=value, x=budget_lag1)) + 
	geom_point() + 
	facet_wrap(~intervention, scales='free') + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Activity Value', x='Budget (1-Quarter Lag)') + 
	theme_bw(base_size=16)
p6a4 = ggplot(data[indicator_type=='activity'], aes(y=value, x=budget_lag4)) + 
	geom_point() + 
	facet_wrap(~intervention, scales='free') + 
	geom_smooth(method='lm', se=FALSE) + 
	labs(y='Activity Value', x='Budget (1-Year Lag)') + 
	theme_bw(base_size=16)
# ----------------------------------------------


# --------------------------------
# Save file
pdf(outputFile4, height=5.5, width=9)
p1a
p1b
p2
p3
p4
p5a
p5o
p6a1
p6a4
dev.off()
# --------------------------------
