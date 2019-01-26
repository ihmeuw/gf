# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# Final pre-processing for impact evaluation model
# This is built for the pilot dataset
# ------------------------------------------------


# ----------------------------------------------
# Load/prep data

# load
data = readRDS(outFile3)

# test unique identifiers
idVars = c('year','quarter','module','intervention','indicator')
test = nrow(data)==nrow(unique(data[,idVars, with=F]))
if (test==FALSE) stop(paste('Something is wrong.', paste(idVars, collapse=' '), 'do not uniquely identify rows.'))

# set aside results chain sections
inputs = unique(data[, c('year','quarter','intervention','budget')])
activities = unique(data[indicator_type=='activity', c('year','quarter','indicator','value', 'completeness')])
outputs = unique(data[indicator_type=='output', c('year','quarter','indicator','value', 'completeness')])

# look at inputs
ggplot(inputs, aes(y=budget, x=year+(quarter/4), color=intervention)) + 
	geom_line() + 
	geom_point()

# look at activities
ggplot(activities, aes(y=value, x=year+(quarter/4), color=indicator)) + 
	geom_line() + 
	geom_point()

# look at outputs
ggplot(outputs, aes(y=value, x=year+(quarter/4), color=indicator)) + 
	geom_line() + 
	geom_point()

# look at correlations
ggplot(data[indicator_type=='activity'], aes(y=value, x=budget)) + 
	geom_point() + 
	facet_wrap(~intervention, scales='free') + 
	geom_smooth(method='lm', se=FALSE)

# look at distributions
ggplot(inputs, aes(x=budget)) + 
	geom_histogram() + 
	facet_wrap(~intervention, scales='free')
# ----------------------------------------------


# --------------------------------
# Save file

# --------------------------------
