# ------------------------------------------------
# David Phillips
# 
# 1/18/2019
# Final pre-processing for impact evaluation model
# This is built for the pilot dataset
# The current working directory should be the root of this repo (set manually by user)
# ------------------------------------------------

source('./impact_evaluation/_common/set_up_r.r')

# -----------------------------------------------------------------
# Load/prep data

# load
data = readRDS(outputFile3)

# compute cumulative budgets
rtVars = names(data)
rtVars = rtVars[grepl('budget|other_dah', rtVars)]
for(v in rtVars) data[, (paste0(v,'_cumulative')):=cumsum(get(v))]
# -----------------------------------------------------------------


# -----------------------------------------------------------------------
# Hotfixes for Heywood cases

# drop zero-variance variables
for(v in names(data)) if (sd(data[[v]],na.rm=T)==0) data[[v]] = NULL

# extrapolate where necessary
data = data[date>=2010 & date<=2018]
for(v in names(data)) {
	form = as.formula(paste0(v,'~date'))
	lmFit = glm(form, data, family='poisson')
	data[, tmp:=exp(predict(lmFit, newdata=data))]
	# print(ggplot(data, aes_string(y=v,x='date')) + geom_point() + geom_line(aes(y=tmp)) + labs(title=v))
	data[is.na(get(v)), (v):=tmp]
}
data$tmp = NULL

# drop completeness variables (for now)
for(v in names(data)[grepl('completeness', names(data))]) data[[v]]=NULL
# -----------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Run final tests

# na omit
data = na.omit(data)

# test unique identifiers
test = nrow(data)==nrow(unique(data[,'date', with=F]))
if (test==FALSE) stop(paste('Something is wrong. date does not uniquely identify rows.'))
# ---------------------------------------------------------------------------------------


# -------------------------
# Save file
saveRDS(data, outputFile5a)
# -------------------------
