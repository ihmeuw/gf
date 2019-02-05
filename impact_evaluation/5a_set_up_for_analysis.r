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


# ---------------------------------------------------------------------------------------
# Run final tests

# test unique identifiers
test = nrow(data)==nrow(unique(data[,'date', with=F]))
if (test==FALSE) stop(paste('Something is wrong. date does not uniquely identify rows.'))
# ---------------------------------------------------------------------------------------


# --------------------
# Save file
saveRDS(outputFile5a)
# --------------------
