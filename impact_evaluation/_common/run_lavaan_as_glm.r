# -------------------------------------------------------------------------------
# David Phillips
# 
# 4/18/2019
# Function that takes a lavaan model object and runs it as unrelated regressions
# -------------------------------------------------------------------------------


# -----------------------------------------------------------------
# Inputs
# modelObject (character) - standard lavaan or blavaan model object
#
# Outputs
# urFit (list) - a list of regression outputs
# -----------------------------------------------------------------


# -----------------------------------------------------------------
# Define function
lavaanUR = function(modelObject=NULL) {

	# extract model info
	parsedModel = lavParseModelString(modelObject)

	

# end function
}
# -----------------------------------------------------------------
