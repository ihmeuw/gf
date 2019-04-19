# -------------------------------------------------------------------------------
# David Phillips
# 
# 4/18/2019
# Function that takes a lavaan model object and runs it as unrelated regressions
# -------------------------------------------------------------------------------


# -----------------------------------------------------------------
# Inputs
# modelObject (character) - standard lavaan or blavaan model object
# data (data.table) - data to run regressions with
#
# Outputs
# urFit (data.table) - regression output formatted just like sem output
# -----------------------------------------------------------------


# -----------------------------------------------------------------
# Define function
lavaanUR = function(modelObject=NULL, data=NULL) {

	# extract model info
	parsedModel = lavParseModelString(modelObject)

	# construct formulae
	lhsVars = unique(parsedModel$lhs[parsedModel$op=='~'])
	formulae = lapply(lhsVars, function(v) { 
		paste0(v, '~', paste0(parsedModel$rhs[parsedModel$lhs==v], collapse='+'))
	})
	
	# run regressions
	lmFits = lapply(seq(length(formulae)), function(i) { 
		fit = lm(formulae[[i]], data)
		data.table(lhs=lhsVars[i], op='~', rhs=names(coef(fit)), est=coef(fit), 
			se=sqrt(diag(vcov(fit))))
	})
	
	# format like sem output
	urFit = rbindlist(lmFits)
	urFit[rhs=='(Intercept)', op:='~1']
	urFit[rhs=='(Intercept)', rhs:='']
	
	# return
	return(urFit)
	
# end function
}
# -----------------------------------------------------------------
