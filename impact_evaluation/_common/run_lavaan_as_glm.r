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

  # parse formulas
  formulae = unlist(str_split(model, '\n'))
  formulae = trimws(formulae)
  
	# extract model info
	parsedModel = lavParseModelString(modelObject)
  
	# ignore covariance terms and other operators not possible in GLM
	parsedModel$lhs = parsedModel$lhs[parsedModel$op=='~']
	parsedModel$rhs = parsedModel$rhs[parsedModel$op=='~']

	# drop meaningless lines from parsed formulae
	formulae = formulae[!grepl("#", formulae)] #Drop comments
	formulae = formulae[grepl('~', formulae)] #Only keep lines that have actual formulas in them. 
	
	# standardize data
	vars = unique(c(parsedModel$lhs, parsedModel$rhs))
	vars = unique(unlist(str_split(vars, ':|\\*')))
	data.std = copy(data)
	for(v in vars) data.std[, (v):=(get(v)-mean(get(v)))/ifelse(sd(get(v))>0, sd(get(v)), 1)]
	
	# run regressions
	lmFits = lapply(seq(length(formulae)), function(i) { 
		fit = lm(formulae[[i]], data)
		fit.std = lm(formulae[[i]], data.std)
		lhsVar = tstrsplit(formulae[[i]], "~", keep=1)
		data.table(lhs=unlist(lhsVar), op='~', rhs=names(coef(fit)), 
			est=coef(fit), se=sqrt(diag(vcov(fit))), 
			est.std=coef(fit.std), se.std=sqrt(diag(vcov(fit.std))))
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
