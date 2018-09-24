# --------------------------------------------------------
# Function that stanardizes DPS names in DRC
# 
# Inputs: 
# nameVector - a vector of class "character" containing names of DPS's to be standardized
# 
# Outputs:
# cleanVector - a vector of class "character" containing corresponding standard DPS names to nameVector
#
# NOTES:
# The current working directory should be the root of the repo.
# --------------------------------------------------------

standardizeDRCnames = function(nameVector=NULL, unit=NULL) {

  unit <- tolower(unit)
  
	# test inputs- nameVector
	if (is.null(nameVector)) stop('You must pass a vector of names')
	if (class(nameVector)!='character') stop('nameVector must be a character vector')
  # test inputs - unit
  if (is.null(unit)) stop('You must pass either "dps" or "hz" to the function')
  if (class(unit)!='character') stop('Unit must be a character vector')
  if (unit !='dps' & unit !='hz') stop('Unit must be either "dps" or "hz"')
  
	# load spreadsheet connecting all known names to standardized names
	require(data.table)
	altNamesFile = "./core/alternate_hz_spellings.csv"
	alternateNames = fread(altNamesFile)
	
	# prep data table
	if (unit == "dps"){
  	alternateNames = unique(alternateNames[, c('dps','dps_snis','dps_pnlp'), with=FALSE])
  	alternateNames = melt(alternateNames, id.vars='dps', value.name='alternate_name')
	} else if (unit == "hz"){
	  
	} else{
	  stop('Unit must be either "dps" or "hz"')
	}
  	
	# clean up alternate names
	alternateNames[, alternate_name:=tolower(alternate_name)]
		# ADD MORE NECESSARY THINGS HERE
	
	# convert input vector to standardized names
	idx = match(nameVector, alternateNames$alternate_name)
	cleanVector = alternateNames[idx]$dps
	
	# test that it worked
	idx = which(is.na(cleanVector))
	if (any(is.na(cleanVector))) warning(paste0('DPS \"', paste(nameVector[idx], collapse=', '), '\" not in alternate names list (', altNamesFile, ')'))
	if (length(nameVector)!=length(cleanVector)) stop('Something went wrong. The input vector is a different length than the output vector!') 
	
	# return cleaned names
	return(cleanVector)
}
