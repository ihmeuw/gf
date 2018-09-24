# --------------------------------------------------------
# Function that stanardizes DPS names in DRC
# 
# Inputs: 
# nameVector - a vector of class 'character' containing names of DPS's to be standardized
# 
# Outputs:
# cleanVector - a vector of class 'character' containing corresponding standard DPS names to nameVector
# 
# The current working directory should be the root of this repo
# --------------------------------------------------------

standardizeDPSNames = function(nameVector=NULL) {

	# test inputs
	if (is.null(nameVector)) stop('You must pass a vector of DPS names')
	if (class(nameVector)!='character') stop('nameVector must be a character vector')
	
	# load spreadsheet connecting all known names to standardized names
	require(data.table)
	altNamesFile = './core/alternate_hz_spellings.csv'
	alternateNames = fread(altNamesFile)
	
	# prep data table
	alternateNames = unique(alternateNames[, c('dps','dps_snis','dps_pnlp'), with=FALSE])
	alternateNames = melt(alternateNames, id.vars='dps', value.name='alternate_name')
	
	# make sure standard names are also an option as an input
	tmp = unique(alternateNames[, 'dps', with=FALSE])
	tmp[, alternate_name:=dps]
	alternateNames = rbind(alternateNames, tmp, fill=TRUE)
	
	# clean up alternate names
	alternateNames[, alternate_name:=tolower(alternate_name)]
	alternateNames[, alternate_name:=iconv(alternate_name, to='ASCII//TRANSLIT')]
	alternateNames[, alternate_name:=gsub(' ', '-', alternate_name)]
	
	# clean up input vector
	nameVector = tolower(nameVector)
	nameVector = iconv(nameVector, to='ASCII//TRANSLIT')
	nameVector = gsub(' ', '-', nameVector)
	
	# convert input vector to standardized names
	idx = match(nameVector, alternateNames$alternate_name)
	cleanVector = alternateNames[idx]$dps
	
	# test that it worked
	idx = which(is.na(cleanVector))
	if (any(is.na(cleanVector))) warning(paste0('DPS \'', paste(unique(nameVector[idx]), collapse=', '), '\' not in alternate names list (', altNamesFile, ')'))
	if (length(nameVector)!=length(cleanVector)) stop('Something went wrong. The input vector is a different length than the output vector!') 
	
	# return cleaned names
	return(cleanVector)
}
