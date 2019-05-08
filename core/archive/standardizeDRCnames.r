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
	
	cleanVector = NULL
	
	# prep data table
	if (unit == "dps"){
  	alternateNames = unique(alternateNames[, c('dps','dps_snis','dps_pnlp'), with=FALSE])
  	alternateNames = melt(alternateNames, id.vars='dps', value.name='alternate_name')
  	
  	# make sure standard names are also an option as an input
  	tmp = unique(alternateNames[, 'dps', with=FALSE])
  	tmp[, alternate_name:=dps]
  	alternateNames = rbind(alternateNames, tmp, fill=TRUE)
  	
  	for(n in 1:length(nameVector)){
  	  cleanVector[n] = alternateNames[alternate_name==nameVector[n], unique(dps)]
  	}
  	
	} else if (unit == "hz"){
	  alternateNames = unique(alternateNames[, c('health_zone','hz_snis','hz_pnlp', 'hz_shp1', 'hz_shp2'), with=FALSE])
	  alternateNames = melt(alternateNames, id.vars='health_zone', value.name='alternate_name')
	  
	  # make sure standard names are also an option as an input
	  tmp = unique(alternateNames[, 'health_zone', with=FALSE])
	  tmp[, alternate_name:=health_zone]
	  alternateNames = rbind(alternateNames, tmp, fill=TRUE)
	  
	  for(n in 1:length(nameVector)){
	    if( nameVector[n] %in% alternateNames$alternate_name){
  	    cleanVector[n] = alternateNames[alternate_name==nameVector[n], unique(health_zone)]
	    } else {
	      cleanVector[n] = nameVector[n]
	      warning ( paste0( nameVector[n], " was not in the alternate names sheet. The name has not changed."))
	    }
	  }
	  
	} else{
	  stop('Unit must be either "dps" or "hz"')
	}
	
	# # clean up alternate names
	# alternateNames[, alternate_name:=tolower(alternate_name)]
	# alternateNames[, alternate_name:=iconv(alternate_name, to='ASCII//TRANSLIT')]
	# alternateNames[, alternate_name:=gsub(' ', '-', alternate_name)]
	# # make sure that bas-congo is changed to kongo-central
	# alternateNames[, alternate_name:=gsub("bas-congo", "kongo-central", alternate_name)]
	# 
	# # clean up input vector
	# nameVector = tolower(nameVector)
	# nameVector = iconv(nameVector, to='ASCII//TRANSLIT')
	# nameVector = gsub(' ', '-', nameVector)
	# # make sure that bas-congo is changed to kongo-central
	# nameVector <- gsub("bas-congo", "kongo-central", nameVector)
	#   # ADD MORE NECESSARY THINGS HERE----------------------
	# 
	# # convert input vector to standardized names
	# idx = match(nameVector, alternateNames$alternate_name)
	# cleanVector = alternateNames[idx]$dps
	# 
	# # test that it worked
	# idx = which(is.na(cleanVector))
	# if (any(is.na(cleanVector))) warning(paste0('DPS \"', paste(nameVector[idx], collapse=', '), '\" not in alternate names list (', altNamesFile, ')'))
	# if (length(nameVector)!=length(cleanVector)) warning('Something went wrong. The input vector is a different length than the output vector!') 
	# 
	# # return cleaned names
	# return(cleanVector)
	
	return(cleanVector)
}
