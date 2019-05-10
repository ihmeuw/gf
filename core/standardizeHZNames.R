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
#
# NOTE:
# The GADM shapefile that we are using for DPS-level maps is not included in the alternate spellings csv
# I'm not sure if it's possible to include it based on how it is formatted so I've added code to do that 
# part 'manually' but at least it will be included in this function.  - Audrey
# --------------------------------------------------------

standardizeHZNames = function(nameVector=NULL) {

	# test inputs
	if (is.null(nameVector)) stop('You must pass a vector of HZ names')
	if (class(nameVector)!='character') stop('nameVector must be a character vector')
  
	# load spreadsheet connecting all known names to standardized names
	require(data.table)
	altNamesFile = './core/hz_renaming_file.csv'
	alternateNames = fread(altNamesFile, header=TRUE)
	
	# prep data table
	alternateNames = unique(alternateNames[, c('health_zone','hz_shp1','hz_shp2', 'hz_snis', 'hz_pnlp', 'hz_snis_cleaned', 'hz_pnlt', 'hz_sv'), with=FALSE])
	alternateNames = melt(alternateNames, id.vars=c('health_zone'), value.name='alternate_name')
	
	# make sure standard names are also an option as an input
	tmp = unique(alternateNames[, 'health_zone', with=FALSE])
	tmp[, alternate_name:=health_zone]
	alternateNames = rbind(alternateNames, tmp, fill=TRUE)
	
	# clean up alternate names
	alternateNames[, alternate_name:=gsub('é', 'e', alternate_name)]
	alternateNames[, alternate_name:=iconv(alternate_name, 'WINDOWS-1252','UTF-8')]
	alternateNames[, alternate_name:=tolower(alternate_name)]
	alternateNames[, alternate_name:=iconv(alternate_name, to='ASCII//TRANSLIT')]
	alternateNames[, alternate_name:=gsub(' ', '-', alternate_name)]
	
	# remove duplicates from alternate names list
	alternateNames = unique(alternateNames[,c('health_zone','alternate_name'), with=FALSE])
	alternateNames = alternateNames[ !is.na(alternate_name) ]
	
	# clean up input vector
	nameVector = gsub('é', 'e', nameVector)
	nameVector = iconv(nameVector, 'WINDOWS-1252','UTF-8')
	nameVector = tolower(nameVector)
	nameVector = iconv(nameVector, to='ASCII//TRANSLIT')
	nameVector = gsub(' ', '-', nameVector)

	# convert input vector to standardized names
	idx = match(nameVector, alternateNames$alternate_name)
	cleanVector = alternateNames[idx]$health_zone

	# test that it worked
	idx = which(is.na(cleanVector))
	if (any(is.na(cleanVector))) warning(paste0('HZ \'', paste(unique(nameVector[idx]), collapse=', '), '\' not in alternate names list (', altNamesFile, ')'))
	if (length(nameVector)!=length(cleanVector)) stop('Something went wrong. The input vector is a different length than the output vector!') 
	
	# return cleaned names
	return(cleanVector)
}
