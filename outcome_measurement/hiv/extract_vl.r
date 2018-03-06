# ----------------------------------------------
# David Phillips, Caitlin O'Brien-Carelli
#
# 3/5/2018
# Function that runs VL extraction for a single month-years
# Inputs:
# y - 2-digit year
# m - 2-digit month
# Outputs:
# nothing. saves one file per age/sex/tb status
# ----------------------------------------------


# --------------------
# Set up R
y <- commandArgs()[4]
m <- commandArgs()[5]
library(jsonlite)
# --------------------


# ----------------------------------------------
# Files and directories

# output file
dir = '/home/j/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscraped_data_repeat/'

# parameters
ages = c('0,1', '1,2,3,4', '5,6,7,8,9,10', '11, 12, 13, 14, 15', 
		'16, 17, 18, 19, 20', '21, 22, 23, 24, 25', '26, 27, 28, 29, 30', 
		'31, 32, 33, 34, 35', '36, 37, 38, 39', '40,41, 42, 43, 44, 45', 
		'46, 47, 48, 49, 50', '51, 52, 53, 54, 55', '56, 57, 58, 59, 60', 
		'61,62,63,64,65', '66,67,68,69,70', '71,72,73,74,75', '76,77,78,79,80', 
		'81,82,83,84,85', '86,87,88,89,90', '91,92,93,94,95', '96,97,98,99')
tbs = c('y','n','x')
sexes = c('m','f','x')

# for testing purposes
# y='16'
# m='01'
ages = '0,1'
tbs = 'n'
sexes = 'f'
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data

# loop over age groups
for(a in ages) { 
  
  # loop over tb groups - includes "unknown" option
  for(t in tbs) { 
	
	# loop over sexes - includes "unknown" option
	for(s in sexes) { # make sure "U" is the correct value

		# store rds file location
		outFile = paste0(dir, '/20', y, '/facilities_suppression_', m,'20', y,'_',a,'_', s, '_tb', t, '.rds')
			  
		# store url
		url = paste0('https://vldash.cphluganda.org/live?age_ids=%5B', a, '%5D&districts=%5B%5D&emtct=%5B%5D&fro_date=20', y, m,'&genders=%5B%22', s, '%22%5D&hubs=%5B%5D&indications=%5B%5D&lines=%5B%5D&regimens=%5B%5D&tb_status=%5B%22', t, '%22%5D&to_date=20',y, m)

		# load
		print(paste('Loading json from:', url))
		data = fromJSON(url)

		# save raw output
		print(paste('Saving data to:', outFile))
		saveRDS(data, file=outFile)
	}
  }
}
# ----------------------------------------------
