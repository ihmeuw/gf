# ----------------------------------------------
# Irena Chen
#
# 11/1/2017
# Master prep code that runs all other functions
# The current working directory should be the same as this code
# ----------------------------------------------
#define variables: 

dir <- 'J:/Project/Evaluation/GF/resource_tracking/gtm/ghe_s/'
period <-365
cost_category <- "All"

# ----------------------------------------------
# load the prep function


# load csv
file_list <- read.csv("C:/Users/irenac2/repos/gf/resource_tracking/prep/file_format_list.csv")

source('./prep_sicoin.r')

file_names <- file_list$filename[file_list$format=="c_coin"]
file_years <- file_list$year[file_list$format=="c_coin"]

inFile <- '2013 MALARIA PRESUPUESTO POR ORGANISMO (departamento municipio).xls'


resource_database <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), 
                          c("source", "start_date", 
                            "period","loc_id", "cost", 
                            "budget","disbursement", "expenditures"))

## loop over all of the files 
for(i in 1:length(file_names))

  tmpData <- prepSicoin(dir, file_names[i], file_years[i], period, cost_category)
  
  resource_database = rbind(resource_database, tmpData)

}


# data directory
dir <- 'C:/Users/irenac2/Documents/'

# input file
inFile <- paste0(dir, '2013 MALARIA PRESUPUESTO POR ORGANISMO (departamento municipio).xls')
