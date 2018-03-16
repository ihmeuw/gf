# Caitlin O'Brien-Carelli
# 3/6/2016

# checks the # of files in the year/month folders for the Uganda VL webscrape
# -----------------------------------------------------------------------

# set up R
rm(list=ls())
library(data.table)

# --------------------

# set directory
dir = 'J:/Project/Evaluation/GF/outcome_measurement/uga/vl_dashboard/webscraped_data_repeat'

# loop into each folder and count the # of files

year <- c(2014, 2015, 2016, 2017, 2018)
month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10, 11, 12)

  for (y in year) {
  for (m in month) {
    f <- paste0(dir,'/',y,'/', m)
    print(paste("For the month", m , "in year", y))
    print(paste("The number of files is", (length(list.files(f))) ))
  }
}

# --------------------

  for (y in year) {
  for (m in month) {
    f <- paste0(dir,'/',y,'/', m)
    x <- (length(list.files(f)) )

    if (x!=57) {
      print(c(m, y, x))
    } else {
        }
      }
    }
    

# -----------------------------------------------------------------------
