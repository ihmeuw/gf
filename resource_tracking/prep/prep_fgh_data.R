
## load the fgh data

fgh_data <- data.table(read.csv("H:/rt_data/ihme_dah_cod_uga_gtm_1990_2016.csv",fileEncoding="latin1"))


fgh_data <- fgh_data[channel=="GFATM"]

##change the column names to match our dataset
setnames(fgh_data, "iso3_rc", "country")

# now get the columns we want: 

toMatch <- c("hiv", "mal", "tb", "hss", "year", "source", "channel", "country")

drop.cols <- (grep(paste(toMatch, collapse="|"), colnames(fgh_data)))
fghData<- fgh_data[,drop.cols, with=FALSE]

## "melt" the data: 

fghData <- melt(fghData, id=c("year", "source", "channel", "country"), variable.name = "sda_orig", value.name="disbursement")


get_disease <- function(sda_orig){
  x <- "hss"
  if(grepl("hiv", sda_orig)){
    x <- "hiv"
  } else if(grepl("mal", sda_orig)){
    x <- "malaria"
  } else if (grepl("tb", sda_orig)){
    x <- "tb"
  } else {
    x <- x
  }
  return(x)
}

##get the disease column: 


fghData$disease <- mapply(get_disease, fghData$sda_orig)

## add in 






