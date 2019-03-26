# save a time-stamped version of output files in archive folder, for reproducibility of impact evaluation results


archive <- function( outputFile ){
  
  #if( class(object)[1] != "data.table" & class(object)[1] != "data.frame" ) stop ("Object is not a data table or data frame, you might not want to save it as an RDS")
  
  date_time = gsub('-|:| ', '_', Sys.time())
  
  outputFileArchive = gsub('prepped_data/', 'prepped_data/archive/', outputFile)
  outputFileArchive = gsub('.rds', (paste0('_', date_time, '.rds')), outputFileArchive, ignore.case = TRUE)
  
  file.copy(outputFile, outputFileArchive)
  
  return(paste0("File copied from ", outputFile, " to ", outputFileArchive))
}