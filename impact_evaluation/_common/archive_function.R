# save a time-stamped version of output files in archive folder, for reproducibility of impact evaluation results


archive <- function( outputFile, folder_name = "archive" ){
  
  #if( class(object)[1] != "data.table" & class(object)[1] != "data.frame" ) stop ("Object is not a data table or data frame, you might not want to save it as an RDS")
  
  date_time = gsub('-|:| ', '_', Sys.time())
  
  outputFileArchive = outputFile
  
  if (!grepl("visualizations", outputFile)) outputFileArchive = gsub('prepped_data/', paste0('prepped_data/',folder_name, '/'),  outputFileArchive)
  outputFileArchive = gsub('visualizations/', paste0('visualizations/',folder_name, '/'),  outputFileArchive)
  
  outputFileArchive = gsub('.rds', (paste0('_', date_time, '.rds')), outputFileArchive, ignore.case = TRUE)
  outputFileArchive = gsub('.rdata', (paste0('_', date_time, '.rdata')), outputFileArchive, ignore.case = TRUE)
  outputFileArchive = gsub('.pdf', (paste0('_', date_time, '.pdf')), outputFileArchive, ignore.case = TRUE)
  
  file.copy(outputFile, outputFileArchive)
  
  return(paste0("File copied from ", outputFile, " to ", outputFileArchive))
  
}
