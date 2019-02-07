# Audrey Batzel
# 9/17/18
# function to clean eval sheets in 2017 data

# TO DO : expand this to cover other years as well
# ----------------------------------------------
## variables to use
# ----------------------------------------------
unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

dps_names <- c('kwango', 'kwilu', 'mai-ndombe', 'kongo-central-est', 'kongo-central-ouest', 'equateur', 'mongala', 'nord-ubangi', 'sud-ubangi', 'tshuapa', 'kasai', 'kasai-central', 
               'kasai-oriental', 'lomami', 'sankuru', 'haut-katanga', 'haut-lomami', 'lualaba', 'tanganyika', 'kinshasa', 'maniema', 'nord-kivu', 'sud-kivu', 'ituri', 'tshopo', 
               'bas-uele', 'haut-uele')
# ----------------------------------------------


# ----------------------------------------------
## functions for set up
# ----------------------------------------------
## get all files in a given year folder

getFiles <- function(year){
  files = list.files(paste0(dir, year))
  return(files)
}

# # get file names for a given year:
# files <- getFiles(year)
# 
# # clean file names to remove ones without data for cleaning:
# files_xlsx <- files[!grepl(".DOC", files)]
# files_xlsx <- files_xlsx[!grepl("NATIONAL", files_xlsx)]
# files_xlsx <- files_xlsx[!grepl("SUBMISSION_DATES", files_xlsx)]
# files_xlsx <- files_xlsx[!grepl("HEALTH_ZONES", files_xlsx)]
# files_xlsx <- files_xlsx[!grepl("INDICATORS_COLLECTED", files_xlsx)]

## get all sheets in a given file

getSheets <- function(file, year){
  sheets <- excel_sheets(paste0(dir, year, "/", file))
  
  # # determine what quarter the file is from:  --- NOT RELEVANT FOR 2017/2018
  # if (grepl("T1", file)){
  #   quarter= "T1"
  # }else if (grepl("T2", file)){
  #   quarter= "T2"
  # }else if (grepl("T3", file)){
  #   quarter= "T3"
  # }else if (grepl("T4", file)){
  #   quarter= "T4"
  # }
  # return(sheets)
}
## NOTE: three types of sheets to clean:  DEP, AGE, and EVAL
# ----------------------------------------------


# ----------------------------------------------
## functions for prep
# ----------------------------------------------
initial_clean <- function(dt, year){
    # remove rows at the top up until the header row
    setnames(dt, colnames(dt)[1], "col1")
    setnames(dt, colnames(dt)[2], "col2")
    
    index <- grep("CPLT", dt$col1 )
    if (year==2018) index <- grep("CSDT", dt$col2)
  
    index <- index[1]
    
    dt <- dt[-c(1:(index-1))]
    
    # remove rows that are entirely NA
    rows_to_remove <- apply(dt, 1, function(x) all(is.na(x)))
    dt <- dt[!rows_to_remove, ]
    
    # remove totals rows
    dt <- dt[!grepl("TOTAL", col1)]
    dt <- dt[!grepl("RDC", col1)]
    dt <- dt[!grepl("TOTAL", col2)]
    dt <- dt[!grepl("RDC", col2)]
    
    return(dt)
}

clean_dps_names <- function(dt){
  # vector of DPS names
  dps_names <- c('kwango', 'kwilu', 'mai-ndombe', 'kongo-central-est', 'kongo-central-ouest', 'equateur', 'mongala', 'nord-ubangi', 'sud-ubangi', 'tshuapa', 'kasai', 'kasai-central', 
                 'kasai-oriental', 'lomami', 'sankuru', 'haut-katanga', 'haut-lomami', 'lualaba', 'tanganyika', 'kinshasa', 'maniema', 'nord-kivu', 'sud-kivu', 'ituri', 'tshopo', 
                 'bas-uele', 'haut-uele')
  
  # clean DPS names
  dt$dps <- gsub(" ", "-", dt$dps)
  dt$dps <- gsub("--", "-", dt$dps)
  
  dt$dps <- chartr(paste(names(unwanted_array), collapse=''),
                   paste(unwanted_array, collapse=''),
                   dt$dps)

  dt <- dt[dps !="EQUATEUR"]
  dt <- dt[dps !="KASAI-ORIENTAL"]
  
  dt$dps <- tolower(dt$dps)
  
  dt[ dps == 'kasai-centre', dps:= 'kasai-central']
  
  dt <- dt[dps %in% dps_names]
  
  return(dt)
}

add_data_sheet_info <- function(dt, dt_sheets){
  # add columns for quarter, year, and TB type
  s <- trimws(s)
  dt[, sheet:= s]
  dt[, quarter:= dt_sheets[sheet_name==s, quarter]]
  if (unique(dt_sheets$sheet_type)!="DEP"){
    dt[, TB_type := dt_sheets[sheet_name==s, TB_type]]
  }
  dt[, data_year := dt_sheets[sheet_name==s, year]]
  dt[, file_year := year]
  
  return(dt)
}

clean_eval_sheets <- function(dir, year, file){
    i <- 1
    for (s in sheets_eval[1:length(sheets_eval)]){
      
      dt <- data.table(read_excel(paste0(dir, year, "/", file), sheet= s))
      
      dt <- initial_clean(dt)
      
      # remove columns of percentages
      cols <- !is.na( dt[1,] )
      cols <- colnames(dt)[cols]
      
      dt <- dt[, cols, with=FALSE]
      
      # set column names to be header row:
      colnames(dt) <- as.character(dt[1,])
      
      # remove header row in row 1
      dt <- dt[-1, ]
      
      ##----------------------------------
      # clean column names:
      colnames(dt) <- tolower(colnames(dt))
      
      setnames(dt, grep('cplt', colnames(dt)), 'dps')
      setnames(dt, grep('enreg', colnames(dt)), 'tot_cas_reg')
      
      for(n in c('guer')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'healed')
      if(!'healed' %in% names(dt)) print(paste0('In sheet, ', s, ', healed is not a column'))
      
      setnames(dt, grep('traitement termine', colnames(dt)), 'trt_complete')
      
      for(n in c('dece','dcd')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'died')
      if(!'died' %in% names(dt)) stop(paste0('In sheet, ', s, ', died is not a column'))
      
      for(n in c('echecs')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'trt_failed')
      if(!'trt_failed' %in% names(dt)) print(paste0('In sheet, ', s, ', trt_failed is not a column'))
      
      for(n in c('perdu','abandon', 'interruptions')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'lost_to_followup')
      if(!'lost_to_followup' %in% names(dt)) stop(paste0('In sheet, ', s, ', lost_to_followup is not a column'))
      
      setnames(dt, grep('transfer', colnames(dt)), 'transferred')
      
      for(n in c('total  evalue','total evalue', 'total cas evalues')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'cas_eval')
      if(!'cas_eval' %in% names(dt)) stop(paste0('In sheet, ', s, ', cas_eval is not a column'))
      
      for(n in c('non evalue')) if(any(grepl(n, names(dt)))) setnames(dt, grep(n, names(dt)), 'cas_not_eval')
      if(!'cas_not_eval' %in% names(dt)) stop(paste0('In sheet, ', s, ', cas_not_eval is not a column'))
      
      dt <- clean_dps_names(dt)
      dt <- add_data_sheet_info(dt)
      
      if (i==1){
        # if it's the first sheet, initialize the new dt
        outcomes <- dt
        # for subsequent sheets, rbind to that dt
      } else {
        outcomes <- rbindlist(list(outcomes, dt), use.names=TRUE, fill= TRUE)
      }
      # print(s)
      i <- i + 1
    }
    return(outcomes)
}
