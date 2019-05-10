# ----------------------------------------------
# Audrey Batzel
# Prep PNLT data - example code

# NOTE: for the code to work, organize files like this:
  # 1.	Create a folder for all of the TB data  example: “National TB Program”
  # 2.	Within this folder, create a folder for each year of data. 
  # 3.	Within each year folder, include all of the files for that year (for all DPS and all quarters).
# ----------------------------------------------

# --------------------
## Set up R / install packages
# -------------------
rm(list=ls())
library(data.table)  
library(reshape2)
library(stringr)
library(readxl)
library(tidyr)
library(dplyr)
library(openxlsx)
# NOTE: if a package is not installed, run install.packages("package.name"); then, rerun library(package.name) above. 
# --------------------

# ----------------------------------------------
## Overview - Files and Directories
# ----------------------------------------------
# directories
dir = "J:/Project/Evaluation/GF/outcome_measurement/cod/National_TB_Program/"  # where the TB files are stored 
out_dir = "" # where you want to save the data

# output files
outFile = paste0(out_dir, "") # name of the output file 
# ----------------------------------------------

# ----------------------------------------------
# Prep each type of sheet / data (“DEP”, “EVAL”, “AGE SEX”, “SUIVI BACT”) separately
# NOTE: this code is set up to prep "DEP" sheets as an example.
sheet_type = "DEP"
# ----------------------------------------------

# ----------------------------------------------
# LOOP THROUGH YEARS
years = list.files(dir)

# Initialize a data table for the entire time series to add the prepped years to as you loop through years
dt_all_years = data.table()
for ( year in years ){  # this is in a loop to demonstrate how it might be done all together, but you may need to step through the loop
                        # year by year and file by file at first, since each file might have specific differences that need to be addressed. 
                        # You can do that by setting y = years[1] manually, as an example.
  
  # Create a list of files for the year 
  files = list.files(paste0(dir, year))
  # Exclude "synthesis" files, keep only the DPS files
  files = files[!grepl(files, pattern = "synth", ignore.case = TRUE)]

  # Create a data table of file information
  dt_files <- as.data.table(files)
  setnames(dt_files, "files", "file_name")
  dt_files[, file_name := trimws(file_name)]
  
  dt_files[, dps := file_name]
  dt_files[, year := lapply( strsplit(file_name, " "), tail, 1)] 
  dt_files[, dps := gsub(paste0(" ", year), "", dps), by = file_name]
  dt_files[, year := gsub(".xlsx", "", year)]
  dt_files[, quarter:= lapply(strsplit(dps, " "), tail, 1)] 
  dt_files[, dps := gsub(paste0(" ", quarter), "", dps), by = file_name]
  dt_files[, quarter := gsub("T", "", quarter)]
  dt_files[, quarter := as.numeric(quarter)]
  
  # For each DPS, take the latest quarter (should be T3 or T4 most commonly):
  dt_files[, max_quarter := max(quarter), by= "dps" ] # will need to make sure there aren't typos in DPS name.
  loop_files = dt_files[max_quarter == quarter, ]
  
  # Initialize a data table for the year to add the prepped DPS to as you loop through DPS
  dt_by_year = data.table()
  # LOOP THROUGH DPS, using the "latest quarter" files for each
  for (file in loop_files$file_name){ # this is in a loop to demonstrate how it might be done all together, but you may need to step through the loop
                                   # year by year and file by file at first, since each file might have specific differences that need to be addressed. 
                                   # You can do that by setting file = loop_files$file_name[1] manually, as an example.
    
    max = loop_files[ file_name == file, max_quarter]
    
    # Get the sheet names for the file
    sheets = excel_sheets(paste0(dir, year, "/", file))
    sheets = sheets[grepl( sheets, pattern = sheet_type, ignore.case = TRUE)]
    sheets = sheets[!grepl( sheets, pattern = "synth", ignore.case = TRUE)]
    sheets = trimws(sheets)
    
    # Create a data table with the sheet information
    dt_sheets <- as.data.table(sheets)
    setnames(dt_sheets, "sheets", "sheet_name")
    
    dt_sheets[, year:= lapply( strsplit(sheet_name, " "), tail, 1)] 
    dt_sheets[, sheet_type := sheet_name]
    dt_sheets[, sheet_type := gsub(paste0(" ", year), "", sheet_type), by = sheet_name]
    dt_sheets[, quarter :=  lapply( strsplit(sheet_type, " "), tail, 1)] 
    dt_sheets[, sheet_type := gsub(paste0(" ", quarter), "", sheet_type), by = sheet_name]
    dt_sheets[, quarter := gsub("T", "", quarter)]
    dt_sheets[, quarter := as.numeric(quarter)]
    
    # Get the sheets to loop through:
    loop_sheets = dt_sheets[ quarter <= max, ]
    
    # Initialize a data table for the DPS to add the prepped data sheets to as you loop through sheets
    dt_by_dps = data.table()
    # LOOP THROUGH SHEETS, up to max_quarter, prep each data sheet, and add it to dt_by_dps
    for (sheet in loop_sheets$sheet_name){# this is in a loop to demonstrate how it might be done all together, but you may need to step through the loop
                                          # year by year and file by file at first, since each file might have specific differences that need to be addressed. 
                                          # You can do that by setting sheet = loop_sheets$sheet_name[1] manually, as an example.
      
      # read in the file and the sheet
      dt <- data.table(read_excel(paste0(dir, year, "/", file), sheet= sheet))
      
      # prep the data
          # //////////////////////////////////// 
          ####### INSERT PREP CODE HERE ########
          # //////////////////////////////////// 
      
      # add columns to the data from the data tables you made to get file and sheet information
      dt$quarter = dt_sheets[sheet_name == sheet, quarter]
      dt$dps = dt_files[file_name == file, dps]
      dt$year = dt_files[file_name == file, year]
      
      # add the data to the data table for the dps
      dt_by_dps = rbindlist(list(dt_by_dps, dt), use.names=TRUE, fill= TRUE)
    }
    # when all of the sheets in one DPS are prepped, add that DPS data to the data table for the year data
    dt_by_year = rbindlist(list(dt_by_year, dt_by_dps), use.names=TRUE, fill= TRUE)
    
    # if you want to save the data by year as an intermediate step, do so here!
    saveRDS(dt_by_year, "") # add where to save the file
    
  }
  # when all of the DPS in one year are prepped, add that year's data to the data table for the whole time series
  dt_all_years = rbindlist(list(dt_all_years, dt_by_year), use.names=TRUE, fill= TRUE)
}
# ----------------------------------------------

# ----------------------------------------------
# Save the data
# ----------------------------------------------
saveRDS(dt_all_years, outFile)
# ----------------------------------------------