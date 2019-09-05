###
### ESPEN API DATA / MAPS CALLS
###
### https://admin.espen.afro.who.int/docs/api ### ----
### Elex Hill
### Only works locally for now

library(httr)
library(jsonlite)
library(lubridate)
library(sf)
library(geojsonsf)
library(geojson)
library(data.table)
#urlC <- paste0(url, "api/cartographies") # odd
#urlJ <- paste0(url, "api/japs") # odd

#Review lines 32-107! EL 9/4/19


###################################################################################################
### ESPEN API DATA CALL ###########################################################################
#
### FUNCTION ###
# Interacts with ESPEN API to pull specific data for the user
# Currently saves data to two locations with the current date :
#   I:/Geospatial/Big5/Focal 3/03_Data/Oncho/Oncho_Base_Data/ESPEN_API_Data/
#   J:/DATA/Data Incoming/WHO_ESPEN/*disease*/*date*/ALL/
#
### PARAMETERS ###
#'@disease - which disease to pull data on { lf | oncho | loa | sch | sth | trachoma }
#'@level - which geographic size level to pull data on. You'll usually want iu { iu | sitelevel }

espenAPIData <- function(disease, level) {

  ### Base inputs ### ----
  options(stringsAsFactors = F)
  diseases <- c("lf", "oncho", "loa", "sch", "sth", "trachoma")
  levels <- c("sitelevel", "iu")
  urlBase <- "https://admin.espen.afro.who.int/api/data?"
  num <- 1
  iso <- pullISO()
  dirs <- pullDir(disease)
  outDir <- dirs[[1]]
  incDir <- dirs[[2]]

  ### Test user inputs ### ----
  if (disease %in% diseases == F) { stop(paste0(disease, " is not in the disease list"))}
  if (level %in% levels == F) { stop(paste0(disease, " is not in the level list"))}

  ### Create API URL ### ----
  url <- paste0(urlBase, "disease=", disease, "&level=", level)

  ### Loop through iso3s pulling each data table ### ----
  for (i in 1:nrow(iso)) {

    i <- iso[i]
    message(paste0("Pulling Data for : ", i$ADMIN0ISO3))

    # Update URL for current iso2
    currURL <- paste0(url, "&iso2=", i$ADMIN0ISO2)

    # Pull data and translate it to a data table
    initPull <- GET(currURL)
    if (initPull$status_code != 200) { stop(paste0("Status Code Does Not Equal 200: ", i$ADMIN0ISO3, " ", currURL))}
    charPull <- rawToChar(initPull$content)
    isoData <- as.data.table(fromJSON(charPull))

    # Make sure something is in the file
    if (nrow(isoData) > 0 ) {

      ### Save country data to incoming folder ### ----
      if (file.exists(paste0(incDir, "/", i$ADMIN0ISO3)) == F) {
        dir.create(paste0(incDir, "/", i$ADMIN0ISO3))
      }
      fwrite(isoData, paste0(incDir, "/", i$ADMIN0ISO3, "/DATA_", i$ADMIN0ISO3, "_", disease, "_", level, ".csv"))

      ### Processing the end ### ----
      if (num == 1) {
        data <- isoData
        num <- num + 1
      } else {
        data <- rbindlist(list(data, isoData), fill = T)
      }
    } else {
      message(paste0("...No ", level, " data for ", i$ADMIN0ISO3))
    }
  }


  message("SAVING ALL")
  ### Save Africa Data Incoming ### ----

  if (file.exists(paste0(incDir, "/ALL")) == F) {
    dir.create(paste0(incDir, "/ALL"))
  }
  fwrite(data, paste0(incDir, "/ALL/DATA_ALL_", disease, "_", level, ".csv"))

  ### Any extra processing or formatting? ### ----

  ### Need add data check for differences

  ### Saving Full Data ### ----
  if (outDir != "None") {
    fwrite(data, paste0(outDir, "/ESPEN_Data_", disease, "_", level, "_", Sys.Date(), ".csv"))
    message(paste0("Data Saved Here : ", outDir, "/ESPEN_Data_", disease, "_", level, "_", Sys.Date(), ".csv"))
  }
  # options(stringsAsFactors = T)
}


### ESPEN API MAPS CALL ###########################################################################
#
### FUNCTION ###
# Interacts with ESPEN API to pull specific geojson data for the user
# Currently saves data to two locations with the current date :
#   I:/Geospatial/Big5/Focal 3/03_Data/Oncho/Oncho_Base_Data/ESPEN_API_Data/
#   J:/DATA/Data Incoming/WHO_ESPEN/*disease*/*date*/ALL/
#
# ONLY CREATES SHAPEFILE WITH ENDEMICITY FILE
#
### PARAMETERS ###
#'@disease - which disease to pull data on { lf | oncho | loa | sch | sth | trachoma }
#'@level - which geographic size level to pull data on. You'll usually want iu { iu | sitelevel }
#'@type -
#'@subtype -
#'

espenAPIMap <- function(disease, level, type, subtype = "nosubtype") {

  ### Base inputs ### ----
  options(stringsAsFactors = F)
  diseases <- c("lf", "oncho", "loa", "sch", "sth", "trachoma")
  levels <- c("sitelevel", "iu")
  urlBase <- "https://admin.espen.afro.who.int/api/maps?"
  ### LF Possible Inputs
  lfSLtype <- c("tas", "sentinel_sites", "mapping_surveys")
  lfIUtype <- c("endemicity", "mda_pc_rounds")
  lfIUMDAsubtype <- c("projections", "therapeutic", "geographic")
  ### Oncho Possible Inputs
  onSLtype <- c("impact_assessment", "mapping_surveys")
  onSLsubtype <- c("skin_biopsy", "anti_ov16_test", "nodule_palpation")
  onIUtype <- c("endemicity", "mda_pc_coverage", "mda_pc_rounds")
  onIUMDAsubtype <- c("geographic", "therapeutic")
  ### Loa Possible Inputs
  loaSLtype <- c("mapping_surveys")
  loaSLsubtype <- c("ewh_questionnaire", "blood_smear")
  loaIUtype <- c("endemicity")
  ### Schisto Possible Inputs
  schSLtype <- c("mapping_surveys")
  schSLsubtype <- c("all_species", "s_haematobium", "s_mansoni")
  schIUtype <- c("endemicity",  "mda_pc_coverage", "mda_pc_rounds")
  schIUMDAsubtype <- c("geographic_sac", "geographic_total", "therapeutic_sac", "therapeutic_total")
  ### STH Possible Inputs
  sthSLtype <- c("mapping_surveys")
  sthSLsubtype <- c("all_species", "ascaris", "hookworms", "trichuris")
  sthIUtype <- c("endemicity",  "mda_pc_coverage", "mda_pc_rounds")
  sthIUMDAsubtype <- c("geographic_sac", "geographic_total", "therapeutic_sac", "therapeutic_total")


  ### Trachoma Possible Inputs
  # currently nothing

  num <- 1
  catV <- c("Non-endemic", "Endemic (MDA not started)", "Endemic (under MDA)", "Unknown (under LF MDA)"
            , "Endemic (post-MDA surveillance)", "Unknown / consider Oncho Elimination mapping")
  iso <- pullISO()

  ### Create Directories for Saving ### ----
  dirs <- pullDir(disease)
  outDir <- dirs[[1]]
  incDir <- dirs[[2]]

  ### Test user inputs ### ----
  if (disease %in% diseases == F) { stop(paste0(disease, " is not in the disease list"))}
  if (level %in% levels == F) { stop(paste0(disease, " is not in the level list"))}
  ### Oncho Input Checks
  if (disease == "oncho" & level == "sitelevel") {
    if (type %in% onSLtype == F) { stop(paste0("The type ", type , " is not in the Oncho Site Level Type list: ", paste(onSLtype, collapse= ", ")))}
    if (subtype %in% onSLsubtype == F) { stop(paste0("The subtype", subtype, " is not in the Oncho Site Level Subtype list: ", paste(onSLsubtype, collapse= ", ")))}
  }
  if (disease == "oncho" & level == "iu") {
    if (type %in% onIUtype == F) { stop(paste0("The type ", type, " is not in the Oncho IU Type list: ", paste(onIUtype, collapse= ", ")))}
    if (grepl("mda", type)) {
      if (subtype %in% onIUMDAsubtype == F) { stop(paste0("The subtype ", subtype, " is not in the Oncho IU MDA/PC Coverage and Rounds Subtype list: ", paste(onIUMDAsubtype, collapse= ", ")))}
    }
  }
  ### LF Input Checks
  if (disease == "lf" & level == "sitelevel") {
    if (type %in% lfSLtype == F) { stop(paste0("The type ", type , " is not in the LF Site Level Type list: ", paste(lfSLtype, collapse= ", ")))}
  }
  if (disease == "lf" & level == "iu") {
    if (type %in% lfIUtype == F) { stop(paste0("The type ", type, " is not in the LF IU Type list: ", paste(lfIUtype, collapse= ", ")))}
    if (grepl("mda", type)) {
      if (subtype %in% lfIUMDAsubtype == F) { stop(paste0("The subtype ", subtype, " is not in the LF IU MDA/PC Coverage and Rounds Subtype list: ", paste(lfIUMDAsubtype, collapse= ", ")))}
    }
  }
  ### Loa Input Checks
  if (disease == "loa" & level == "sitelevel") {
    if (type %in% loaSLtype == F) { stop(paste0("The type ", type , " is not in the Loa Site Level Type list: ", paste(loaSLtype, collapse= ", ")))}
    if (subtype %in% loaSLsubtype == F) { stop(paste0("The subtype ", subtype , " is not in the Loa Site Level Subtype list: ", paste(loaSLsubtype, collapse= ", ")))}
  }
  if (disease == "loa" & level == "iu") {
    if (type %in% loaIUtype == F) { stop(paste0("The type ", type, " is not in the Loa IU Type list: ", paste(loaIUtype, collapse= ", ")))}
  }
  ### Schisto Input Checks
  if (disease == "sch" & level == "sitelevel") {
    if (type %in% schSLtype == F) { stop(paste0("The type ", type , " is not in the Schisto Site Level Type list: ", paste(schSLtype, collapse= ", ")))}
    if (subtype %in% schSLsubtype == F) { stop(paste0("The subtype ", subtype , " is not in the Schisto Site Level Subtype list: ", paste(schSLsubtype, collapse= ", ")))}
  }
  if (disease == "sch" & level == "iu") {
    if (type %in% schIUtype == F) { stop(paste0("The type ", type, " is not in the Schisto IU Type list: ", paste(schIUtype, collapse= ", ")))}
    if (grepl("mda", type)) {
      if (subtype %in% schIUMDAsubtype == F) { stop(paste0("The subtype ", subtype, " is not in the Schisto IU MDA/PC Coverage and Rounds Subtype list: ", paste(schIUMDAsubtype, collapse= ", ")))}
    }
  }
  ### STH Input Checks
  if (disease == "sth" & level == "sitelevel") {
    if (type %in% sthSLtype == F) { stop(paste0("The type ", type , " is not in the STH Site Level Type list: ", paste(schSLtype, collapse= ", ")))}
    if (subtype %in% sthSLsubtype == F) { stop(paste0("The subtype ", subtype , " is not in the STH Site Level Subtype list: ", paste(schSLsubtype, collapse= ", ")))}
  }
  if (disease == "sth" & level == "iu") {
    if (type %in% sthIUtype == F) { stop(paste0("The type ", type, " is not in the STH IU Type list: ", paste(schIUtype, collapse= ", ")))}
    if (grepl("mda", type)) {
      if (subtype %in% sthIUMDAsubtype == F) { stop(paste0("The subtype ", subtype, " is not in the STH IU MDA/PC Coverage and Rounds Subtype list: ", paste(schIUMDAsubtype, collapse= ", ")))}
    }
  }

  ### Create Values Vectors ### ----

  # {"categories":[{"name":"Non-endemic","value":0},{"name":"Endemic (MDA not started)","value":1}
  # ,{"name":"Unknown","value":2}],
  if (disease == "oncho") {
    classF <- c("Non-endemic","Endemic (MDA not started)","Endemic (under MDA)","Unknown (under LF MDA)"
                ,"Endemic (post-MDA surveillance)","Unknown / consider Oncho Elimination mapping")
  } else if (disease == "lf") {
    classF <- c("Non-endemic", "Endemic (MDA not started)", "Endemic (post-MDA surveillance)", "Unknown")
  } else if (disease == "sth") {
    classF <- c("< 1%", "1 - 19.9%", "20 - 49.9%", ">= 50%")
  } else if (disease == "sch") {
    classF <- c("< 1%", "1 - 9.9%", "10 - 49.9%", ">= 50%")
  } else if (disease == "loa") {
    classF <- c("Non-endemic","Endemic (MDA not started)","Unknown")
  }

  ### Create API URL ### ----
  url <- paste0(urlBase, "disease=", disease, "&level=", level, "&type=", type)
  if (subtype != "nosubtype") {
    url <- paste0(url, "&subtype=", subtype)
  }


  ### Loop through iso3s pulling each data table ### ----
  for (n in 1:nrow(iso)) {
    i <- iso[n]

    # Testing bit
    # if (i$ADMIN0ISO3 %in% c("ETH")) {i <- iso[18]}

    message(paste0("Pulling Data for : ", i$ADMIN0ISO3, " : ", n, "/", nrow(iso)))

    # Update URL for current iso2
    currURL <- paste0(url, "&iso2=", i$ADMIN0ISO2, "&year_start=2017")

    # currURL <- "https://admin.espen.afro.who.int/api/maps?iso2=ET&disease=oncho&level=iu&type=endemicity&year_start=2017"

    # Pull data and translate it to a data table
    initPull <- GET(currURL)
    if (initPull$status_code != 200) { stop(paste0("Status Code Does Not Equal 200: ", i$ADMIN0ISO3, " - ", initPull$status_code, " - ", currURL))}
    charPull <- rawToChar(initPull$content)

    remove <- "{\"categories\":[{\"name\":\"Non-endemic\",\"value\":0},{\"name\":\"Endemic (MDA not started)\",\"value\":1},{\"name\":\"Endemic (under MDA)\",\"value\":2},{\"name\":\"Unknown (under LF MDA)\",\"value\":3},{\"name\":\"Endemic (post-MDA surveillance)\",\"value\":4},{\"name\":\"Unknown / consider Oncho Elimination mapping\",\"value\":5}],\"geojson\":"

    starting <- regexpr("\"geojson\":", charPull)
    startN <- starting[1] + 10

    # message(substr(charPull, 1, starting[1]))

    gjsonPull <- to_geojson(substr(charPull, startN, nchar(charPull)-1))
    sfPull <- unique(geojson_sf(gjsonPull))


    ### Check if has any data
    if (nrow(sfPull) > 0 & "value" %in% colnames(sfPull)) {

      pullVal <- data.table(value = c(sfPull$value)+1, Endem_MDA = "")
      pullVal[is.na(value), value := 1]

      for (v in 1:length(classF)) {
        pullVal[value == v, Endem_MDA := classF[v]]
      }

      sfPull$Shape_Area <- NULL
      sfPull$Shape_Leng <- NULL
      sfPull$Shape_Le_1 <- NULL
      sfPull$orig_val <- sfPull$value
      sfPull$value <- NULL
      sfPull$Endem_MDA <- pullVal$Endem_MDA

      ### Save Country Level Map Info
      if (file.exists(paste0(incDir, "/", i$ADMIN0ISO3)) == F) {
        dir.create(paste0(incDir, "/", i$ADMIN0ISO3))
      }

      if (type == "endemicity") {


        st_write(sfPull, paste0(incDir, "/", i$ADMIN0ISO3, "/MAP_", i$ADMIN0ISO3, "_",level, "_", type, "_", subtype, ".shp"))

        if (num == 1) {
          data <- sfPull
          num <- num + 1
        } else {
          data <- do.call(rbind, list(data, sfPull))
          # data <- rbindlist(list(data, isoData), fill = T)
        }
        if (nrow(data) == 0) {
          stop(message("Data Is Not Combining"))
        }
      } else { ### Should never enter here
        message("...Entering a non Endemcity call - this section is not tested yet")
        sfData <- as.data.table(sfPull)
        # for (n in 0:5) { sfData[value == n, class := catV[n+1]] }
        # Value column only exists in pulls that have sitelevel data - not just the outlines
        if ("value" %in% colnames(sfData)) {
          # Pull out only sitelevels, no
          sfData <- sfData[!is.na(value)]
          sfData <- sfData[, .(value, geometry)] # All other columns are NA for the sitelevel
          # Separate lat/long out of geometry
          for (row in 1:nrow(sfData)) {
            sfData[row, long := sfData[row]$geometry[[1]][1]]
            sfData[row, lat := sfData[row]$geometry[[1]][2]]
          }
          # Remove geometry column and save country specific
          sfData$geometry <- NULL
          sfData$ISO3 <- i$ADMIN0ISO3
          fwrite(sfData, paste0(incDir, "/", i$ADMIN0ISO3, "/MAP_", i$ADMIN0ISO3, "_",level, "_", type, "_", subtype, ".csv"))

          # Combine data
          if (num == 1) {
            data <- sfData
            num <- num + 1
          } else {
            num <- num
            data <- rbindlist(list(data, sfData), fill = T)
          }
        } else {
          message(paste0("...No Data for ", i$ADMIN0ISO3))
        }
      }

    }
  }

  ### Save Africa Data Incoming ### ----
  message("SAVING ALL")
  if (file.exists(paste0(incDir, "/ALL")) == F) {
    dir.create(paste0(incDir, "/ALL"))
  }

  if (type == "endemicity") {
    st_write(data, paste0(incDir, "/ALL/MAP_ALL_", level, "_", type, "_", subtype, ".shp"))
  }

  # fwrite(data, paste0(incDir, "/ALL/MAP_ALL_", disease, "_", level, "_", type, "_", subtype, ".csv"))


  if (outDir != "None") {

    if (type == "endemicity") {
      st_write(data, paste0(outDir, "/ESPEN_Map_", disease, "_", level, "_", type, "_", subtype, "_", Sys.Date(), ".shp"))
      message(paste0("Data Saved Here : ", outDir, "/ESPEN_Map_", disease, "_", level, "_", type, "_", subtype, "_", Sys.Date(), ".shp"))
    }
    # fwrite(data, paste0(outDir, "/ESPEN_Map_", disease, "_", level, "_", type, "_", subtype, "_", Sys.Date(), ".csv"))
    # message(paste0("Data Saved Here : ", outDir, "/ESPEN_Map_", disease, "_", level, "_", type, "_", subtype, "_", Sys.Date(), ".csv"))
  }


  # options(stringsAsFactors = T)
}

### HELPER FUNCTIONS ###############################################################################

pullISO <- function() {
  if (Sys.info()[1] == "Windows") rootJ <- "J:" else rootJ <- "/home/j"
  allData <- as.data.table(fread(paste0(rootJ, "/temp/hillele/Oncho/ESPEN_ISO3-2.csv")))
  allData[ADMIN0ISO3 == "NAM", ADMIN0ISO2 := "NA"]
  return(allData)
}

pullDir <- function(disease) {

  if (Sys.info()[1] == "Windows") rootJ <- "J:" else rootJ <- "/home/j"
  if (Sys.info()[1] == "Windows") rootI <- "I:" else rootI <- "/home/i" # this might be wrong? /ihme/ ??

  ### Incoming Data Directory
  incDir <- paste0(rootJ, "/DATA/Incoming Data/WHO_ESPEN/")

  ### Output of full combined data
  outDir <- paste0(rootI, "/Geospatial/Big5/Focal 3/03_Data/")

  if (disease == "oncho") {
    outDir <- paste0(outDir, "Oncho/Oncho_Base_Data/ESPEN_API_Data")
    incDir <- paste0(incDir, "Oncho/", Sys.Date())
  } else if (disease == "lf") {
    outDir <- paste0(outDir, "LF/ESPEN_API_Data")
    incDir <- paste0(incDir, "LF/", Sys.Date())
  } else if (disease == "sch") {
    outDir <- paste0(outDir, "Schisto/ESPEN_API_Data")
    incDir <- paste0(incDir, "Schisto/", Sys.Date())
  } else if (disease == "sth") {
    outDir <- "None"
    incDir <- paste0(incDir, "STH/", Sys.Date())
  } else if (disease == "loa") {
    outDir <- "None"
    incDir <- paste0(incDir, "Loiasis/", Sys.Date())
  } else if (disease == "trachoma") {
    outDir <- "None"
    incDir <- paste0(incDir, "trachoma/", Sys.Date())
  }

  if(file.exists(incDir) == F) {
    # make file
    dir.create(incDir)
  }

  return(list(outDir, incDir))
}

### COMPARE FUNCTION ##############################################################################

comparePulls <- function(disease, level, new_date, old_date, type = "notype", subtype = "nosubtype") {

  ### Create Directories ### ----

  if (Sys.info()[1] == "Windows") rootJ <- "J:" else rootJ <- "/home/j"
  if (Sys.info()[1] == "Windows") rootI <- "I:" else rootI <- "/home/i" # this might be wrong? /ihme/ ??

  ### Incoming Data Directory
  incDirN <- paste0(rootJ, "/DATA/Incoming Data/WHO_ESPEN/")
  incDirO <- paste0(rootJ, "/DATA/Incoming Data/WHO_ESPEN/")

  ### Output of full combined data
  outDir <- paste0(rootI, "/Geospatial/Big5/Focal 3/03_Data/")

  if (disease == "oncho") {
    outDir <- paste0(outDir, "Oncho/Oncho_Base_Data/ESPEN_API_Data")
    incDirN <- paste0(incDirN, "Oncho/", new_date)
    incDirO <- paste0(incDirO, "Oncho/", old_date)
  } else if (disease == "lf") {
    outDir <- paste0(outDir, "LF/ESPEN_API_Data")
    incDirN <- paste0(incDirN, "LF/", new_date)
    incDirO <- paste0(incDirO, "LF/", old_date)
  } else if (disease == "sch") {
    outDir <- paste0(outDir, "Schisto/ESPEN_API_Data")
    incDirN <- paste0(incDirN, "Schisto/", new_date)
    incDirO <- paste0(incDirO, "Schisto/", old_date)
  } else if (disease == "sth") {
    outDir <- "None"
    incDirN <- paste0(incDirN, "STH/", new_date)
    incDirO <- paste0(incDirO, "STH/", old_date)
  } else if (disease == "loa") {
    outDir <- "None"
    incDirN <- paste0(incDirN, "Loiasis/", new_date)
    incDirO <- paste0(incDirO, "Loiasis/", old_date)
  } else if (disease == "trachoma") {
    outDir <- "None"
    incDirN <- paste0(incDirN, "trachoma/", new_date)
    incDirO <- paste0(incDirO, "trachoma/", old_date)
  }

  ### Pull Data ### ----

  if (type == "endemicity") {
    # Pulling shapefile
    stop(message("Not built to compare shapefile currently"))
  } else  if (level == "iu") {
    oldData <- fread(paste0(incDirO, "/ALL_DATA_", disease, "_", level, ".csv"))
    newData <- fread(paste0(incDirN, "/ALL_DATA_", disease, "_", level, ".csv"))

    if ((sort(colnames(oldData)) == sort(colnames(newData))) == F) {
      stop(paste0("Column names do not match up, need do manual comparison check"))
    }

    if (nrows(oldData) > nrows(newData)) {
      message(paste0("There are ", nrows(oldData)-nrows(newData), " less rows in the new data set"))
    }
    if (nrows(oldData) < nrows(newData)) {
      message(paste0("There are ", nrows(newData)-nrows(oldData), " more rows in the new data set"))
    }
    if (nrows(oldData) == nrows(newData)) {
      message("...Same number of rows in both datasets")
    }

    ### Check for Differences ### ----

    comData <- unique(rbindlist(list(newData, oldData)))

    diffID <- comData[, .(nRows = .N), by = list(IU_ID)]
    diffID <- diffID[nRows > 1]

    diffID <- unique(diffID$IU_ID)
    message(paste0("There are ", length(diffID), " IUs that have changed"))

    ### Pull ID for new rows or lost rows ### ----

    newData$changed <- "new"
    oldData$changed <- "old"

    comData <- rbindlist(list(newData, oldData))

    updID <- comData[, .(nRows = .N), by = list(IU_ID)]
    updID <- updID[nRows == 1]$IU_ID

    ### Pull all rows that are new/old/changed ### ----

    diffData <- comData[IU_ID %in% updID | IU_ID %in% diffID, ]

  } else if (level == "sitelevel") {

    oldData <- fread(paste0(incDirO, "/ALL_DATA_", disease, "_", level, ".csv"))
    newData <- fread(paste0(incDirN, "/ALL_DATA_", disease, "_", level, ".csv"))

    if ((sort(colnames(oldData)) == sort(colnames(newData))) == F) {
      stop(paste0("Column names do not match up, need do manual comparison check"))
    }

    if (nrows(oldData) > nrows(newData)) {
      message(paste0("There are ", nrows(oldData)-nrows(newData), " less rows in the new data set"))
    }
    if (nrows(oldData) < nrows(newData)) {
      message(paste0("There are ", nrows(newData)-nrows(oldData), " more rows in the new data set"))
    }
    if (nrows(oldData) == nrows(newData)) {
      message("...Same number of rows in both datasets")
    }

    ### Check for Differences ### ----

    comData <- unique(rbindlist(list(newData, oldData)))

    diffLL <- comData[, .(nRows = .N), by = list(Latitude, Longitude, Diagnostic)]
    diffLL <- diffLL[nRows > 1]

    diffLL <- unique(diffLL[nRows == 1, .(Latitude, Longitude, Diagnostic)])
    message(paste0("There are ", nrow(diffLL), " Sites that have changed"))

    ### Pull ID for new rows or lost rows ### ----

    newData$changed <- "new"
    oldData$changed <- "old"

    comData <- rbindlist(list(newData, oldData))

    updLL <- comData[, .(nRows = .N), by = list(Latitude, Longitude, Diagnostic)]
    updLL <- unique(updLL[nRows == 1, .(Latitude, Longitude, Diagnostic)])

    ### Pull all rows that are new/old/changed ### ----

    pullLL <- rbindlist(list(updLL, diffLL))

    for (n in 1:nrow(pullLL)) {
      currR <- pullLL[n]
      pullR <- comData[Latitude == currR$Latitude & Longitude == currR$Longitude
                       & Diagnostic == currR$Diagnostic, ]
      if (n == 1) {
        diffData <- pullR
      } else {
        diffData <- rbindlist(list(diffData, pullR))
      }
    }
  }


  # Actually want to do all of them as csv because easier

  if (type == "endemicity") {
    # Save shapefile
    fwrite(diffData, paste0(incDirN, "ALL_MAP_", disease, "_", type, "_DIFFw_", old_date, ".csv"))
  } else {
    # Saving csv
    fwrite(diffData, paste0(incDirN, "ALL_DATA_", disease, "_", level, "_DIFFw_", old_date, ".csv"))
  }



}

###################################################################################################
### CALLS THEMSELVES ##############################################################################
###################################################################################################


### ALL DATA CALLS ### ----
espenAPIData("oncho", "sitelevel")
espenAPIData("oncho", "iu")
espenAPIData("lf", "sitelevel")
espenAPIData("lf", "iu")
espenAPIData("loa", "sitelevel")
espenAPIData("loa", "iu")
espenAPIData("sch", "sitelevel")
espenAPIData("sch", "iu")
espenAPIData("sth", "sitelevel")
espenAPIData("sth", "iu")
# espenAPIData("trachoma", "sitelevel")
# espenAPIData("trachoma", "iu")

### ALL MAP CALLS ### ----

espenAPIMap("oncho", "iu", "endemicity")

espenAPIMap("loa", "iu", "endemicity")
espenAPIMap("sch", "iu", "endemicity")
espenAPIMap("sth", "iu", "endemicity")

espenAPIMap("lf", "iu", "endemicity") # LF Hates COD and ETH and BFA??? and NGA

# espenAPIMap("lf", "sitelevel", "tas")
# espenAPIMap("lf", "sitelevel", "sentinel_sites")
# espenAPIMap("lf", "sitelevel", "mapping_surveys")
# espenAPIMap("lf", "iu", "mda_pc_rounds", "projections")
# espenAPIMap("lf", "iu", "mda_pc_rounds", "therapeutic")
# espenAPIMap("lf", "iu", "mda_pc_rounds", "geographic")

# espenAPIMap("oncho", "sitelevel", "impact_assessment", "skin_biopsy")
# espenAPIMap("oncho", "sitelevel", "impact_assessment", "anti_ov16_test")
# espenAPIMap("oncho", "sitelevel", "impact_assessment", "nodule_palpation")
# espenAPIMap("oncho", "sitelevel", "mapping_surveys", "skin_biopsy")
# espenAPIMap("oncho", "sitelevel", "mapping_surveys", "anti_ov16_test")
# espenAPIMap("oncho", "sitelevel", "mapping_surveys", "nodule_palpation")
# espenAPIMap("oncho", "iu", "mda_pc_coverage", "geographic")
# espenAPIMap("oncho", "iu", "mda_pc_rounds", "geographic")
# espenAPIMap("oncho", "iu", "mda_pc_coverage", "therapeutic")
# espenAPIMap("oncho", "iu", "mda_pc_rounds", "therapeutic")

# espenAPIMap("loa", "sitelevel", "mapping_surveys", "ewh_questionnaire")
# espenAPIMap("loa", "sitelevel", "mapping_surveys", "blood_smear")

# espenAPIMap("sch", "sitelevel", "mapping_surveys", "all_species")
# espenAPIMap("sch", "sitelevel", "mapping_surveys", "s_haematobium")
# espenAPIMap("sch", "sitelevel", "mapping_surveys", "s_mansoni")
# espenAPIMap("sch", "iu", "mda_pc_coverage", "geographic_sac")
# espenAPIMap("sch", "iu", "mda_pc_rounds", "geographic_sac")
# espenAPIMap("sch", "iu", "mda_pc_coverage", "therapeutic_sac")
# espenAPIMap("sch", "iu", "mda_pc_rounds", "therapeutic_sac")
# espenAPIMap("sch", "iu", "mda_pc_coverage", "geographic_total")
# espenAPIMap("sch", "iu", "mda_pc_rounds", "geographic_total")
# espenAPIMap("sch", "iu", "mda_pc_coverage", "therapeutic_total")
# espenAPIMap("sch", "iu", "mda_pc_rounds", "therapeutic_total")

# espenAPIMap("sth", "sitelevel", "mapping_surveys", "all_species")
# espenAPIMap("sth", "sitelevel", "mapping_surveys", "ascaris")
# espenAPIMap("sth", "sitelevel", "mapping_surveys", "hookworms")
# espenAPIMap("sth", "sitelevel", "mapping_surveys", "trichuris")
# espenAPIMap("sth", "iu", "mda_pc_coverage", "geographic_sac")
# espenAPIMap("sth", "iu", "mda_pc_rounds", "geographic_sac")
# espenAPIMap("sth", "iu", "mda_pc_coverage", "therapeutic_sac")
# espenAPIMap("sth", "iu", "mda_pc_rounds", "therapeutic_sac")
# espenAPIMap("sth", "iu", "mda_pc_coverage", "geographic_total")
# espenAPIMap("sth", "iu", "mda_pc_rounds", "geographic_total")
# espenAPIMap("sth", "iu", "mda_pc_coverage", "therapeutic_total")
# espenAPIMap("sth", "iu", "mda_pc_rounds", "therapeutic_total")



