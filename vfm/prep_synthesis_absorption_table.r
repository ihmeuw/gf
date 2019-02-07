#---------------------------------------------
# title: "Synthesis Report"
# author: "Emily Linebarger"
# date: "November 26, 2018"
# output: J:/Project/Evaluation/GF/vfm/outputs/synthesis_absorption_table.csv
#---------------------------------------------

rm(list=ls())
library(data.table)
library(ggplot2)
library(googlesheets)
library(doBy)
library(Hmisc)

outFile = 'J:/Project/Evaluation/GF/vfm/outputs/synthesis_absorption_table.csv'

#---------------------------------------------
# Load data from google sheets
#---------------------------------------------

# gs_gap() %>%
#   gs_copy(to = "MyDrive")
consortia_data <- gs_title("Absorption Table")
malaria <- gs_read(consortia_data, ws = "Malaria")
hiv <- gs_read(consortia_data, ws = "HIV")
tb <- gs_read(consortia_data, ws = "TB")
hivtb <- gs_read(consortia_data, ws = "HIV-TB")

#--------------------------------------
# Manually correct data entry errors 
#--------------------------------------

#HIV
hiv[1, 14] <- "MOZ_GRANT 1 (Q1-Q2 2018)"
hiv[1, 23] <- "SEN_GRANT 1 (Q1-Q2 2018) ONLY FOR ANCS (CIVIL SOCIETY)"
hiv[1, 26] <- "SEN_GRANT 2 (Q1-Q2 2018) MOH"

#TB
tb[1, 11] <- "MOZ_GRANT 1 (Q1-Q2 2018)"
tb[1, 20] <- "SEN_GRANT 1 (Q1-Q2 2018)"
tb[1, 23] <- "SDN-T-UNDP"

#Malaria
malaria[1,14] <- "KHM-QSE-M-UNOPS"
malaria[1,23] <- "MMR-QSE-M-UNOPS"
malaria[1, 26] <- "SEN_GRANT 1 (Q1-Q2 2018)"
malaria[1, 29] <- "SDN-M-MOH"

diseases <- list(malaria, hiv, tb)
disease_strings <- c("malaria", "hiv", "tb")

#-----------------------------------------------------------
# Prepare prepped file that can be used for all calculations
#-----------------------------------------------------------

prep_data = function(df, col_index) {
  df <- df[c(1,col_index:(col_index+2))]
  df$grant <- df$budget[1]
  df = df[3:nrow(df), ]
}

for (i in 1:length(diseases)){
  names(diseases[[i]])[2:ncol(diseases[[i]])] <- rep(c("budget", "absorption_q1_2018", "historical_absorption"), ncol(diseases[[i]])/3)
  
  grant_indices <- seq(2,ncol(diseases[[i]]),3)

  for (index in grant_indices){
    df <- prep_data(diseases[[i]], index)
    setDT(df)
    df$disease = disease_strings[i]
    if (index == 2 & i == 1) {
      prepped <- df 
    } else {
      prepped = rbind(prepped, df)  
          
    }
    print(index)
  }
}
names(prepped)[1] <- "gf_module"
#Format as numeric 
for (col in 2:4){
    prepped[, col] <- gsub("[^0-9\\.]", "", prepped[[col]])
    prepped[, col] <- as.numeric(prepped[[col]])
 }

prepped[is.na(prepped)] <- 0
prepped$country <- substring(prepped$grant, 1, 3)
prepped$gf_module = ifelse(prepped$gf_module == "Program Management", "Program management", prepped$gf_module)
prepped$gf_module = ifelse(prepped$gf_module == "TB/HIV (Labelled as HIV)" | prepped$gf_module == "TB/HIV (Labelled as TB)", "TB/HIV", prepped$gf_module)

# save
write.csv(prepped, outFile, row.names=TRUE)
