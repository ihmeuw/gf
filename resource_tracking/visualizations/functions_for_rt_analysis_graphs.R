# ----------------------------------------------
# Irena Chen
#
# 1/11/2017
# ### Run these functions for the resource_tracking_analysis_graph.R script:  

# Set up R
rm(list=ls())
library(ggplot2)
library(dplyr)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(readxl)
library(reshape)
library(scales)

# ---------------------------------------------
##function to create indicator between current, upcoming, and rejected budgets: 
appr_rej_indicators <- function(year, data_source){
  if(year < 2018){
    x <- "Past/Active"
  } else if (data_source=="rej_fpm"){
    x <- "In Iteration"
  } else if (data_source=="init_fpm"){
    x <- "Initial"
  }else {
    x <- "Upcoming"
  }
  return(x)
}

# ---------------------------------------------
##make disease names look nicer on graphs: 
disease_names_for_plots <- function(gos_data){
  
  graphData <- copy(gos_data)
  graphData[disease=='hiv', disease:='HIV/AIDS']
  graphData[disease=='malaria', disease:='Malaria']
  graphData[disease=='tb', disease:='Tuberculosis']
  graphData[disease=='hss', disease:='HSS']
  return(graphData)
}


# ---------------------------------------------
##colors to map for non HSS disease categories (Malaria, TB, HIV): 
primColors <- c('#b20000', '#660000', ##reds
                '#f6ae8f', '#EE5D1F', ##oranges
                '#3DCC3D', '#008000',##greens
                '#004c4c', '#00D9D9',##blues
                "#35978f", "#80cdc1", ##teals
                '#3786b0', '#0097f1',##ocean 
                '#ffd700', '#d3a308',#yellows
                '#3d017a','#b200b2', ##magenta
                '#660066', '#bf7fbf',#purples
                '#ff748c', "#e00222", #pinks 
                '#4169e1', '#00007f',##royal and midnight blues
                '#c0c0c0', '#ffc0cb') ##grey and millennial pink 


names(primColors) <- c('HIV/AIDS care and support', 'Community care and outreach', ##reds
                       'Case detection and diagnosis', 'Case Diagnosis',##oranges
                       'Treatment','HIV/AIDS counseling and testing',##greens
                       'RSSH: health workforce', 'RSSH: information system',##blues
                       'RSSH: service delivery', 'RSSH: other', ##teals
                       'Key and vulnerable populations', 'Community care and outreach',##ocean
                       'Prevention', 'Malaria other control and prevention', ##yellows
                       'MDR-TB case detection and diagnosis', 'MDR-TB prevention',   ##magenta
                       'HIV/TB collaborative interventions', 'MDR-TB treatment', ##purples
                       'Monitoring and evaluation', 'Malaria bed nets', #pinks
                       'PBF', 'Malaria indoor residual spraying' ##royal and midnight blues
                       ,'Other/Unidentified', 'HIV/AIDS PMTCT') ##grey and millennial pink 

# ---------------------------------------------
######create hss indicator: 
get_hss_ind <- function(dataset, program_activity){
  dataset$hss_ind <-factor(sapply(dataset$program_activity, function(x){
    if(grepl("RSSH", x)){
      x <- as.character(x)
    } else{
      x <- "Non RSSH"
    }
  }), levels=c('Non RSSH', 'RSSH: information system', 'RSSH: health workforce',
               'RSSH: service delivery','RSSH: other')
  )
  return(dataset)
}
# ---------------------------------------------
### create a key populations indicator: 
get_keypop_ind <- function(dataset, program_activity){
  dataset$key_pop <- factor(sapply(dataset$program_activity, function(x){
    if(x=="Key and vulnerable populations"){
      x <- "Key and vulnerable populations"
    } else if(x=="HIV/TB collaborative interventions") {
      x <- "HIV/TB collaborative interventions"
    } else{
      x <- "Other categories"
    }
  }), levels=c("Other categories","Key and vulnerable populations", "HIV/TB collaborative interventions"))
  return(dataset)
}

