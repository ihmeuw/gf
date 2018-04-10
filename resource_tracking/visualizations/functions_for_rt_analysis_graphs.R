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
uga_data_sources <- function(year, data_source){
  if(year < 2018){
    x <- "Past/Active"
  } else if (data_source=="rej_fpm"){
    x <- "In Iteration"
  } else if (grepl("init_fpm", data_source)){
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
  graphData[disease=='hss', disease:='RSSH']
  graphData$disease <- factor(graphData$disease, levels=c("HIV/AIDS", "Malaria", "Tuberculosis", "RSSH"))
  return(graphData)
}
# ---------------------------------------------
##make data source names nicer for graphs
data_source_names_for_plots <- function(gos_data){
  
  graphData <- copy(gos_data)
  graphData[data_source=='sicoin',data_source:='SICOIN']
  graphData[data_source=='fpm',data_source:='Final FPM Budgets']
  graphData[data_source=='init_fpm',data_source:='Initial FPM Budgets']
  graphData[data_source=='init2_fpm',data_source:='2nd Iter. FPM Budgets']
  graphData[data_source=='gos_expend',data_source:='GOS Expenditures']
  graphData[data_source=='gos_budget',data_source:='GOS Budgets']
  graphData[data_source=='fgh',data_source:='Financing Global Health']
  graphData[data_source=='pudr',data_source:='Progress Update Disb. Request']
  graphData$data_source<- factor(graphData$data_source, levels=c("SICOIN", "Progress Update Disb. Request",
                                                          "Financing Global Health","Initial FPM Budgets","2nd Iter. FPM Budgets",
                                                          "Final FPM Budgets", "GOS Budgets", "GOS Expenditures"))
  return(graphData)
}


# ---------------------------------------------
##colors to map for non HSS disease categories (Malaria, TB, HIV): 
primColors <- c('#b20000', '#660000', ##reds
                '#4169e1', '#00007f',##royal and midnight blue
                '#004c4c', '#00D9D9',##blues
                "#35978f", "#80cdc1", ##teals
                '#3786b0', '#0097f1',##ocean 
               '#f6ae8f', '#EE5D1F', 
               '#00bf97', '#bf9700',
                '#ffd700', '#d3a308',#yellows
                '#3d017a','#e8946d', ##magenta
                '#9900cc','#00b8bf',
                '#660066', '#bf7fbf',#purples
                '#3DCC3D', '#008000',##greens 
                '#05ff40','#54489e',
                '#58fff8', '#005060',
               '#ff748c', "#a9e89b", #pinks 
                '#bf3700') ##grey and millennial pink 


names(primColors) <- c('Treatment, care and support', 'HIV Testing Services', ##reds
                       'Comprehensive prevention programs for men who have sex with men', 'Prevention programs for general population',##oranges
                       'Comprehensive prevention programs for sex workers and their clients','Comprehensive prevention programs for people who inject drugs and their partners',##greens
                       'Comprehensive prevention programs for transgender people', 'Prevention programs for adolescents and youth, in and out of school',##blues
                       'Programs to reduce human rights-related barriers to HIV services', 'Prevention programs for other vulnerable populations', ##teals
                       "Comprehensive programs for people in prisons and other closed settings",'Case management', ##ocean
                       'Vector control', 'Specific prevention interventions', ##yellows  ##magenta
                       'TB/HIV','TB care and prevention', 'Multidrug-resistant TB' ,
                       'Prevention of mother-to-child transmission',##grey and millennial pink
                       'Procurement and supply chain management systems', 'Health management information system and M&E',
                       'Human resources for health, including community health workers', 'Integrated service delivery and quality improvement'
                       ,'Financial management systems', 'National health strategies',
                       'Community responses and systems', 'Program management', 'TB/HIV',
                       'Performance Based Financing', 'Unidentified', 'Other RSSH/Unidentified')

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

