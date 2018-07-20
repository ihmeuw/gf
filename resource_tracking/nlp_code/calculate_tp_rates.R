# ----------------------------------------------

# Irena Chen
# Calculate the True/False Positive and negative rates
# ----------------------------------------------
###### Set up R / install packages  ###### 
# ----------------------------------------------

rm(list=ls())
library(lubridate)
library(data.table)
library(readr)
library(stats)
library(stringr)
library(rlang)
library(zoo)


# ----------------------------------------------
#### functions to calculate T/F P/N
# ----------------------------------------------


get_needed_cor <- function(orig_mod,predicted_mod,corrected_mod){
  x <- 0
  if(predicted_mod != orig_mod & corrected_mod != orig_mod){
    x <- 1
  } else {
    x <- x
  }
  return(x)
}

get_true_positive <- function(needed_correction,predicted_mod,corrected_mod){
  x <- 0
  if(needed_correction==1 & corrected_mod == predicted_mod){
    x <- 1
  } else {
    x <- x
  }
  return(x)
}

get_negative <- function(orig_mod,corrected_mod){
  x <- 0
  if(corrected_mod == orig_mod){
    x <- 1
  } else {
    x <- x
  }
  return(x)
}

get_true_negative <- function(no_correction, predicted_mod,corrected_mod){
  x <- 0
  if(no_correction==1&predicted_mod==corrected_mod){
    x <- 1
  } else {
    x <- x
  }
  return(x)
}



# ----------------------------------------------
#### load the data #### 
# ----------------------------------------------

##change this to whatever the most recent iteration is: 
bestIteration <- data.table(read_csv("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/nlp_data/model_outputs/iteration3/iteration3.csv"))

bestIteration$needed_correction <- mapply(get_needed_cor, bestIteration$gf_module, bestIteration$predicted_module_translated, bestIteration$corrected_module)
bestIteration$true_positive <- mapply(get_true_positive, bestIteration$needed_correction, bestIteration$predicted_module_translated, bestIteration$corrected_module)
bestIteration$no_correction <- mapply(get_negative, bestIteration$gf_module, bestIteration$corrected_module)
bestIteration$true_negative <- mapply(get_true_negative,bestIteration$no_correction, bestIteration$predicted_module_translated, bestIteration$corrected_module)


iterMatrix <- bestIteration[,list(needed_correction=sum(needed_correction), true_positive=sum(true_positive),
                   true_negative=sum(true_negative), no_correction=sum(no_correction))]

iterMatrix[,true_positive_rate:=true_positive/needed_correction]
iterMatrix[,true_negative_rate:=true_negative/no_correction]
## the complements of these two rates are the false positive and false negative respectively: 







