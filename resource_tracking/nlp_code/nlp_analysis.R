# ----------------------------------------------

# Naomi Provost
# Calculate PPV, NPV, Specificity, Sensitivity, and Frequency Matrix for NLP 
# 
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
library(gmodels)

# Make functions
make_matrix = function(dt, mod) {
  y <- dt$corrected_module==mod
  yhat <- dt$predicted_module==mod
  # store indices
  w = which(y== 1)
  w2 = which(yhat==1)
  
  # compute sensitivity/specificity
  sensitivity = mean( yhat[w] == 1 ) 
  specificity = mean( yhat[-w] == 0 ) 
  
  # compute ppv/npv/concordance
  ppv = mean( y[w2] == 1) 
  npv = mean( y[-w2] == 0) 
  
  return (data.table("module" = mod, "specificity" = specificity, "sensitivity" = sensitivity, "ppv" = ppv, "npv" = npv))
}

iterMatrix = function(dt_iter){
  df_total = data.table()
  for(modu in unique(dt_iter$corrected_module)){
    matr = make_matrix(dt_iter, modu)
    df_total <- rbind(df_total,matr)
  }
  return(df_total)
}

makefreq <- function(freqi, disease){
  #break down by desease
  #open in excel - delete top section, parse by '|' then =TRIM() colomns and rows, keep second row only
  cf = capture.output(CrossTable(freqi$corrected_module, freqi$predicted_module_translated, prop.r=FALSE, prop.c=TRUE,
                                 prop.t=FALSE, prop.chisq=FALSE))
  write.csv(cf, paste0(dir, disease,".csv"), row.names = FALSE)
}

# ----------------------------------------------
#### load the data #### 
# ----------------------------------------------
dir <- "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/nlp_data/"
##change this to whatever the most recent iteration is: 
bestIteration <- data.table(read_csv(paste0(dir,"model_outputs/iteration3/iteration3.csv")))

total_Iteration = bestIteration[,c("corrected_module", "predicted_module_translated")]
tb_Iteration = bestIteration[disease_lang_concat %in% c("tbfr", "tbesp", "tbeng"),c("corrected_module", "predicted_module_translated")]
hiv_Iteration = bestIteration[disease_lang_concat %in% c("hivfr", "hivesp", "hiveng"),c("corrected_module", "predicted_module_translated")]
mal_Iteration = bestIteration[disease_lang_concat %in% c("malariafr", "malariaesp", "malariaeng"),c("corrected_module", "predicted_module_translated")]

total_matrix = iterMatrix(total_Iteration)
tb_matrix = iterMatrix(tb_Iteration)
hiv_matrix = iterMatrix(hiv_Iteration)
mal_matrix = iterMatrix(mal_Iteration)

makefreq(tb_Iteration, "hiv_freq")
makefreq(hiv_Iteration, "mal_freq")
makefreq(mal_Iteration, "tb_freq")
