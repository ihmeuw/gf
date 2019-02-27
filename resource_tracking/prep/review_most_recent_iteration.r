rm(list=ls())
library(data.table)

j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')


#Overall, concerns with model are: 
#1. Unclear how training data was selected and why (were only low confidence observations sampled to improve the model? Was this disaggregated by language?
#     was this pulled from data that were run through the old module map?)
#2. Currently training a different model for each disease when there are overlaps across diseases (program management, RSSH, etc.)
#3. Performance of the model is stratified by language, and is not clustered around any certain module/intervention grouping. 
#4. Current python model code is very complicated, and it is difficult to untangle exactly what it's doing. 
#5. There is no HIV/TB in the current model, but this can be managed. 
#6. Was the original training dataset hand-coded off of activity description? Activity description + disease? Module/intervention? It's unclear, and it looks like some 
#     observations in here were pulled from the old budgets, because the interventions are not standardized. 

#Analysis of current performance of model. 
iteration5 = fread(paste0(dir, "resource_tracking/multi_country/mapping/nlp_data/model_outputs/iteration5/test_output.csv"))
iteration5 = iteration5[order(-confidence_translated)]

summary(iteration5$confidence_translated)

iteration5[, .N] #73386 observations
total_obs = iteration5[, .N]

iteration5[confidence_translated<.8, .N] #9,270 observations have less than 80% confidence. 
obs_confid_below_80 = iteration5[confidence_translated<.8, .N]
print(paste0("Percentage of observations with confidence below 80%: ", round(obs_confid_below_80/total_obs, 2)*100, "%"))


#Are the observations with low confidence because the counts for those codes are low, or because they're in another language? 
low_confidence = iteration5[confidence_translated < .8]
low_confidence[, lang:="english"]
low_confidence[, lang:=ifelse(grepl("esp", disease_lang_concat), "spanish", lang)]
low_confidence[, lang:=ifelse(grepl("fr", disease_lang_concat), "french", lang)]

#What % of the low confidence observations is each language? 
low_confidence[, .N, by=lang]

#What modules/interventions are causing this low-confidence, and what is their representation in the overall training data? 
review_mods = unique(low_confidence[, gf_module, gf_intervention]) #This isn't concentrated in a few modules/interventions. It's all over the place. 
review_mods