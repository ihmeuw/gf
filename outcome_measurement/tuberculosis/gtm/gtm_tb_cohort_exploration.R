##########################################################
# GTM mortality cohort data exploration
# J. Ross 8-22-2018
#
#########################################################

rm(list=ls())
library(data.table)

#Read the prepped cohort data. This is broken up since it is part of a script that runs on the cluster.
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j') 
cohort_dir <- paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/prepped_data/')
cohort <- data.table(fread(paste0(cohort_dir, "GTM - Tx cohort data 2012-2016.csv")))
cohort <- cohort[order(year, deptocode, table)]


#Subset to avoid overlapping types - See full listing below of all of the values of 'table'
tb_denom <- cohort[table %in% c('Nuevos Pulmonares BK+', 
                                     'Nuevos Pulmonares BK-', 'Nuevos Pediatricos', 
                                     'Nuevos Extrapulmonares', 'Nuevos TB/VIH', 'Retratamiento')]

#Include only the annual totals and drop the trimester subtotals
tb_denom <- tb_denom[col_name=='TOTAL',]

# Initial result - Total of new and retreatment cases in the cohorts per year----------------------------------------------------------------------------
byVars = names(tb_denom)[names(tb_denom)%in%c('year')]
annual= tb_denom[, list(value=sum(na.omit(value))), by=byVars]
#This results in annual counts >6000, which is nearly double the expected values. 

#4 categories of row_name_B "COMPLETED TREATMENT", "DEATHS", LOST TO FOLLOW-UP", "REFERRED"
#Is it correct the FRACASO_TERAPEUTICO (treatment failure) is under completed treatment?

#subset deaths
deaths <- tb_denom[row_name_B=="DEATHS"]
## total deaths 
byVars = names(deaths)[names(deaths)%in%c('year')]
annual_d= deaths[, list(value=sum(na.omit(value))), by=byVars]

#Number of reporting categories varies by year
deaths[, .N, by = year]

#See that Departments 1, 14 and 17 consistently have more reporting categories than other departments
#I haven't figured out how to code the two-way frequency table in data table
table(deaths$year, deaths$deptocode)


##This is old. It doesn't correctly identify the hierarchy of values of table. Just leaving it as a listing of all values of table---------------------
#47 treatment categories listed under 'table'. Unclear hierarchy for which of these subset to others
cats <- unique(tb_denom$table)

#Groups these categories into mutually sets of treatment categories
  #tb_new_ids versus tb_ret_ids
  #hiv_neg_new_ids versus hiv_neg_ret_ids vs. tb_hiv_new vs. tb_hiv_ret
tb_new_ids <- c("Nuevos Extrapulmonares",
                "Nuevos Extrapulmonares Pediatricos TB/VIH",
                "Nuevos Extrapulmonares TB/VIH",                 
                "Nuevos Pediatricos",                             
                "Nuevos Pediatricos BK+",                         
                "Nuevos Pediatricos BK-",                        
                "Nuevos Pediatricos Extrapulmonares",             
                "Nuevos Pediatricos TB/VIH BK+",                  
                "Nuevos Pediatricos TB/VIH BK-",                 
                "Nuevos Pulmonares BK+",                          
                "Nuevos Pulmonares BK-",                          
                "Nuevos Pulmonares TB/VIH BK+",                  
                "Nuevos Pulmonares TB/VIH BK-",                   
                "Nuevos TB/VIH",
                "Nuevos Extrapulmonares Pediatricos", 
                "Nuevos Extrapulmonares BK+",                    
                "Nuevos Extrapulmonares BK-",                     
                "Nuevos Extrapulmonares pediatricos BK+",         
                "Nuevos Extrapulmonares pediatricos BK-",        
                "Nuevos Pulmonares pediatricos BK+",              
                "Nuevos Pulmonares pediatricos BK-"            
                )                                  


tb_ret_ids <- c("Retratamiento",                                 
                       "Retratamiento BK+",                              
                       "Retratamiento BK-",                              
                       "Retratamiento Extrapulmonar",                   
                       "Retratamiento Extrapulmonar TB/VIH",             
                       "Retratamiento Pediatricos",                      
                       "Retratamiento Pediatricos TB/VIH",              
                       "Retratamiento TB/VIH BK+",                       
                       "Retratamiento TB/VIH BK-",                       
                       "Retratamiento BK+ TB/VIH",                       
                       "Retratamiento BK- TB/VIH",                       
                       "Retratamiento Extrapulmonar Pediatricos",       
                       "Retratamiento Extrapulmonar Pediatricos TB/VIH", 
                       "Retratamiento Pediatricos BK+",                  
                       "Retratamiento Pediatricos BK+ TB/VIH",          
                       "Retratamiento Pediatricos BK-",                  
                       "Retratamiento Pediatricos BK- TB/VIH",           
                       "Retratamiento Abandonos Recuperados & BK+",     
                       "Retratamiento Abandonos Recuperados & BK-",      
                       "Retratamiento Extrapulmonar & TB/VIH",           
                       "Retratamiento Fracasos & BK+",                  
                       "Retratamiento Fracasos & BK-",                   
                       "Retratamiento Recaidas & BK+",                   
                       "Retratamiento Recaidas & BK-",                  
                       "Retratamiento TB/VIH & BK+",                     
                       "Retratamiento TB/VIH & BK-" 
                       )

#Now divide four ways by hiv and new and retreatment status
hiv_neg_new_ids <- c("Nuevos Extrapulmonares",
                "Nuevos Pediatricos",                             
                "Nuevos Pediatricos BK+",                         
                "Nuevos Pediatricos BK-",                        
                "Nuevos Pediatricos Extrapulmonares",             
                "Nuevos Pulmonares BK+",                          
                "Nuevos Pulmonares BK-",                          
                "Nuevos Extrapulmonares Pediatricos", 
                "Nuevos Extrapulmonares BK+",                    
                "Nuevos Extrapulmonares BK-",                     
                "Nuevos Extrapulmonares pediatricos BK+",         
                "Nuevos Extrapulmonares pediatricos BK-",        
                "Nuevos Pulmonares pediatricos BK+",              
                "Nuevos Pulmonares pediatricos BK-"
)
hiv_neg_ret_ids <- c(
                "Retratamiento",                                 
                "Retratamiento BK+",                              
                "Retratamiento BK-",                              
                "Retratamiento Extrapulmonar",                   
                "Retratamiento Pediatricos",                      
                "Retratamiento Extrapulmonar Pediatricos",       
            "Retratamiento Pediatricos BK+",                  
            "Retratamiento Pediatricos BK-",                  
            "Retratamiento Abandonos Recuperados & BK+",     
            "Retratamiento Abandonos Recuperados & BK-",      
            "Retratamiento Fracasos & BK+",                  
            "Retratamiento Fracasos & BK-",                   
            "Retratamiento Recaidas & BK+",                   
            "Retratamiento Recaidas & BK-"                 
)
tb_hiv_new <-c("Nuevos Extrapulmonares Pediatricos TB/VIH",
          "Nuevos Extrapulmonares TB/VIH",     
          "Nuevos Pediatricos TB/VIH BK+",                  
          "Nuevos Pediatricos TB/VIH BK-",   
          "Nuevos Pulmonares TB/VIH BK+",                  
          "Nuevos Pulmonares TB/VIH BK-",                   
          "Nuevos TB/VIH"
)         
          
tb_hiv_ret <-c("Retratamiento Extrapulmonar TB/VIH",  
          "Retratamiento Pediatricos TB/VIH", 
          "Retratamiento TB/VIH BK+",                       
          "Retratamiento TB/VIH BK-",                       
          "Retratamiento BK+ TB/VIH",                       
          "Retratamiento BK- TB/VIH", 
          "Retratamiento Extrapulmonar Pediatricos TB/VIH", 
          "Retratamiento Pediatricos BK+ TB/VIH",  
          "Retratamiento Pediatricos BK- TB/VIH", 
          "Retratamiento Extrapulmonar & TB/VIH",  
          "Retratamiento TB/VIH & BK+",                     
          "Retratamiento TB/VIH & BK-"
)

#Separate new and retreatment cases and see how they match up with the 
new <- tb_denom[table %in% tb_new_ids]
retreatment <- tb_denom[table %in% tb_ret_ids]

byVars = names(new)[names(new)%in%c('year','row_name_B')]
annual_new = new[, list(value=sum(na.omit(value))), by=byVars]

byVars = names(retreatment)[names(retreatment)%in% c('year','row_name_B')]

#Separate tb and tb/hiv
tb <- tb_denom[table %in% c('hiv_neg_new_ids','hiv_neg_ret_ids')]
tb_hiv <- tb_denom[table %in% c('tb_hiv_new','tb_hiv_ret')]

table(new$row_name_B, new$table)









