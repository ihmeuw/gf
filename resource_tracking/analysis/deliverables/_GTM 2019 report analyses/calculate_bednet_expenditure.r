library(data.table) 
dt = readRDS("C:/Users/elineb/Box Sync/Global Fund Files/tableau_data/final_expenditures_gtm.rds")
# This should be the same as your "final prepped expenditures" file Audrey - just make sure it 
# goes back to 2003

# What are the available grant periods for malaria in Guatemala? 
unique(dt[grant_disease=="malaria", .(grant, grant_period)])

#Between Q1 2015 and Q4 2017, how much money was spent on bednets? 
unique(dt[grant_disease=="malaria" & start_date>="2015-01-01" & start_date<="2017-10-01" & gf_module=="Vector control", 
          .(budget=sum(budget, na.rm=TRUE), expenditure=sum(expenditure, na.rm=TRUE)), by='gf_intervention'])

# It will be the amount under the interventions "Long lasting insecticidal nets: mass campaign" 
# and "Long lasting insecticidal nets: continuous distribution"
