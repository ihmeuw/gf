# Apologies for this quick fix/messy solution! Had to make the figures quickly for a report and it would be
# a lot more effort to go back into the initial prep (and rerun it all) to figure out what's going wrong with pulling
# in this line item in the UGA FR... the issue is that there is a budget line item that is being dropped in the UGA NFM3
# TB/HIV FR, and I need to add that line item in for a figure.

# I seriously doubt anyone will ever look at this file... but if you are here, please accept my sincere apologies and I hope the
# above explanation suffices :) -Audrey
# -----------------------------------------------

# -----------------------------------------------
# set up
# -----------------------------------------------
library(data.table)

#Box filepaths - these should be used to source raw files, and to save final prepped files. 
user=as.character(Sys.info()[7])
box = paste0("C:/Users/",user,"/Box Sync/Global Fund Files/tableau_data/")

inFile = paste0(box, 'budgetRevisions_with_frBudgets_activityLevel.csv')
# -----------------------------------------------

# -----------------------------------------------
# check if this line is already in the data so we don't accidentally add it extra times if this code were somehow
# run again... then manually add the budget line and re-save the data, so it's there. 
# -----------------------------------------------
dt = as.data.table(read.csv(inFile))
check = dt[loc_name == 'Uganda' & grant_period == '2021-2023' & data_source == 'funding_request' & file_name == 'UGA_C_NSP_DetailedBudget_20200429_En_2.xlsx' & gf_module == 'Removing human rights and gender related barriers to TB services'] 
while(nrow(check) == 0){
  add_row = dt[loc_name == 'Uganda' & grant_period == '2021-2023' & data_source == 'budget' & gf_module == 'Removing human rights and gender related barriers to TB services'] 
  add_row[, file_name := 'UGA_C_NSP_DetailedBudget_20200429_En_2.xlsx']
  add_row[, grant := NA]
  add_row[, data_source := 'funding_request']
  add_row[, budget_version := 'funding_request20']
  add_row[, version_date := '2020-04-29']
  add_row[, update_date := '2020-04-29']
  dt = rbind(dt, add_row)
  check = dt[loc_name == 'Uganda' & grant_period == '2021-2023' & data_source == 'funding_request' & file_name == 'UGA_C_NSP_DetailedBudget_20200429_En_2.xlsx' & gf_module == 'Removing human rights and gender related barriers to TB services'] 
}

write.csv(dt, inFile, row.names = FALSE)
# -----------------------------------------------

