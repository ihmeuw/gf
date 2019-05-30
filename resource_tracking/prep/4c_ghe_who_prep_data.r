# --------------------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Prep WHO government health data for Global Fund diseases. 
# Data downloaded from WHO's Global Health Expenditure Database. 
# DATE: Last updated March 2019. 
# ---------------------------------------------------------------------------------

who = read_xlsx(paste0(who_raw, "who_global_health_expenditure_database_3.22.19.xlsx"))
setDT(who)

who = who[-1, ] #Drop the first row 

setnames(who, c('loc_name', 'indicator', 'currency_unit', '2010', '2011', '2012', '2013', '2014', '2015', '2016'))
who[, currency_unit:='USD_2019']

#Relabel indicators so they're easier to work with
who[indicator=="Domestic General Government Expenditure on HIV/AIDS and sexually transmitted diseases", indicator:='domestic_ghe_hiv_stis']
who[indicator=="Domestic General Government Expenditure on tuberculosis", indicator:='domestic_ghe_tb']
who[indicator=="Domestic General Government Expenditure on malaria", indicator:='domestic_ghe_malaria']
who[indicator=="External sources of funding on HIV/AIDS and sexually transmitted diseases", indicator:='dah_hiv_stis']
who[indicator=="External sources of funding on tuberculosis", indicator:='dah_tb']
who[indicator=="External sources of funding on malaria", indicator:='dah_malaria']

#Melt data 
who_long = melt(who, id.vars=1:3, variable.name='year', value.name='expenditure')

#The expenditure is in millions USD, so make this explicit. 
who_long[, expenditure:=as.numeric(expenditure)]
who_long[, expenditure:=expenditure*1000000]

#Standardize the country names
who_long[loc_name=="Democratic Republic of the Congo", loc_name:='cod']
who_long[loc_name=='Senegal', loc_name:='sen']
who_long[loc_name=='Uganda', loc_name:='uga']

#Make the year an integer, not a factor. 
who_long[, year:=as.character(year)]
who_long[, year:=as.numeric(year)]


#Save final data 
saveRDS(who_long, paste0(who_prepped, "who_prepped.RDS"))
write.csv(who_long, paste0(who_prepped, "who_prepped.csv"), row.names=FALSE)