
#------------------------------------------------------
# Set shared values to be used throughout the database. 
#------------------------------------------------------

# ---------------------------------------------------------------------------------------------------
# #Mark which grants are currently active to save in file - this should be updated every grant period! 
# ----------------------------------------------------------------------------------------------------
current_gtm_grants <- c('GTM-H-HIVOS', 'GTM-H-INCAP', 'GTM-M-MSPAS', 'GTM-M-MSPAS', 'GTM-T-MSPAS', 'GTM-T-MSPAS')
current_gtm_grant_period <- c('2018', '2018-2020', '2018-2020', '2019-2021', '2016-2019', '2019-2022')

current_cod_grants <- c('COD-C-CORDAID', 'COD-H-MOH', 'COD-T-MOH', 'COD-M-MOH', 'COD-M-SANRU')
current_cod_grant_period <- rep("2018-2020", 5)

current_uga_grants <- c('UGA-C-TASO', 'UGA-H-MoFPED', 'UGA-M-MoFPED', 'UGA-M-TASO', 'UGA-T-MoFPED')
current_uga_grant_period <- rep("2018-2020", 5)

#---------------------------------------------------------------------------------------------
# Store a list of the IHME internal country codes that correspond to our project's iso codes. 
# To be used throughout the database for consistency. 
#---------------------------------------------------------------------------------------------
code_lookup_tables = data.table(ihme_country_code = c(128, 190,171, 216), iso_code = c('gtm', 'uga', 'cod', 'sen'), country=c('Guatemala', 'Uganda', 'Congo (Democratic Republic)', 'Senegal'))

#Given a data table and a column that contains the IHME code, returns a column with the matching ISO code. 
gen_iso_code = function(dt, ihme_code_col){
  for (i in 1:nrow(code_lookup_tables)){
    dt[ihme_code_col == code_lookup_tables$ihme_country_code[i], iso_code:=code_lookup_tables$iso_code[i]]
  }
  return(dt)
}

#Given a data table and a column that contains the country ISO code, returns a column with the matching IHME country code. 
gen_ihme_country_code = function(dt, iso_col){
  for (i in 1:nrow(code_lookup_tables)){
    dt[iso_col == code_lookup_tables$iso_code[i], ihme_country_code:=code_lookup_tables$ihme_country_code[i]]
  }
  return(dt)
}

#-----------------------------------------------------------------------------------
# What year do we have GOS data through? Want to make this 
# a static variable so it can be easily updated as we receive new data
#-----------------------------------------------------------------------------------
gos_year = 2017

