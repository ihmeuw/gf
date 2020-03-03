
#------------------------------------------------------
# Set shared values to be used throughout the database. 
#------------------------------------------------------

# ---------------------------------------------------------------------------------------------------
# #Mark which grants are currently active to save in file - this should be updated every grant period! 
# ----------------------------------------------------------------------------------------------------
current_gtm_grants <- c('GTM-H-INCAP', 'GTM-M-MSPAS', 'GTM-M-MSPAS', 'GTM-T-MSPAS', 'GTM-T-MSPAS')
current_gtm_grant_period <- c('2018-2020', '2018-2020', '2019-2021', '2016-2019', '2019-2022')

current_cod_grants <- c('COD-C-CORDAID', 'COD-H-MOH', 'COD-T-MOH', 'COD-M-MOH', 'COD-M-SANRU')
current_cod_grant_period <- rep("2018-2020", 5)

current_uga_grants <- c('UGA-C-TASO', 'UGA-H-MoFPED', 'UGA-M-MoFPED', 'UGA-M-TASO', 'UGA-T-MoFPED')
current_uga_grant_period <- rep("2018-2020", 5)

current_sen_grants <- c('SEN-H-ANCS', 'SEN-H-CNLS', 'SEN-M-PNLP', 'SEN-Z-MOH')
current_sen_grant_period <- rep("2018-2020", 4)

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
gos_year = 2015

#----------------------------------
# Other global variables 
#----------------------------------
rssh_abbrev_mods = c("Info systems & M&E", "Financial systems", "HR & health workers", "Service delivery", "Nat. health strategies", 
                                 "PSM", "Community systems")
rssh_gf_mods = c("Community responses and systems", "Financial management systems", "Health management information system and monitoring and evaluation", 
                 "Human resources for health, including community health workers", "Integrated service delivery and quality improvement", 
                 "National health strategies",  "Procurement and supply chain management systems")

# These were validated using metadata extracted from the Global Fund Data explorer, saved here: "J:\Project\Evaluation\GF\resource_tracking\_gf_files_gos\metadata\grant_agreement_implementation_periods_dataset_201963.csv"
# Emily Linebarger, 2/28/2020
governmental_prs = c('COD-M-MOH', 'GTM-M-MSPAS', 'UGA-M-MoFPED', 'UGA-T-MoFPED', 'COD-H-MOH', 'UGA-H-MoFPED', 'COD-T-MOH', 
                 'GTM-T-MSPAS', 'SEN-Z-MOH', 'SEN-S-MOH', 'UGA-S-MoFPED', 'SEN-M-PNLP', 'GUA-311-G06-H', 'SEN-H-CNLS', 'SNG-T-PNT', 
                 'COD-607-G05-T', 'GTM-610-G04-T', 'GUA-M-MSPAS', 'SEN-102-G02-M-00', 'SEN-405-G03-M', 'SEN-708-G07-M', 'SEN-708-G08-T', 
                 'SNG-H-DLSI', 'SNG-M-PNLP', 'UGA-011-G09-S', 'UGA-102-G01-H-00', 'UGA-202-G02-M-00', 'UGA-202-G03-T-00', 'UGA-304-G04-H', 
                 'SEN-102-G01-H-00', 'SEN-607-G05-H', 'UGA-607-G06-T', 'UGD-011-G11-M', 'UGD-405-G05-M', 'UGD-708-G07-H', 'UGD-708-G08-M', 
                 'ZAR-202-G01-T-00', 'ZAR-304-G02-H', 'ZAR-304-G03-M', 'ZAR-506-G04-T', 'ZAR-708-G06-H', 'ZAR-809-G10-H', 'ZAR-810-G09-M', 
                 'ZAR-911-G13-T', 'ZAR-M-MOH', 'ZAR-S-MOH') # This includes UN Agencies. Emily Linebarger 2/28/2020 
civil_society_prs = c('COD-M-SANRU', 'UGA-M-TASO', 'UGA-C-TASO', 'COD-H-CORDAID', 'COD-C-CORDAID', 'GTM-H-HIVOS', 'GTM-H-INCAP', 'COD-H-SANRU',
                  'COD-T-CARITAS', 'COD-M-PSI', 'GUA-311-G05-H', 'SEN-H-ANCS', 'SNG-T-PLAN', 'SEN-M-IntraH', 'UGA-S-TASO', 'COD-809-G07-M', 
                  'COD-810-G08-M', 'COD-810-G11-H', 'COD-810-G12-H', 'COD-M-PSI-X', 'GTM-304-G01-H', 'GTM-405-G02-M', 'GTM-607-G03-T', 'SEN-102-G04-H-00', 
                  'SNG-M-IH', 'SEN-607-G06-H', 'UGA-708-G13-H', 'UGD-011-G10-S', 'UGD-011-G12-M', 'ZAR-911-G14-T') 