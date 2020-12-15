# prep_synthesis_equity_data.R

# Merge IHME/PATH absorption with EHG countries
# Francisco Rios Casas

# Pending:

#########################
# set up
#########################

library(data.table)
library(readxl)

# input file
box = paste0("C:/Users/frc2/Box Sync/Global Fund Files/")
inFile = paste0(box, 'tableau_data/cumulative_absorption.csv')
inFile2 = paste0(box, 'tableau_data/all_absorption.csv')
ehgEquityFile = paste0(box, 'synthesis/data/EHG Equity Absorption Synthesis 22Nov20.xlsx') # with data on Equity

# output files
outDir = paste0(box, '/synthesis/data/')

# data file to be saved
outFile = paste0(outDir, 'merged_consortia_equity_absorption_data.csv')
#######################################################

#######################################################
# read in IHME and EGH data
#######################################################
idat1 <- as.data.table(read.csv(inFile)) # cumulative IHME absorption data
idat2 <- as.data.table(read.csv(inFile2)) # all IHME absorption data
# edat1 <- as.data.table(read_xlsx(ehgFile, sheet = "National Program")) # EHG data with Equity and non-SO data
# edat2 <- as.data.table(read_xlsx(ehgFile, sheet = "RSSH")) # EGH RSSH Data in a separate tab
edat3 <- as.data.table(read_xlsx(ehgEquityFile, sheet="National Program")) # this is the cumulative Equity data

####################################
# prep IHME data
###################################

# add equity indicator to cumulative data
mf_map <- readRDS("J:\\Project\\Evaluation\\GF\\resource_tracking\\modular_framework_mapping\\2018_2020_MF.rds")
equity_map <- mf_map[,.(gf_module, gf_intervention, equity)]
idat1 <- merge(idat1, equity_map, by=c('gf_module', 'gf_intervention'), all.x = TRUE)
idat1 <- idat1[gf_module=="Vector control", equity:=FALSE]
# corrections
idat1[gf_intervention=="Differentiated HIV testing services", equity:=TRUE]
idat1[gf_intervention=="Community-led advocacy", equity:=TRUE]
idat1[gf_intervention=="Social mobilization, building community linkages, collaboration and coordination", equity:=TRUE]

# add final equity indicators for idat2
idat2[gf_intervention=="Differentiated HIV testing services", equity:=TRUE]
idat2[gf_intervention=="Community-led advocacy", equity:=TRUE]
idat2[gf_intervention=="Social mobilization, building community linkages, collaboration and coordination", equity:=TRUE]

# subset rows in IHME cumulative data to remove those 'calculated' incorrectly
idat1 <- idat1[cumul_abs_method=="reported_in_pudr"]

# subset columns 
idat1 <- idat1[,.(loc_name, grant, grant_period, disease, start_date, end_date, gf_module, gf_intervention, cumulative_budget, cumulative_expenditure, rssh, equity)]

# add year variable
idat1[,year:=year(end_date)]

# sum to country disease and year--cumulative data contains 2019 and 2020 data for some grants
idat1 <- idat1[,.(cum_budget=sum(cumulative_budget, na.rm=TRUE), cum_expend=sum(cumulative_expenditure, na.rm=TRUE)), by=c('loc_name', 'disease', 'year', 'gf_module', 'gf_intervention', 'rssh', 'equity')] 

##############################
# Prep first year ihme data
##############################
# remove NFM1 data from data set
idat2 <- idat2[grant_period%in%c('2018-2020', '2019-2021', '2019-2022')]

# select first year of implementation data 
idat2.first <- idat2[semester%in%c("Semester 1-2", "Semester 1-3")]

# remove GTM INCAP Progress Report from this since it's more of a PU and not end of year PUDR
idat2.first <- idat2.first[file_name!="GTM-H-INCAP_Progress Report_30Jun2019_v10_30082019_RevALF english version.xlsx"]

# add year to this subset of data
idat2.first <- idat2.first[,year:=2018]

# change year for GTM grants which are a year behind schedule
idat2.first <- idat2.first[loc_name=="Guatemala", year:=2019]

# sum first year of data
idat2.first <- idat2.first[,.(cum_budget=sum(budget, na.rm=TRUE), cum_expend=sum(expenditure, na.rm=TRUE)), by=c('loc_name', 'disease', 'year', 'gf_module', 'gf_intervention', 'rssh', 'equity')]

#####################################################s
# Prep missing cumulative data for 2020 and 2019
####################################################

# certain grants are missing cumulative data in IHME PUDRs for 2020, this includes the following grants
# - COD-C-CORDAID
# - COD-H-MOH
# - COD-M-SANRU
# - COD-T-MOH
# - GTM-H-INCAP

# Senegal grants do have the data reported for 2020, but I also will calculate cumulative absorption for 2019 in case we need that data point

# subset to grants that are missing cumulative data for 2020 and those that can be summed for 2019 and 2020
idat2.second <- idat2[grant %in% c('COD-C-CORDAID', 'COD-H-MOH', 'COD-M-SANRU', 'COD-T-MOH', 'SEN-Z-MOH', 'SEN-M-PNLP', 'SEN-H-ANCS', 'SEN-H-CNLS')]

# subset to first year, second year, and third year PUDRs
idat2.second <- idat2.second[semester%in%c('Semester 1-2', 'Semester 3-4', 'Semester 5')]

# add year to pudr rows
idat2.second <- idat2.second[semester=="Semester 1-2", year:=2018]
idat2.second <- idat2.second[semester=="Semester 3-4", year:=2019]
idat2.second <- idat2.second[semester=="Semester 5", year:=2020]

# calculate cumulative absorption for 2019
idata19 <- idat2.second[semester%in%c("Semester 1-2", "Semester 3-4"), .(cum_budget=sum(budget, na.rm=TRUE), cum_expend=sum(expenditure, na.rm=TRUE)), by=c('loc_name', 'disease', 'gf_module', 'gf_intervention', 'rssh', 'equity')]

# add corresponding year
idata19 <- idata19[,year:=2019] 

# calculate cumulative absorption for 2020 (and exclude grants that already have it reported in pudrs (i.e. Senegal grants))
idata20 <- idat2.second[grant%in%c('COD-C-CORDAID', 'COD-H-MOH', 'COD-M-SANRU', 'COD-T-MOH') & semester%in%c("Semester 1-2", "Semester 3-4", "Semester 5"), .(cum_budget=sum(budget, na.rm=TRUE), cum_expend=sum(expenditure, na.rm=TRUE)), by=c('loc_name', 'disease','gf_module', 'gf_intervention', 'rssh', 'equity')]

# add corresponding year
idata20 <- idata20[,year:=2020]

# calculate cumulative absorption for INCAP grant 
idata20.incap <- idat2[grant=='GTM-H-INCAP' & semester%in%c('Semester 1-3', 'Semester 4'), .(cum_budget=sum(budget, na.rm=TRUE), cum_expend=sum(expenditure, na.rm=TRUE)), by=c('loc_name', 'disease','gf_module', 'gf_intervention', 'rssh', 'equity')]
idata20.incap <- idata20.incap[,year:=2020]

# bind together four files: (1) IHME cumulative data reported in pudrs, (2) first year data from PUDRs, and (3) calculated 2019 and 2020 data, and (4) second year GTM-H-INCAP grant
idata <- rbind(idat1, idat2.first, idata19, idata20, idata20.incap, fill=TRUE)

# sum up together one more time
idata <- idata[,.(cum_budget=sum(cum_budget, na.rm=TRUE), cum_expend=sum(cum_expend, na.rm=TRUE)), by=c('loc_name', 'disease','year', 'gf_module', 'gf_intervention', 'rssh', 'equity')]

# reorder columns
# idata <- idata[,.(loc_name, year, gf_module, cum_budget, cum_expend)]

##################################################
# Prep EHG Data
##################################################
# subset columns
# edat1 <- edat1[,.(Country, component, year, gf_module, cum_budget, cum_expend)] 
# edat2 <- edat2[,.(loc_name, component, year, gf_module, cum_budget, cum_expend)]
edat3 <- edat3[,.(loc_name, component, year, Module, gf_intervention, cum_budget, cum_expend)]

# add indicator for RSSH variables
# # edat1$rssh <- FALSE
# edat2$rssh <- TRUE 
edat3$rssh <- FALSE

# # add indicator for Equity variables
# # edat1$equity <- FALSE
# edat2$equity <- FALSE
edat3$equity <- TRUE

# rename variables
# setnames(edat1, old=c('Country', 'component'), new=c('loc_name', 'disease')) 
setnames(edat3, old=c('Module'), new=c('gf_module'))

# bind two EHG data files together
edata <- edat3

# sum to country, disease, year, module, and rssh, equity
edata <- edata[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c('cum_budget', 'cum_expend'), by =c('loc_name', 'year', 'gf_module', 'gf_intervention', 'rssh', 'equity')] 

# corrections
edata[gf_module=="Treatment", gf_module:="Treatment, care and support"]
edata[gf_module=="HIV testing", gf_module:="HIV Testing Services"]
edata[gf_module=="Human rights", gf_module:="Programs to reduce human rights-related barriers to HIV services"]
edata[gf_module=="PMTCT", gf_module:="Prevention of mother-to-child transmission"]
edata[gf_module=="Prevention gen pop", gf_module:="Prevention programs for general population"]
edata[gf_module=="Adolescents", gf_module:="Prevention programs for adolescents and youth, in and out of school"]
edata[gf_module=="FSW and clients", gf_module:="Comprehensive prevention programs for sex workers and their clients"]
edata[gf_module=="Other vulnerable populations", gf_module:="Prevention programs for adolescents and youth, in and out of school"]
edata[gf_module=="MSM", gf_module:="Comprehensive prevention programs for men who have sex with men"]
edata[gf_module=="PWID", gf_module:="Comprehensive prevention programs for people who inject drugs and their partners"]
edata[gf_module=="Specific prevention interventions (SPI)", gf_module:="Specific prevention interventions"]
edata[gf_module=="Comprehensive prevention programs for MSM", gf_module:="Comprehensive prevention programs for men who have sex with men"]
edata[gf_module=="Comprehensive prevention programs for people who inject drugs (PWID) and their partners", gf_module:="Comprehensive prevention programs for people who inject drugs and their partners"]
edata[gf_module=="Comprehensive prevention programs for TGs", gf_module:="Comprehensive prevention programs for transgender people"]
# edata[gf_module=="Comprehensive programs for people in prisons and other closed settings", gf_module:=""]
edata[gf_module=="MDR-TB", gf_module:="Multidrug-resistant TB"]

# RSSH Corrections
edata[gf_module=="RSSH: Community responses and systems", gf_module:="Community responses and systems"]
edata[gf_module=="RSSH: Health management information systems and M&E", gf_module:="Health management information system and monitoring and evaluation"]
edata[gf_module=="RSSH: Human resources for health (HRH), including community health workers", gf_module:="Human resources for health, including community health workers"]
edata[gf_module=="RSSH: Integrated service delivery and quality improvement", gf_module:="Integrated service delivery and quality improvement"]
edata[gf_module=="RSSH: Procurement and supply chain management systems", gf_module:="Procurement and supply chain management systems"]
edata[gf_module=="RSSH: Financial management systems", gf_module:="Financial management systems"]
edata[gf_module=="RSSH: National health strategies", gf_module:="National health strategies"]

# # DISEASE CORRECTIONS
# edata[is.na(disease) & rssh=="TRUE", disease:='rssh']

# bind IHME and EHG data together
final_data <- rbind(idata, edata, fill=TRUE)

# subset to equity data
final_equity_data <- final_data[equity==TRUE]

# add label map
equity_modules_interventions <- as.data.table(read_xlsx(paste0(box, "synthesis/data/draft_hrgequity_related_modules_interventions.xlsx")))

# # fix some names to make sure labels are added correctly
# final_equity_data[gf_intervention=="Integrated community case management (ICCM)", gf_intervention:="Integrated community case management (iCCM)"]
# final_equity_data[gf_intervention=="Community led advocacy", gf_intervention:="Community-led advocacy"]
# final_equity_data[gf_intervention=="Addressing stigma, discrimination and violence against MSM", gf_intervention:="Addressing stigma, discrimination and violence against men who have sex with men"]
# final_equity_data[gf_intervention=="Behavioral interventions for MSM", gf_intervention:="Behavioral interventions for men who have sex with men"]
# final_equity_data[gf_intervention=="Community empowerment for MSM", gf_intervention:="Community empowerment for men who have sex with men"]

# merge onto new labels
final_equity_data <- merge(final_equity_data, equity_modules_interventions, by=c('gf_module', 'gf_intervention'), all.x = TRUE)

# corrections that need to be made
equity_additions <- final_equity_data[is.na(...1)]
final_equity_data <- final_equity_data[!is.na(...1)]

# add labels to the few that are not merged correctly
equity_additions[grepl(gf_module, pattern = 'human rights'), crg_hr := TRUE ]
equity_additons[grepl(gf_module, pattern = 'human rights'), crg_hr := TRUE ]
equity_additions[grepl(gf_intervention, pattern = 'human rights'), crg_hr := TRUE ]
equity_additions[grepl(gf_intervention, pattern = 'Key'), kp := TRUE ]
equity_additions[grepl(gf_intervention, pattern = 'Addressing stigma'), crg_hr := TRUE ]
equity_additions[grepl(gf_module, pattern = 'Comprehensive'), kp := TRUE ]
equity_additions[grepl(gf_intervention, pattern = 'Prong'), other_equity := TRUE ]
equity_additions[grepl(gf_intervention, pattern = 'Differentiated'), kp := TRUE ]
equity_additions[grepl(gf_module, pattern = 'adolescents'), other_equity := TRUE ]

final_equity_data <- rbind(final_equity_data, equity_additions, fill=TRUE)

# View(unique(equity_additions[is.na(crg_hr) & is.na(kp) & is.na(other_equity),.(gf_module, gf_intervention)]))
final_equity_data <- final_equity_data[,.(loc_name, gf_module, gf_intervention, year, equity, crg_hr, kp, other_equity, cum_budget, cum_expend)]
# subset columns


# calculate absorption
final_equity_data[,absorption:=cum_expend/cum_budget]

# subset to those that are not zero for budget and expenditure
final_equity_data <- final_equity_data[cum_budget!=0 & cum_expend!=0]

# save data 
write.csv(final_equity_data, file=outFile)
