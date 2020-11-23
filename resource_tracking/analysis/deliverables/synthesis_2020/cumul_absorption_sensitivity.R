# File to compare cumulative absorption calculation methods

################################################################
# set up
################################################################

library(data.table)

# file paths
box = paste0("C:/Users/frc2/Box Sync/Global Fund Files/")
inFile = paste0(box, 'tableau_data/all_absorption.csv')
inFile2 = paste0(box, 'tableau_data/cumulative_absorption.csv')

# output files
outDir = paste0(box, '/synthesis/data/')

# data file to be saved
outFile = paste0(outDir, 'cumul_absorption_calc_comparisons.csv')

# load files
idat1 <- as.data.table(read.csv(inFile)) # all IHME absorption data
idat2 <- as.data.table(read.csv(inFile2)) # cumulative IHME absorption data

################################################################
# First calculate Uganda cumulative absorption using each end-of-year PUDR
################################################################
uganda_data <- idat1[loc_name=="Uganda" & semester%in%c("Semester 1-2", "Semester 3-4", "Semester 5"),.(loc_name, gf_module, semester, grant, start_date, end_date, budget, expenditure)]
uga_20 <- uganda_data[,.(cum_budget=sum(budget, na.rm=TRUE), cum_expend=sum(expenditure, na.rm=TRUE)), by=c('loc_name', 'gf_module')]
uga_20$cumul_abs_method <- "calculated"

##############################################################
# Read in cumulative absorption reported in the PUDRs
#############################################################
uga_recent_pudr <- idat2[loc_name=="Uganda",.(loc_name, gf_module, start_date, end_date, grant, cumul_abs_method, cumulative_budget, cumulative_expenditure)]
uga_20_pudr <- uga_recent_pudr[,.(cum_budget=sum(cumulative_budget, na.rm=TRUE), cum_expend=sum(cumulative_expenditure, na.rm=TRUE)), by=c('loc_name', 'gf_module')]
uga_20_pudr$cumul_abs_method <- "reported in PUDR"

# bind two data sets together
cumulative_absorption <- rbind(uga_20, uga_20_pudr, fill=TRUE)

# save file
write.csv(cumulative_absorption, outFile)
