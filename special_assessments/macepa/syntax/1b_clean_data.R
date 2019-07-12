#1b_clean_data.R
# June 25, 2019
# Francisco Rios Casas
# code to clean the DHIS2 Data downloaded for Senegal
# Confirmed Malaria cases from 2016 to 2019 for three regions (Louga, Matam, Saint Louis)

# set up
library(data.table)

# load data table
dhis2_data <- fread('C:/Users/frc2/Documents/data/dhis2_paluconfirme.csv')

# subset data for three regions of interest (Matam, Louga, Saint-Louis)
dt1 <- dhis2_data[organisationunitname %in% c("RM Matam", "RM Louga", "RM Saint-Louis")]

# select columns to delete
Cols.chosen = c("organisationunitid", "periodid", "organisationunitcode", "organisationunitdescription", "periodcode", "perioddescription")
dt1[,(Cols.chosen) := NULL]

# convert format into date r understands (the first of each month)
dt1$month_year <- paste("1", dt1$periodname, sep=" ")

# convert from string to date format
dt1[,V1 := as.Date(month_year, format = "%d %B %Y")]

# change format to just month and year again
dt1$V1 <- format(dt1$V1, "%m-%Y")

# create new variable to store the corretly formatted name
dt1$V2 <- NA

# fill in proper names (by removing the RM from the name)
dt1$V2[dt1$organisationunitname=="RM Matam"] <- "MATAM"
dt1$V2[dt1$organisationunitname=="RM Louga"] <- "LOUGA"
dt1$V2[dt1$organisationunitname=="RM Saint-Louis"] <- "SAINT-LOUIS"

# select additional columns we want to delete
dt1[,c("organisationunitname", "periodname", "month_year") := NULL]

# change name of column of interest to N
setnames(dt1,"cas paludisme confirmés total", "N_DHIS2")

# setorder
setcolorder(dt1, c("V1", "V2", "N_DHIS2"))

# save as RDS object
saveRDS(dt1, file = "dhis2_data.rds")