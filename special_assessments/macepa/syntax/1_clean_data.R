# Clean MACEPA data
# June 24, 2019
# Francisco Rios Casas

# set up
library(data.table)

# load data table
macepa_data <- fread('C:/Users/frc2/Documents/data/macepa/Carte_data.csv')

# columns to keep are date of diagnosis and the region
dt <- macepa_data[,c(17,28)]

# rename date column, reformate date column
names(dt) <- c("old_date_diagnos", "region")
dt$date_diagnosis <- as.Date(dt$old_date_diagnos, format = "%m/%d/%Y")
dt$month_year <- format(dt$date_diagnosis, "%m-%Y")

# create counts in a datatable
dt2 <- dt[, c("month_year", "region")]

# double-check why 08-2017 Matama is so large

# subset into three countries
# change from list to datatable

# MATAM
DT1 <- dt2[region=="MATAM"]
m <- data.table(table(DT1$month_year, DT1$region))

# LOUGA
DT2 <- dt2[region=="LOUGA"]
l <- data.table(table(DT2$month_year, DT2$region))

# SAINT-LOUIS
DT3 <- dt2[region=="SAINT-LOUIS"]
s <- data.table(table(DT3$month_year, DT3$region))

# merge all of the macepa data
DTM <- rbind(m, l, s)
saveRDS(DTM, file = "macepa_data.rds")
