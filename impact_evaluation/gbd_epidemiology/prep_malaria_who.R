# clean WHO malaria data for use in SO1 section of synthesis report
# Francisco Rios
# 1/22/2020

# set up
library(data.table)
library(tidyr)

# this file reads csv file of the WHO PDF of data that was converted to an excel file and then saved as a CSV
DT1 <- fread("J:/Project/Evaluation/GF/impact_evaluation/synthesis_epidemiology/raw_data/who_malaria/annex3f_page1.csv") # this is the country-specific findings
DT2 <- fread("J:/Project/Evaluation/GF/impact_evaluation/synthesis_epidemiology/raw_data/who_malaria/annex3f_page2.csv") # this is the global trends in malaria

# combine two datasets together
DT <- rbind(DT1, DT2, use.names=FALSE)

# fix headings
setnames(DT,
         old=names(DT),
         new=c("country", "year", "population_at_risk", "cases_lower", "cases_point", "cases_upper", "deaths_lower", "deaths_point", "deaths_upper", "V10"))

# delete the last column of NAs
DT[,V10:=NULL]

# delete first three rows
DT <- DT[-c(1:3),]

# replace blank spaces in country and year columns with NA
DT[country==""]$country  <- NA
DT[year==""]$year <- NA
DT[population_at_risk==""]$population_at_risk <- NA

# add in missing country names
DT <- DT %>% fill(country)
DT <- DT %>% fill(year)
DT <- DT %>% fill(population_at_risk)

# re-convert to datatable as function above will change to dataframe only
DT <- as.data.table(DT)

# subset to 8 PCE countries and global trends
DT <- DT[country %in% c("Cambodia", "Myanmar", "Senegal", "Democratic Republic of", "the Congo", "Uganda", "Guatemala", "Mozambique", "Sudan", "South Sudan4", "Total")]

# remove commas
DT$population_at_risk <- gsub(",", "", DT$population_at_risk)
DT$cases_lower <- gsub(",", "", DT$cases_lower)
DT$cases_point <- gsub(",", "", DT$cases_point)
DT$cases_upper <- gsub(",", "", DT$cases_upper)
DT$deaths_lower <- gsub(",", "", DT$deaths_lower)
DT$deaths_point <- gsub(",", "", DT$deaths_point)
DT$deaths_upper <- gsub(",", "", DT$deaths_upper)
# would be better written as a loop

# change character types
cols.num <- c("year", "population_at_risk", "cases_lower", "cases_point", "cases_upper", "deaths_lower", "deaths_point", "deaths_upper")   
DT <- DT[, lapply(.SD, as.numeric), by=country, .SDcols=cols.num]
lapply(DT, class)

# remove extra rows where cases_lower are missing ""
DT <- DT[!is.na(cases_lower)]

# clean the names of the countries
unique(DT$country)

DT$country[which(DT$country=="Democratic Republic of")] <- "DRC"
DT$country[which(DT$country=="the Congo")] <- "DRC"
DT$country[which(DT$country=="South Sudan4")] <- "South Sudan"
DT$country[which(DT$country=="Total")] <- "Global"

# calculate incidence per year
DT$incidence_lower <- DT$cases_lower/DT$population_at_risk
DT$incidence_point <- DT$cases_point/DT$population_at_risk
DT$incidence_upper <- DT$cases_upper/DT$population_at_risk

# calculate mortality per year
DT$mortality_lower <- DT$deaths_lower/DT$population_at_risk
DT$mortality_point <- DT$deaths_point/DT$population_at_risk
DT$mortality_upper <- DT$deaths_upper/DT$population_at_risk

# change units
rate.col <- c("incidence_lower", "incidence_point", "incidence_upper", "mortality_lower","mortality_point", "mortality_upper")
DT <- DT[, lapply(.SD, function(x) x*100000), by=.(country, year, population_at_risk, cases_lower, cases_point, cases_upper, deaths_lower, deaths_point, deaths_upper), .SDcols=rate.col]

# Save on j-drive
saveRDS(DT,   file="J:/Project/Evaluation/GF/impact_evaluation/synthesis_epidemiology/raw_data/who_malaria/estimated_malaria_cases_and_deaths_2010_2018.RDS")
write.csv(DT, file="J:/Project/Evaluation/GF/impact_evaluation/synthesis_epidemiology/raw_data/who_malaria/estimated_malaria_cases_and_deaths_2010_2018.csv")
