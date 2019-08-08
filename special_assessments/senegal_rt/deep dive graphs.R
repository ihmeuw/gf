# Francisco Rios Casas
# Financial Analyses for Senegal TB Deep Dive Deliverable
# Aug 5 2019

# set-up r
library(data.table)
library(ggplot2)
library(scales)
library(dplyr)
library(RColorBrewer)

absorption <- readRDS("C:/Users/frc2/Documents/data/finances/absorption_sen.rds")
absorption = absorption[grant=="SEN-Z-MOH"]
absorption = absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('gf_module', 'gf_intervention', 'semester')] # Collapse and recalculate absorption 
absorption[, absorption:=(expenditure/budget)*100]
absorption = absorption[order(gf_module, gf_intervention, semester)] #Order your dataset nicely
absorption[order(-absorption)]

# Semester 1-2 absorption
absorption2 <- absorption[semester=="Semester 1-2"]

# plot a: absorption by module in Semesters 1-2
absorption2$gf_module <- factor(absorption2$gf_module, 
                                      levels = c("TB care and prevention", 
                                                 "Human resources for health, including community health workers",
                                                 "Health management information system and monitoring and evaluation",
                                                 "Multidrug-resistant TB",
                                                 "TB/HIV",
                                                 "Program management"))

absorption2$gf_intervention <- factor(absorption2$gf_intervention,
                                      levels = c("Community TB care delivery",
                                                 "Case detection and diagnosis",
                                                 "Treatment",
                                                 "Prevention",
                                                 "Key populations (TB care and prevention) - Others",
                                                 "Engaging all care providers (TB care and prevention)", 
                                                 "Key populations (TB care and prevention) - Prisoners",
                                                 "Collaborative activities with other programs and sectors (TB care and prevention)",
                                                 "Removing human rights- and gender-related barriers to TB care and prevention",
                                                 "Retention and scale-up of health workers, including for community health workers",
                                                 "Analysis, review and transparency",
                                                 "Routine reporting",
                                                 "Surveys",
                                                 "Treatment: MDR-TB",
                                                 "Case detection and diagnosis: MDR-TB",
                                                 "TB/HIV collaborative interventions",
                                                 "Collaborative activities with other programs and sectors (TB/HIV)",
                                                 "Grant management",
                                                 "Policy, planning, coordination and management of national disease control programs"))

a <- ggplot(absorption2, aes(y=absorption, x=reorder(gf_intervention,desc(gf_intervention)))) + 
  geom_bar(stat='identity', aes(fill = gf_module)) + 
  coord_flip() +
  theme_bw() +
  xlab("Intervention") +
  ylab("Absorption") +
  labs(fill = "Global Fund Module") +
  scale_fill_brewer(palette = "Accent") +
  labs(title = "2018 TB/RSSH Grant Absorption", caption = "Data source: Expenditure from semester 1-2 PUDRs")

# save file
dir <- "C:/Users/frc2/Documents/Deep_dive"
setwd(dir)
jpeg("rplot.jpg", width = 975, height = 425, quality = 100)
a
dev.off()


##### Visualizing donor spending on TB ######

# Read in the data
DT <- fread("C:/Users/frc2/Documents/data/finances/other_dah_actuals_all_sen.csv")

# truncate data to be greater than 2005
DT = DT[year>=2005]

# Subset data to include all HEALTH SPENDING: TB all forms
DT = DT[disease=="tb"]

# combine UN, World Bank, with multilateral organizations
DT$financing_source[DT$financing_source=="UN agencies, The World Bank and other regional development banks"] <- "Multilateral organizations (GAVI, CEPI, UN, The World Bank)"
DT$financing_source[DT$financing_source=="Multilateral organizations (GAVI, CEPI)"] <- "Multilateral organizations (GAVI, CEPI, UN, The World Bank)"

# add in information on domestic spending on TB from FGH
domesticdata <- data.table(year=c(2005, 2006, 2008, 2009, 2013,
                                  2007, 2010, 2011, 2012),
                           financing_source=c("Domestic spending", "Domestic spending", "Domestic spending", "Domestic spending", "Domestic spending",
                                              "Domestic spending", "Domestic spending", "Domestic spending", "Domestic spending"),
                           disease=c("tb", "tb", "tb", "tb", "tb",
                                     "tb", "tb", "tb", "tb"),
                           disbursement=c(171438,336223,499203,462615,12461350,
                                          0, 0, 0, 0))

# merge domestic spending with other TB spending
DT1 <- rbind(DT, domesticdata, fill=TRUE)

# restructure datatable to include summed values for year and financing source
DT1 = DT1[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

DT1 = DT1[order(financing_source, year)] #Order your dataset nicely

# factor financing source
DT1$financing_source <- factor(DT1$financing_source, levels = c("Domestic spending", 
                                                              "Multilateral organizations (GAVI, CEPI, UN, The World Bank)",
                                                              "NGOs and foundations",
                                                              "U.S. bilateral assistance",
                                                              "Other bilateral assistance",
                                                              "The Global Fund"))

b <- ggplot(DT1, aes(x=year,y=disbursement, fill=financing_source)) + 
  geom_area(position = 'stack') +
  labs(title = "TB Spending in Senegal, 2005-2018", 
       caption = "Data source: FGH disbursement data. \n Note: Government health spending on TB is unavailable for 2007, 2010, 2011, and 2012",
       fill="Financing source") +
  scale_y_continuous(labels=dollar) +
  scale_fill_brewer(palette = "Set3") +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()

# this graph does not include government health spending

DT = DT[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]
DT = DT[order(financing_source, year)] #Order your dataset nicely


# factor financing source
DT$financing_source <- factor(DT$financing_source, levels = c("Multilateral organizations (GAVI, CEPI, UN, The World Bank)",
                                                                "NGOs and foundations",
                                                                "U.S. bilateral assistance",
                                                                "Other bilateral assistance",
                                                                "The Global Fund"))

c <- ggplot(DT, aes(x=year,y=disbursement, fill=financing_source)) + 
  geom_area(position = 'stack') +
  labs(title = "TB Spending in Senegal, 2005-2018", 
       caption = "Data source: FGH disbursement data. \n Note: Government health spending on TB for 2013 was 12.46 million USD",
       fill = "Financing source") +
  scale_y_continuous(labels=dollar) +
  scale_fill_brewer(palette = "Set3") +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()

# plot domestic spending on TB
# add in information on domestic spending on TB from FGH
domesticdataavail <- data.table(year=c(2005, 2006, 2008, 2009, 2013),
                           financing_source=c("Domestic spending", "Domestic spending", "Domestic spending", "Domestic spending", "Domestic spending"),
                           disease=c("tb", "tb", "tb", "tb", "tb"),
                           disbursement=c(171438,336223,499203,462615,12461350))

d <- ggplot(domesticdataavail, aes(x=year, y=disbursement)) +
  geom_line() +
  geom_point() +
  labs(title = "Domestic Spending on TB, 2005-2013", 
       caption = "Data source: IHME FGH  data.") +
  scale_y_continuous(labels=dollar) +
  ylab("amount") +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()
