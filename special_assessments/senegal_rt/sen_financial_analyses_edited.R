# Francisco Rios Casas
# Financial Analyses for Senegal
# July 11 2019

# ------
# set-up r
library(data.table)
library(ggplot2)
library(scales)
# -------

# Read in the data
DT <- fread("C:/Users/frc2/Documents/data/finances/other_dah_actuals_all_sen.csv")

# truncate data to be greater than 2005
DT = DT[year>=2005]

# combine UN, World Bank, with multilateral organizations
DT$financing_source[DT$financing_source=="UN agencies, The World Bank and other regional development banks"] <- "Multilateral organizations (GAVI, CEPI, UN, The World Bank)"
DT$financing_source[DT$financing_source=="Multilateral organizations (GAVI, CEPI)"] <- "Multilateral organizations (GAVI, CEPI, UN, The World Bank)"

# factor financing source
DT$financing_source <- factor(DT$financing_source, levels = c("Multilateral organizations (GAVI, CEPI, UN, The World Bank)",
                                                              "NGOs and foundations",
                                                              "U.S. bilateral assistance",
                                                              "Other bilateral assistance",
                                                              "The Global Fund"))

#-------------------------------------------
# Subset the data for different visualizations
#-------------------------------------------

#-------------------------------------------
# DT1: HEALTH SPENDING: TB all forms
#-------------------------------------------
DT1 = DT[disease=="tb"]
DT1 = DT1[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

#-------------------------------------------
# DT2: HEALTH SPENDING: TB-MDR (all interventions)
#-------------------------------------------
DT2 = DT[gf_module=="Multidrug-resistant TB"]
DT2 = DT2[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

#-------------------------------------------
# DT3: HEALTH SPENDING: TB treatment & MDR-Treatment (two interventions)
#-------------------------------------------
DT3 = DT[disease=="tb"]
DT3 = DT3[gf_intervention %in% c("Treatment","Treatment: MDR-TB")]
DT3 = DT3[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

#-------------------------------------------
# DT4: HEALTH SPENDING: TB Diagnosis (TB and TB-MDR) (two interventions)
#-------------------------------------------
DT4 = DT[disease=="tb"]
DT4 = DT4[gf_intervention %in% c("Case detection and diagnosis","Case detection and diagnosis: MDR-TB")]
DT4 = DT4[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

#-------------------------------------------
# DT5: HEALTH SPENDING: Malaria
#-------------------------------------------
DT5 = DT[disease=="malaria"]
DT5 = DT5[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

#-------------------------------------------
# DT6: HEALTH SPENDING: Malaria LLIN
#-------------------------------------------
DT6 = DT[disease=="malaria"]
DT6 = DT6[gf_intervention %in% c("Long lasting insecticidal nets: Continuous distribution","Long lasting insecticidal nets: Mass campaign")]
DT6 = DT6[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

# ------------------------------------------------------
# visualizations
# ------------------------------------------------------

# plot a: TB disease speding from all sources in Senegal, 2005-2018
a <- ggplot(DT1, aes(x=year,y=disbursement)) + 
  geom_area(aes(colour = financing_source, fill=financing_source), position = 'stack') +
  labs(title = "TB Spending in Senegal, 2005-2018", 
       caption = "Data source: FGH disbursement data. \n Note: Government health spending on TB was $10 million in 2016.") +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()

# plot b: stacked area chart of TB-MDR spending in Senegal, 2005-2018
b <- ggplot(DT2, aes(x=year,y=disbursement)) + 
  geom_area(aes(colour = financing_source, fill=financing_source), position = 'stack') +
  labs(title = "TB-MDR Spending in Senegal, 2005-2018", 
       caption = "Data source: Financing Global Health disbursement data. \n Note: Government health spending on TB was $10 million in 2016.") +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()

# plot c: spending for TB treatment and diagnosis in Senegal, 2005-2018
c <- ggplot(DT3, aes(x=year,y=disbursement)) + 
  geom_area(aes(colour = financing_source, fill=financing_source), position = 'stack') +
  labs(title = "TB/TB-MDR Treatment Spending in Senegal, 2005-2018", 
       caption = "Data source: Financing Global Health disbursement data. \n Note: Government health spending on TB was $10 million in 2016.") +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()

# plot d: spending for TB treatment and diagnosis in Senegal, 2005-2018
d <- ggplot(DT4, aes(x=year,y=disbursement)) + 
  geom_area(aes(colour = financing_source, fill=financing_source), position = 'stack') +
  labs(title = "TB/TB-MDR Case Detection and Diagnosis Spending in Senegal, 2005-2018", 
       caption = "Data source: Financing Global Health disbursement data. \n Note: Government health spending on TB was $10 million in 2016.") +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()

# plot e: spending on Malaria in Senegal, 2005-2018
e <- ggplot(DT5, aes(x=year,y=disbursement)) + 
  geom_area(aes(colour = financing_source, fill=financing_source), position = 'stack') +
  labs(title = "Malaria Spending in Senegal, 2005-2018", 
       caption = "Data source: Financing Global Health disbursement data. \n Note: Annual government health spending on malaria averaged $3.2 million between 2005-2016.") +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()

# plot f: spending on Malaria: Vector control (LLIN) in Senegal, 2005-2018
f <- ggplot(DT6, aes(x=year,y=disbursement)) + 
  geom_area(aes(colour = financing_source, fill=financing_source), position = 'stack') +
  labs(title = "Malaria Vector Control (LLIN) Senegal, 2005-2018", 
       caption = "Data source: Financing Global Health disbursement data. \n Note: Annual government health spending on malaria averaged $3.2 million between 2005-2016.") +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()


#-------------------------------------------
# ABSORPTION: TB/RSSH
#-------------------------------------------
absorption <- readRDS("C:/Users/frc2/Documents/data/finances/absorption_sen.rds")
absorption = absorption[grant=="SEN-Z-MOH"]
absorption = absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('gf_module', 'gf_intervention', 'semester')] # Collapse and recalculate absorption 
absorption[, absorption:=(expenditure/budget)*100]
absorption = absorption[order(gf_module, gf_intervention, semester)] #Order your dataset nicely
absorption[order(-absorption)]

# Semester 1 absorption
absorption1 <- absorption[semester=="Semester 1"]

# plot h: absorption by module in Semester 1
absorption1$gf_intervention <- factor(absorption1$gf_intervention, levels = as.vector(absorption1$gf_intervention))
h <- ggplot(absorption1, aes(y=absorption, x=gf_intervention)) + geom_bar(stat='identity', aes(fill = gf_module)) + 
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "2018 TB/RSSH Grant Absorption", caption = "Data source: Semester 1 PUDRs")

# Semester 1-2 absorption
absorption2 <- absorption[semester=="Semester 1-2"]

# plot i: absorption by module in Semesters 1-2
absorption2$gf_intervention <- factor(absorption2$gf_intervention, levels = as.vector(absorption1$gf_intervention))
i <- ggplot(absorption2, aes(y=absorption, x=gf_intervention)) + geom_bar(stat='identity', aes(fill = gf_module)) + 
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "2018 TB/RSSH Grant Absorption", caption = "Data source: Semester 1-2 PUDRs")

#-------------------------------------------
# ABSORPTION: Malaria
#-------------------------------------------
absorption.m <- readRDS("C:/Users/frc2/Documents/data/finances/absorption_sen.rds")
absorption.m = absorption.m[grant=="SEN-M-PNLP"]
absorption.m = absorption.m[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('gf_module', 'gf_intervention', 'semester')] # Collapse and recalculate absorption 
absorption.m = absorption.m[budget!=0]# remove rows where budget is equal to zero 
absorption.m[, absorption:=(expenditure/budget)*100]
absorption.m = absorption.m[order(gf_module, gf_intervention, semester)] #Order your dataset nicely
absorption.m[order(-absorption)]

# Semester 1 absorption
absorption1.m <- absorption.m[semester=="Semester 1"]

# plot j: absorption by module in Semester 1
absorption1.m$gf_intervention <- factor(absorption1.m$gf_intervention, levels = as.vector(absorption1.m$gf_intervention)) # factor variable to plot in same order
j <- ggplot(absorption1.m, aes(y=absorption, x=gf_intervention)) + geom_bar(stat='identity', aes(fill = gf_module)) + 
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "2018 Malaria Grant Absorption", caption="Data source: Semester 1 PUDRs.")

# Semester 1-2 absorption
absorption2.m <- absorption.m[semester=="Semester 1-2"]

# plot K: absorption by module in Semesters 1-2
absorption2.m$gf_intervention <- factor(absorption2.m$gf_intervention, levels = as.vector(absorption2.m$gf_intervention)) # factor variable to plot in same order
k <- ggplot(absorption2.m, aes(y=absorption, x=gf_intervention)) + geom_bar(stat='identity', aes(fill = gf_module)) + 
 coord_flip() +
 theme_bw() +
 guides(fill = guide_legend(reverse = TRUE))+
 labs(title = "2018 Malaria Grant Absorption", caption="Data source: Semester 1-2 PUDRs.")

# plot K: absorption by module in Semesters 1-2

# remove the data points with absorption above 200%
absorption3.m <- absorption2.m[absorption<200]

l <- ggplot(absorption3.m, aes(y=absorption, x=gf_intervention)) + geom_bar(stat='identity', aes(fill = gf_module)) + 
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "2018 Malaria Grant Absorption", caption="Data source: Semester 1-2 PUDRs. \n Values above 200% removed.")

# --------------
# SAVE
outFile = "Senegal_financial_graphs_07162019.pdf"

# select where you want to save the output
dir = "J:/Project/Evaluation/GF/resource_tracking/visualizations/random/sen/TB_malaria"
setwd(dir = dir)

# create pdf with graphs
pdf(outFile, height = 8, width=16)
a
b
c
d
e
f
h
i
j
k
l
dev.off()
# -------------