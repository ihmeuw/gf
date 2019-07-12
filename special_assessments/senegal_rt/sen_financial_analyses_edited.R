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
# DT1: HEALTH SPENDING: TB all forms
#-------------------------------------------
DT1 = DT[disease=="tb"]
DT1 = DT1[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

#-------------------------------------------
# DT2: HEALTH SPENDING: TB-MDR
#-------------------------------------------
DT2 = DT[gf_module=="Multidrug-resistant TB"]
DT2 = DT2[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

#-------------------------------------------
# DT3: HEALTH SPENDING: TB treatment & MDR-Treatment
#-------------------------------------------
DT3 = DT[disease=="tb"]
DT3 = DT3[gf_intervention %in% c("Treatment","Treatment: MDR-TB")]
DT3 = DT3[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

#-------------------------------------------
# DT4: HEALTH SPENDING: TB Diagnosis (TB and TB-MDR)
#-------------------------------------------
DT4 = DT[disease=="tb"]
DT4 = DT4[gf_intervention %in% c("Case detection and diagnosis","Case detection and diagnosis: MDR-TB")]
DT4 = DT4[, .(disbursement=sum(disbursement, na.rm=T)), by=c('year', 'financing_source')]

# ------------------------------------------------------
# visualizations
# ------------------------------------------------------

# Color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# plot all TB disease spendingof all sources in Senegal, 2005-2018
b <- ggplot(DT1, aes(x=year,y=disbursement)) + 
  geom_area(aes(colour = financing_source, fill=financing_source), position = 'stack') +
  labs(title = "TB Spending in Senegal, 2005-2018") +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()

# plot stacked area chart of TB-MDR spending in Senegal, 2005-2018
c <- ggplot(DT2, aes(x=year,y=disbursement)) + 
  geom_area(aes(colour = financing_source, fill=financing_source), position = 'stack') +
  labs(title = "TB-MDR Spending in Senegal, 2005-2018") +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()

# plot spending for TB treatment and diagnosis in Senegal, 2005-2018
c2 <- ggplot(DT3, aes(x=year,y=disbursement)) + 
  geom_area(aes(colour = financing_source, fill=financing_source), position = 'stack') +
  labs(title = "TB/TB-MDR Treatment Spending in Senegal, 2005-2018") +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  theme_bw()

# plot spending for TB treatment and diagnosis in Senegal, 2005-2018
c3 <- ggplot(DT4, aes(x=year,y=disbursement)) + 
  geom_area(aes(colour = financing_source, fill=financing_source), position = 'stack') +
  labs(title = "TB/TB-MDR Case Detection and Diagnosis Spending in Senegal, 2005-2018") +
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

# plot absorption by module in Semester 1
absorption1$gf_intervention <- factor(absorption1$gf_intervention, levels = as.vector(absorption1$gf_intervention))
e <- ggplot(absorption1, aes(y=absorption, x=gf_intervention)) + geom_bar(stat='identity', aes(fill = gf_module)) + 
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "2018 Semester 1 TB/RSSH Absorption")

# Semester 1-2 absorption
absorption2 <- absorption[semester=="Semester 1-2"]

# plot absorption by module in Semesters 1-2
absorption2$gf_intervention <- factor(absorption2$gf_intervention, levels = as.vector(absorption1$gf_intervention))
f <- ggplot(absorption2, aes(y=absorption, x=gf_intervention)) + geom_bar(stat='identity', aes(fill = gf_module)) + 
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "2018 Semester 1-2 TB/RSSH Absorption")

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

# plot absorption by module in Semester 1
absorption1.m$gf_intervention <- factor(absorption1.m$gf_intervention, levels = as.vector(absorption1.m$gf_intervention))
g <- ggplot(absorption1.m, aes(y=absorption, x=gf_intervention)) + geom_bar(stat='identity', aes(fill = gf_module)) + 
  coord_flip() +
  theme_bw() +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title = "2018 Semester 1 Malaria Absorption")

# there is no absorption for Semesters 1-2 for malaria

# Semester 1-2 absorption
# absorption2.m <- absorption.m[semester=="Semester 1-2"]

# plot absorption by module in Semesters 1-2
# absorption2.m$gf_intervention <- factor(absorption2.m$gf_intervention, levels = as.vector(absorption2.m$gf_intervention))
#h <- ggplot(absorption2.m, aes(y=absorption, x=gf_intervention)) + geom_bar(stat='identity', aes(fill = gf_module)) + 
 # coord_flip() +
 #  theme_bw() +
 #  guides(fill = guide_legend(reverse = TRUE))+
 #  ggtitle("2018 Semester 1-2 Malaria Absorption")

# --------------
# SAVE
outFile = "Senegal_financial_graphs_07112019.pdf"
pdf(outFile, height = 7, width=16)
b
c
c2
c3
e
f
g
dev.off()
# -------------