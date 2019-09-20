#----------------------------------------
# Audrey Batzel
# 6/28/2019
# consistency checks and fixes and then graph combined sigsa/sigpro data
#----------------------------------------

#-----------------------
# Install packages 
# ----------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(RColorBrewer)
library(gridExtra)
# ----------------------

#----------------------------------------
# Set up directories 
#----------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set working and output directories
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/gtm/hiv/')

# input
combined_data = paste0(dir, "prepped/combined_sigsa_sigpro.rds")

# output
outFile = paste0(dir, 'prepped/combined_sigsa_sipro_corrected_for_graphing.rds')

# output files
natl_trends = paste0(dir, 'visualizations/natl_time_series_hiv_testing_edited.pdf')
natl_by_pop = paste0(dir, 'visualizations/natl_time_series_hiv_testing_by_pop.pdf')
natl_key_pops = paste0(dir, 'visualizations/natl_hiv_testing_key_populations_dateSubset.pdf')
remake_natl_by_pop = paste0(dir, 'visualizations/natl_time_series_hiv_testing_by_pop_facetedVar.pdf')
remake_natl_by_pop_subset_dates = paste0(dir, 'visualizations/natl_time_series_hiv_testing_by_pop_facetedVar2.pdf')

# read in combined data file:
dt = readRDS(combined_data)
#----------------------------------------

#----------------------------------------
# # set up for graphing / additional prep
# #----------------------------------------
# dt[, trans := ifelse(gender == "Trans", TRUE, FALSE) ]
# dt[pop == 'trans', pop := NA]
# 
# dt[, hiv_test_comp := as.numeric(hiv_test)]
# dt[, hiv_confirmatoryTest_comp := as.numeric(hiv_confirmatoryTest)]
# dt[, hiv_positive := ifelse(hiv_testResult == 'reactive', TRUE, FALSE) ]
# dt[, hiv_confirmatoryTest_positive := ifelse(hiv_confirmatoryTestResult == '1', TRUE, FALSE) ]
# #----------------------------------------
# 
# #----------------------------------------
# # data checks / consistency checks:
# #----------------------------------------
# # data variable checks:
# dt[ is.na(date), .N ]
# dt[ is.na(date), .N, by = .(set) ]
# dt[ is.na(date) & hiv_test == 1, .N ]
# dt[ is.na(date) & hiv_test == 1 & hiv_positive == TRUE, .N ]
# 
# # hiv testing consistency checks:
# # where hiv_test is missing:
# dt[ is.na(hiv_test), .N ]
# dt[ is.na(hiv_test) & !is.na(hiv_positive), .N, by = .(hiv_testResult)]
# dt[ is.na(hiv_test) & hiv_positive == TRUE, .N]
#   # where hiv test was originally NA and hiv_testResult was nonreactive, set hiv_test_comp to 1/TRUE
#   dt[ is.na(hiv_test) & hiv_testResult == "nonreactive", hiv_test_comp := 1]
# 
# # where hiv_test was not completed
# dt[ hiv_test == 0, .N, by = .(hiv_testResult)]
#   # where hiv test was originally 0 but hiv_testResult indicated otherwise, set hiv_test_comp to be 1/TRUE
#   dt[ hiv_test == 0 & hiv_testResult %in% c('reactive', 'nonreactive', 'indeterminate'), hiv_test_comp := 1 ]
# 
# # hiv confirmatory testing consistency checks:
# dt[, unique(hiv_confirmatoryTest), by = .(set)] # only in sigsa data
# 
# dt[is.na(hiv_test), .N, by = .(hiv_confirmatoryTest, hiv_confirmatoryTestResult)]
#   # Fix inconsistencies:
#   dt[is.na(hiv_test) & hiv_confirmatoryTest == 1 & hiv_confirmatoryTestResult == 1, hiv_positive := TRUE]
#   dt[is.na(hiv_test) & hiv_confirmatoryTest == 1 & hiv_confirmatoryTestResult == 1, hiv_test_comp := 1]
# 
# dt[hiv_test == 0, .N, by = .(hiv_confirmatoryTest, hiv_confirmatoryTestResult)]
#   # Fix inconsistencies:
#   dt[hiv_test == 0 & hiv_confirmatoryTest == 1 & hiv_confirmatoryTestResult == 1, hiv_positive := TRUE]
#   dt[hiv_test == 0 & hiv_confirmatoryTest == 1 & hiv_confirmatoryTestResult == 1, hiv_test_comp := 1]
# 
# dt[ hiv_confirmatoryTest == 0 , .N, by = .(hiv_confirmatoryTestResult)]
# dt[ is.na(hiv_confirmatoryTest), .N, by = .(hiv_confirmatoryTestResult)]
#   # Fix inconsistencies:
#   dt[ (hiv_confirmatoryTest == 0 | is.na(hiv_confirmatoryTest)) & hiv_confirmatoryTestResult == 1, hiv_positive := TRUE]
#   dt[ (hiv_confirmatoryTest == 0 | is.na(hiv_confirmatoryTest)) & hiv_confirmatoryTestResult == 1, hiv_test_comp := 1]
# 
# dt[ hiv_confirmatoryTestResult == 0 & hiv_positive == TRUE, .N]
# dt[ hiv_confirmatoryTestResult == 0 & hiv_positive == TRUE, hiv_positive := FALSE]
# 
# # remove missing data for analyses
# dt = dt[!is.na(date)]
# dt = dt[!is.na(hiv_test)]
# dt = dt[ hiv_test != 0 ] # now the data is all of the data for tests completed
# 
# dt[ hiv_test == 1, .N ]
# dt[ hiv_positive == TRUE, .N]
# (dt[ hiv_positive == TRUE, .N] / dt[ hiv_test == 1, .N ]) * 100
# 
# # confirmatory test completion:
# dt[ hiv_positive == FALSE, .N, by = .(hiv_confirmatoryTest_comp)]
# dt[ hiv_positive == TRUE, .N, by = .(hiv_confirmatoryTest_comp)]
# 
# saveRDS(dt, outFile)

dt = readRDS(outFile)
#----------------------------------------


#----------------------------------------
# graphing at national level
#----------------------------------------
# get national level data:
#-----------------------
natl = dt[ ,.(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
              hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
           by=.(date, pop, trans)]
natl[grepl(pop, pattern = 'msm'), msm := TRUE]
natl[ is.na(msm), msm := FALSE]

natl_long = melt.data.table(natl, id.vars = c('date', 'pop', 'trans', 'msm'))
natl[, pos_rate := ((hiv_positive / hiv_test_comp) *100)]

all_patients = dt[ ,.(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
                      hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
                   by=.(date)]

all_patients_long = melt.data.table(all_patients, id.vars = c('date'))
all_patients[, pos_rate := ((hiv_positive / hiv_test_comp) *100)]
#-----------------------
# National time series, all patients
#-----------------------
# OPEN PDF
pdf(natl_trends, height = 9, width = 11)

# Tests performed and tests positive
ggplot(all_patients_long[date >= "2014-01-01"], aes(x=date, y=value, color=variable)) +
  geom_point() + geom_line() + theme_bw() +
  labs(title = "HIV tests and positive HIV tests, all patients",
       x='Date', y='Count', color="") +
  theme(text = element_text(size=18)) + facet_wrap(~variable, scales = 'free')

ggplot(all_patients_long[date >= "2015-01-01" & date <= "2017-12-01"], aes(x=date, y=value, color=variable)) +
  geom_point() + geom_line() + theme_bw() +
  labs(title = "HIV tests and positive HIV tests, all patients",
       x='Date', y='Count', color="") +
  theme(text = element_text(size=18)) + facet_wrap(~variable, scales = 'free')

# Test positivity rate
ggplot(all_patients[date >= "2014-01-01"], aes(x=date, y=pos_rate)) +
  geom_point() + geom_line() + theme_bw() +
  labs(title = "HIV test positivity rate, all patients",
       x='Date', y='Percent', color="") +
  theme(text = element_text(size=18))

ggplot(all_patients[date >= "2015-01-01" & date <= "2017-12-01"], aes(x=date, y=pos_rate)) +
  geom_point() + geom_line() + theme_bw() +
  labs(title = "HIV test positivity rate, all patients",
       x='Date', y='Percent', color="") +
  theme(text = element_text(size=18))

# CLOSE PDF
dev.off()
#-----------------------
# National time series, by pop and gender identity 
#-----------------------
# OPEN PDF:
pdf(natl_by_pop, height = 9, width = 11)

# sum by pop:
pop = natl[ ,.(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
              hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
           by=.(date, pop)]

pop_long = melt.data.table(pop, id.vars = c('date', 'pop'))
pop[, pos_rate := ((hiv_positive / hiv_test_comp) *100)]

ggplot(pop_long[date >= "2014-01-01"], aes(x=date, y=value, color=variable)) +
  geom_point() + geom_line() + theme_bw() +
  labs(title = "HIV tests and positive HIV tests, by population",
       x='Date', y='Count', color="") +
  theme(text = element_text(size=18)) +
  facet_wrap( ~pop, scales = "free")

ggplot(pop[date >= "2015-01-01" & date <= "2017-12-01"], aes(x=date, y=pos_rate, color = pop)) +
  geom_point() + geom_line() + theme_bw() +
  labs(title = "HIV test positivity rate, by population",
       x='Date', y='Percent', color="") +
  theme(text = element_text(size=18))
#-----------------------
# sum by trans:
trans = natl[ ,.(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
               hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
            by=.(date, trans)]

trans_long = melt.data.table(trans, id.vars = c('date', 'trans'))
trans[, pos_rate := ((hiv_positive / hiv_test_comp) *100)]

ggplot(trans_long[date >= "2014-01-01"], aes(x=date, y=value, color=variable)) +
  geom_point() + geom_line() + theme_bw() +
  labs(title = "HIV tests and positive HIV tests, by whether or not patients are transgender",
       x='Date', y='Count', color="") +
  theme(text = element_text(size=18)) +
  facet_wrap( ~trans, scales = "free")

ggplot(trans[date >= "2015-01-01" & date <= "2017-12-01"], aes(x=date, y=pos_rate, color = trans)) +
  geom_point() + geom_line() + theme_bw() +
  labs(title = "HIV test positivity rate, by whether or not patients are transgender",
       x='Date', y='Percent', color="") +
  theme(text = element_text(size=18))

# CLOSE PDF
dev.off()
#-----------------------
# National time series of tests performed and positivity rate for MSM, transgender persons, sex workers, prisoners, and migrants 
#-----------------------
trans = trans[ trans == TRUE, ]
trans[ , pop := "Transgender people"]

all_patients[, pop := "All patients"]

msm = dt[grepl(pop, pattern = 'msm'),
         .(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
           hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
         by=.(date)]
msm[, pop := "MSM"]
csw = dt[grepl(pop, pattern = 'csw'),
         .(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
           hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
         by=.(date)]
csw[, pop := "Commercial sex workers"]
prisoners = dt[grepl(pop, pattern = 'prisoner'),
               .(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
                 hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
               by=.(date)]
prisoners[, pop := "Prisoners"]
migrants = dt[grepl(pop, pattern = 'migrant'),
              .(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
                hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
              by=.(date)]
migrants[, pop := "Migrants"]

dt = rbindlist( list(trans, all_patients, msm, csw, prisoners, migrants), use.names = TRUE, fill = TRUE)
dt[, trans := NULL]
dt[, pos_rate := ((hiv_positive / hiv_test_comp) *100)]
dt_long = melt.data.table(dt, id.vars = c('date', 'pop'))
dt_long[ variable == 'hiv_test_comp', variable := 'Tested for HIV']
dt_long[ variable == 'hiv_positive', variable := 'HIV positive']
dt_long[ variable == 'pos_rate', variable := 'Percent positive (%)']
dt_long = dt_long[ date >= '2015-01-01' & date <= '2017-12-01', ]

# OPEN PDF
pdf(natl_key_pops, height = 9, width = 11)
# loop through pops and make figures:
for (x in unique(dt[pop != "All patients", pop])) {
  
  pop_name = as.character(x)
  
  p1 = ggplot(dt_long[pop==x & variable!="Percent positive (%)"], aes(x=date, y=value, color=variable)) +
    geom_point() + geom_line() + theme_bw() +
    scale_color_manual(values=c('#feb24c', '#fb6a4a')) +
    labs(title=paste0('Tests and HIV+ tests: ', pop_name), color="", x='Date', y="Count")
  
  p2 = ggplot(dt_long[pop==x & variable!="Percent positive (%)"], aes(x=date, y=value, fill=variable)) +
    geom_bar(position = "fill",stat = "identity") +
    labs(title=paste0('Percentage of HIV tests that were positive: \n', pop_name), x='Date', y = 'Ratio', fill="") +
    scale_fill_manual(values = c('#feb24c', '#fb6a4a')) +
    theme_bw() + theme(axis.text.x=element_text(angle=90))
  
  p3 = ggplot(dt_long[pop==x & variable=="Percent positive (%)"], aes(x=date, y=value, color=variable)) +
    geom_point() + geom_line() + theme_bw() +
    scale_color_manual(values='#8c96c6') + guides(color=FALSE) + guides(fill=FALSE) +
    labs(title=paste0('Percentage of HIV tests that were positive: \n', pop_name), color="", x='Date', y="Percent (%)") 
  
  p4 = ggplot(dt_long[pop=='All patients' & variable=="Percent positive (%)"], aes(x=date, y=value, color=variable)) +
    geom_point() + geom_line() + theme_bw() +
    scale_color_manual(values='#d73027') + guides(color=FALSE) +
    labs(title=paste0('Percentage of HIV tests that were positive: All patients'), color="", x='Date', y=" ")
  
  grid.arrange(p1, p2, p3, p4, nrow=2) }
# CLOSE PDF
dev.off()
#----------------------------------------

#----------------------------------------
# remake natl figures by pop
#----------------------------------------
# create a year variable
dt[, year := year(date)]
# subset data
dt = dt[,.(match_dept, year, date, hiv_test_comp, hiv_positive, hiv_confirmatoryTest_comp, hiv_confirmatoryTest_positive, pop, trans, pregnant, pueblo, linguistic_community, nationality, sr)]
# identify kvps 
dt[ , msm := ifelse(grepl(pop, pattern = 'msm'),TRUE,FALSE)]
dt[ , csw := ifelse(grepl(pop, pattern = 'csw'),TRUE,FALSE)]
dt[ , prisoner := ifelse(grepl(pop, pattern = 'prisoner'),TRUE,FALSE)]
dt[ , migrant := ifelse(grepl(pop, pattern = 'migrant'),TRUE,FALSE)]
dt[ , military := ifelse(grepl(pop, pattern = 'military'),TRUE,FALSE)]
dt[, pop := NULL]
# sum data across nation by date
id_vars = c('date')

sum_by_kvp = function( var ){
  dt2 = dt[ get(var) == TRUE, .(hiv_test_comp=sum(hiv_test_comp, na.rm = TRUE),
                                hiv_positive=sum(hiv_positive, na.rm = TRUE)), 
            by=c(id_vars)] 
  dt2_long = melt.data.table(dt2, id.vars = id_vars)
}
preg_long = sum_by_kvp('pregnant')
migr_long = sum_by_kvp('migrant')
trans_long = sum_by_kvp('trans')
pris_long = sum_by_kvp('prisoner')
msm_long = sum_by_kvp('msm')
csw_long = sum_by_kvp('csw')
mili_long = sum_by_kvp('military')
#--------------------
# maps by year and variable
list_of_dts = list("All people" = all_patients_long, "Transgender persons" = trans_long, "Migrants" = migr_long, "Prisoners" = pris_long,
                   "MSM" = msm_long, "CSWs" = csw_long, "Pregnant Women" = preg_long, "Military" = mili_long)

pdf(remake_natl_by_pop, height = 9, width = 11) 
for (x in 1:length(list_of_dts)){
  title = names(list_of_dts[x])
  
  print(ggplot(list_of_dts[[x]][date >= "2014-01-01"], aes(x=date, y=value, color=variable)) +
    geom_point() + geom_line() + theme_bw() +
    labs(title = paste0("HIV tests and positive HIV tests: ", title),
         x='Date', y='Count', color="") +
    theme(text = element_text(size=18)) +
    facet_wrap( ~variable, scales = "free") + guides(color = FALSE))
}  
dev.off()

