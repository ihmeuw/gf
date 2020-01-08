# AUTHOR: Emily Linebarger 
# PURPOSE: Produce absorption graphics for 2018 synthesis report. 
# adapted in January 2020 to help with analyses for 2019 synthesis report. 

rm(list=ls())
library(data.table)
library(ggplot2)
library(doBy)
library(Hmisc)
library(readxl)

#---------------------------------------------
# PART 3: Generate combined graph using 
#         consortia's numbers 
#---------------------------------------------
# 
# # gs_gap() %>%
# #   gs_copy(to = "MyDrive")
# malaria <- data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/Synthesis 2018/Absorption Table.xlsx", sheet="Malaria"))
# malaria$grant_disease = 'malaria'
# hiv <- data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/Synthesis 2018/Absorption Table.xlsx", sheet="HIV"))
# hiv$grant_disease = 'hiv'
# tb <- data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/Synthesis 2018/Absorption Table.xlsx", sheet="TB"))
# tb$grant_disease = 'tb'
# hivtb <- data.table(read_xlsx("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/Synthesis 2018/Absorption Table.xlsx", sheet="HIV-TB"))
# hivtb$grant_disease = 'hivtb'
# #--------------------------------------
# # Manually correct data entry errors
# #--------------------------------------
# prepped = rbindlist(list(malaria, hiv, tb, hivtb), fill=T)
# 
# # Fix grant names
# prepped[grant_disease=="malaria" & grant=="GRANT 1 (Q1-Q2 2018)", grant:="SEN-M-PNLP"]
# prepped[grant_disease=="hiv" & grant=="GRANT 1 (Q1-Q2 2018) ONLY FOR ANCS (CIVIL SOCIETY)", grant:="SEN-H-ANCS"]
# prepped[grant_disease=="hiv" & grant=="GRANT 2 (Q1-Q2 2018) MOH", grant:="SEN-H-CNLS"]
# prepped[grant_disease=="tb" & grant=="GRANT 1 (Q1-Q2 2018)", grant:="SEN-Z-MOH"]
# 
# # Add country variable
# prepped$country <- substring(prepped$grant, 1, 3)
# stopifnot(length(unique(prepped$country))==9)
# 
# # Fix modules
# prepped$gf_module = ifelse(prepped$gf_module == "Program Management", "Program management", prepped$gf_module)
# prepped$gf_module = ifelse(prepped$gf_module == "TB/HIV (Labelled as HIV)" | prepped$gf_module == "TB/HIV (Labelled as TB)", "TB/HIV", prepped$gf_module)
# 
# 
# saveRDS(prepped, "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/Synthesis 2018/prepped_2018_data.rds")
#-----------------------------------------------------------
# Calculate observed absorption across module and across country 
#-----------------------------------------------------------

prepped = readRDS("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/Synthesis 2018/prepped_2018_data.rds")

absorption_by_module = prepped[, .(gf_module, disease, absorption_q1_2018)]
absorption_by_module <- melt(absorption_by_module, id = c("gf_module", "disease")) 
absorption_by_module$value <- as.numeric(absorption_by_module$value)
setDT(absorption_by_module)
absorption_by_module<-dcast(absorption_by_module, gf_module+disease~variable, fun = list(min, median, mean, max))
absorption_by_module = absorption_by_module[, .(min =value_min_absorption_q1_2018, max = value_max_absorption_q1_2018, median = value_median_absorption_q1_2018, mean = value_mean_absorption_q1_2018, gf_module = gf_module, disease = disease) ]

absorption_by_country = prepped[, .(gf_module, disease, country, absorption_q1_2018)]
absorption_by_country <- melt(absorption_by_country, id = c("gf_module", "disease", "country")) 
absorption_by_country$value <- as.numeric(absorption_by_country$value)
setDT(absorption_by_country)
absorption_by_country<-dcast(absorption_by_country, gf_module+disease+country~variable, fun = list(min, median, mean, max))
absorption_by_country = absorption_by_country[, .(min =value_min_absorption_q1_2018, max = value_max_absorption_q1_2018, median = value_median_absorption_q1_2018, mean = value_mean_absorption_q1_2018, gf_module = gf_module, disease = disease, country = country) ]
 
setDT(prepped)
absorption_by_disease <- prepped[, .(mean = mean(absorption_q1_2018), median = median(absorption_q1_2018)), by = 'disease']

absorption_by_country_hiv <- absorption_by_country[disease == "hiv"]
absorption_by_country_hiv <- absorption_by_country_hiv[, .(country, gf_module, mean)]
absorption_by_country_hiv <- melt(absorption_by_country_hiv, id = c("gf_module", "country"))
setDT(absorption_by_country_hiv)
absorption_by_country_hiv <- dcast(absorption_by_country_hiv, gf_module+country~variable, fun=mean)

#-----------------------------------------------------------
# Calculate which modules have had the lowest absorption 
#   on average across country and disease. 
#-----------------------------------------------------------
zero_country_absorption <- absorption_by_country[max == 0]
for (col in c('min', 'mean', 'median', 'max')){
  zero_country_absorption[, col] <- gsub("[^0-9\\.]", "", zero_country_absorption[[col]])
  zero_country_absorption[, col] <- as.numeric(zero_country_absorption[[col]])
}
setDT(zero_country_absorption)
zero_absorption_hiv <- zero_country_absorption[disease == "hiv"]
zero_absorption_malaria <- zero_country_absorption[disease == "malaria"]
zero_absorption_tb <- zero_country_absorption[disease == "tb"]

zero_absorption <- list(zero_absorption_hiv, zero_absorption_malaria, zero_absorption_tb)
for (i in 1:length(zero_absorption)){
  zero_absorption[[i]] <- zero_absorption[[i]][, c('gf_module', 'min')] #Only need gf_module and one variable
  zero_absorption[[i]] <- melt(zero_absorption[[i]], id = c("gf_module")) #Just melt on gf_module to see which modules are the most frequent
  zero_absorption[[i]]$min <- as.numeric(zero_absorption[[i]]$min)
  zero_absorption[[i]]<-dcast(zero_absorption[[i]], gf_module~variable, fun = length)
  zero_absorption[[i]] <- zero_absorption[[i]][order(min)]
}

zero_absorption_hiv <- zero_absorption[1]
zero_absorption_malaria <- zero_absorption[2]
zero_absorption_tb <- zero_absorption[3]

#-----------------------------------------------------------
# Universally low absorption numbers (high counts of modules in zero_absorption list above) 
#   on average across country and disease. 
#-----------------------------------------------------------
zero_country_absorption <- zero_country_absorption[, c("gf_module", "min")]
zero_country_absorption <- melt(zero_country_absorption, id = c("gf_module"))
zero_country_absorption <- dcast(zero_country_absorption, gf_module~variable, fun = length)

zero_country_absorption <- zero_country_absorption[order(min)]
overall_low_absorption <- tail(zero_country_absorption, 3)
overall_high_absorption <- head(zero_country_absorption, 3)

overall_low_absorb_mods <- overall_low_absorption[["gf_module"]]


#Calculate modules with the highest and lowest absorption by disease
absorption_by_module<- absorption_by_module[order(disease, mean)]

absorption_by_module_hiv <- absorption_by_module[disease == "hiv"]
absorption_by_module_tb <- absorption_by_module[disease == "tb"]
absorption_by_module_malaria <- absorption_by_module[disease == "malaria"]

#Lowest absorption
low_absorption_hiv = head(absorption_by_module_hiv, 2)
low_absorption_tb = head(absorption_by_module_tb, 2)
low_absorption_malaria = head(absorption_by_module_malaria, 2)

#Highest absorption
high_absorption_hiv = tail(absorption_by_module_hiv, 2)
high_absorption_tb = tail(absorption_by_module_tb, 2)
high_absorption_malaria = tail(absorption_by_module_malaria, 2)

high_absorb_hiv_mods <-high_absorption_hiv[["gf_module"]]
#One special calculation for HIV- find the countries with the minimum and maximum absorption for high absorbing hiv mods. 

high_absorb_hiv_country = absorption_by_country_hiv[gf_module %in% high_absorb_hiv_mods, max(mean), by = country]
low_absorb_hiv_country = high_absorb_hiv_country[V1 == min(V1)]
low_absorb_hiv_country = low_absorb_hiv_country[[1]]
high_absorb_hiv_country = high_absorb_hiv_country[V1 == max(V1)]
high_absorb_hiv_country = high_absorb_hiv_country[[1]]

standard_round = function(x) {
  x = round(x, 1)
  return(x)
  
}

#-----------------------------------------------------------
# Generate point-range graphic  
#-----------------------------------------------------------

#Organize data for mapping 

#Function to remove outliers greater than 200% absorption 
# remove_outliers <- function(dt, var_with_outliers){
#   setDT(dt)
#   return(dt[var_with_outliers < 200]) 
# }

#Merge with abbreviated module for a cleaner plot 
absorption_by_module = absorption_by_module[order(-gf_module)]

#Find illegitimate grants (cases where no absorption for q1 was reported)
illegitimate_grants <- prepped[, .(grant, absorption_q1_2018)]
illegitimate_grants <- melt(illegitimate_grants, id = "grant")
illegitimate_grants <- dcast(illegitimate_grants, grant~variable, fun = max)
illegitimate_grants <- illegitimate_grants[absorption_q1_2018 == 0, .(grant)]
illegitimate_grants <- illegitimate_grants$grant

#Add in a count of how many countries are reporting for each module
country_counts <- prepped[grant %nin% illegitimate_grants, .(gf_module, country, disease, budget, absorption_q1_2018)] #Remove grants that had no absorption reported, because we don't have PUDRs for these grants. 
country_counts <- country_counts[budget!=0] #Don't want to count modules where modules were 0. 
country_counts <- unique(country_counts[, .(gf_module, country, disease)])
country_counts$count <- 1

check_prepped <- unique(prepped[, .(gf_module), by = c("disease", "country")])
check_prepped$count <- 1
check_prepped <- dcast(check_prepped, gf_module~disease, value.var = "count", fun = sum)
countries_missing <- dcast(country_counts, gf_module~country, value.var = "count", fun = sum)

country_counts <- dcast(country_counts, gf_module~disease, value.var = "count", fun = sum)
country_counts <- country_counts[!(hiv == 0 & malaria == 0) & !(tb == 0 & malaria == 0) & !(hiv == 0 & tb == 0), label:=paste0(hiv, ",", malaria, ",", tb)]
country_counts <- country_counts[(hiv == 0 & malaria == 0) | (tb == 0 & malaria == 0) | (hiv == 0 & tb == 0), label:=paste0(abs(hiv-malaria-tb))]

#----------------------------------------------------
# I think this might be best to add in as a footnote? 
#----------------------------------------------------

#Add in abbreviated modules for easier plot viewing. 
abbrev_module_map = read.csv("I:/RTs_and_Projects/Evaluations/Global Fund PCE/Reports/Synthesis Reports/2018/abbrev_module_mapping.csv")
plot_data = merge(absorption_by_module, abbrev_module_map, by = c("gf_module"), all.x = T)
plot_data = merge(plot_data, country_counts, by = "gf_module", all.x = T)
targeted_modules <- c("Financial management", "Community responses and systems", "Info systems & M&E", "Program management", "PSM", "HR & health workers", "National health strategies", "Service delivery")
plot_data$priority_mod = ifelse(plot_data$abbrev_module %in% targeted_modules, 0, 2)
plot_data = plot_data[order(priority_mod)]

plot_data[abbrev_module == "Service delivery", abbrev_module:="Integrated service delivery/QI"]
plot_data[priority_mod == FALSE, abbrev_module:=paste0("RSSH: ", abbrev_module)]
plot_data[abbrev_module == "RSSH: Program management", abbrev_module:="Program management"]
plot_data[abbrev_module=="Program management", priority_mod:=1]
plot_data[abbrev_module == "HR & health workers", abbrev_module:="Human resources and health workers"]

# #Collapse modules further - new tweak added for synthesis slides February 2019 
# plot_data[grep("RSSH", abbrev_module), abbrev_module:='RSSH']
# plot_data[grep("prevention", tolower(abbrev_module)), abbrev_module:='Prevention']
# plot_data = plot_data[!is.na(abbrev_module)] #Something isn't merging correctly above. 

plot_data$abbrev_mod_nolab <- plot_data$abbrev_module
plot_data[, abbrev_module:=paste0(abbrev_module, " (", label, ")")]

#Trim outliers 
plot_data$max = ifelse(plot_data$max > 200, 200, plot_data$max)
plot_data$mean = ifelse(plot_data$mean > 200, 200, plot_data$mean)

#Capitalize diseases for facet wrapping
plot_data$disease = ifelse(plot_data$disease == "hiv", "HIV", plot_data$disease)
plot_data$disease = ifelse(plot_data$disease == "malaria", "Malaria", plot_data$disease)
plot_data$disease = ifelse(plot_data$disease == "tb", "TB", plot_data$disease)

print(nrow(absorption_by_module) - nrow(plot_data)) #0 observations being dropped at the moment. 

#plot_data$abbrev_module <- factor(plot_data$abbrev_module, levels = plot_data$abbrev_module[order(plot_data$abbrev_module, plot_data$priority_mod)])
plot_data[disease == "HIV", point_scale:=hiv]
plot_data[disease == "TB", point_scale:=tb]
plot_data[disease == "Malaria", point_scale:=malaria]

plot_data[, point_scale:=point_scale/100]

# cols = c('min', 'max', 'mean')
# plot_data1 = plot_data[, lapply(.SD, mean), .SDcols = cols, by= c('abbrev_module', 'priority_mod', 'disease')]

#Removing strange NA's from plot data - these were not here in original version of synthesis report? 
plot_data = plot_data[!gf_module == 'as']

#Plot 
ggplot(data = plot_data, aes(x = reorder(abbrev_module, priority_mod), color = disease)) + 
  geom_pointrange(mapping=aes(y=mean, ymin=min, ymax=max)) + 
  geom_point(aes(y = mean, size = point_scale)) +
  theme_bw(base_size = 16) + theme(legend.position = 'none', text = element_text(size=24)) + 
  coord_flip() + 
  labs(y = "Absorption Q1-Q2 2018 (%)", x = "Module", caption = "*Obervations with absorption > 200% not displayed.\nPoints represent average absorption across country/disease, with range showing min and max. \nParentheses show number of countries (out of 8) with Q1-Q2 absorption data for each disease.") + 
  facet_wrap(vars(disease))

ggsave("I:/RTs_and_Projects/Evaluations/Global Fund PCE/Reports/Synthesis Reports/2018/absorption_by_module.png", width = 25, height = 25, units = "cm", dpi = "retina")
ggsave("I:/RTs_and_Projects/Evaluations/Global Fund PCE/Reports/Synthesis Reports/2018/absorption_by_module.pdf",  width = 15, height = 15)


#Version 2, with point scaling
library(gridExtra)
malaria <- plot_data[disease == "Malaria"]
HIV <- plot_data[disease == "HIV"]
TB <- plot_data[disease == "TB"]

g1 <- ggplot(data = malaria, aes(x = reorder(abbrev_mod_nolab, priority_mod))) +
  geom_pointrange(mapping=aes(y=mean, ymin=min, ymax=max), color = "green", fatten = malaria$point_scale) +
  theme_bw(base_size = 14) + theme(legend.position = 'none') +
  coord_flip()  +
  labs(y = "Malaria absorption Q1-Q2 2018 (%)", x = "Module")

g2 <- ggplot(data = HIV, aes(x = reorder(abbrev_mod_nolab, priority_mod))) +
  geom_pointrange(mapping=aes(y=mean, ymin=min, ymax=max), color = "red", fatten = HIV$point_scale) +
  theme_bw(base_size = 13) + theme(legend.position = 'none') +
  coord_flip() +
  labs(y = "HIV absorption Q1-Q2 2018 (%)", x = "Module")

g3 <- ggplot(data = TB, aes(x = reorder(abbrev_mod_nolab, priority_mod))) +
  geom_pointrange(mapping=aes(y=mean, ymin=min, ymax=max), color = "blue", fatten = TB$point_scale) +
  theme_bw(base_size = 13) + theme(legend.position = 'none') +
  coord_flip() +
  labs(y = "TB absorption Q1-Q2 2018 (%)", x = "Module", caption = "*Obervations with absorption > 200% not displayed.\nPoints represent average absorption across country/disease, with range showing min and max. \nParentheses show number of countries (out of 8) with Q1-Q2 absorption data for each disease.")

final_plot = grid.arrange(g1, g2, g3)
final_plot

ggsave("I:/RTs_and_Projects/Evaluations/Global Fund PCE/Reports/Synthesis Reports/2018/absorption_by_module_scaled.png", plot = final_plot, width = 25, height = 25, units = "cm", dpi = "retina")


resource_tracking <- fread("J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/final_budgets.csv")
resource_tracking = resource_tracking[data_source == "fpm"]

resource_tracking$commodity = resource_tracking[, .(commodity = (gf_module  %in% c('Treatment, care and support', 'Vector control', 'Case managmement', 'TB care and prevention', 'Multidrug-resistant TB', 'HIV Testing Services', 'Procurement and supply chain management systems')))]

rt_subset = resource_tracking[country == "Uganda" & disease == "malaria"]

rt_subset = rt_subset[, budget:=as.numeric(budget)]

grants_12_14 <- rt_subset[grant_period == "2012-2014"]
grants_11_13 <- rt_subset[grant_period == "2011-2013"] #for comparison
grants_15_17 <- rt_subset[grant_period == "2015-2017"]
grants_18_20 <- rt_subset[grant_period == "2018-2020"] 

# grants_12_14[commodity == TRUE, sum(budget)]/grants_12_14[, sum(budget)]
# grants_11_13[commodity == TRUE, sum(budget)]/grants_12_14[, sum(budget)]
# grants_15_17[commodity == TRUE, sum(budget)]/grants_15_17[, sum(budget)]
# grants_18_20[commodity == TRUE, sum(budget)]/grants_18_20[, sum(budget)]


hist_absorb_by_module = prepped[, .(gf_module, disease, historical_absorption)]
hist_absorb_by_module <- melt(hist_absorb_by_module, id = c("gf_module", "disease")) 
hist_absorb_by_module$value <- as.numeric(hist_absorb_by_module$value)
setDT(hist_absorb_by_module)
hist_absorb_by_module<-dcast(hist_absorb_by_module, gf_module+disease~variable, fun = list(min, mean, max))

nrow(hist_absorb_by_module[value_mean_historical_absorption < 50])/nrow(hist_absorb_by_module)


kpis <-readRDS("J:/Project/Evaluation/GF/outcome_measurement/multi_country/cleaned_indicator_performance_data.rds")
setDT(kpis)
kpis = kpis[is.na(remove_whereGrantHasAllZeroes)] #Remove lines where all fields are zero for a grant, or there are NAs in achievement ratio. 
kpis = kpis[module_code == "MDR TB", gf_module:="Multidrug-resistant TB"] 
kpis = kpis[module_code == "VC", gf_module:="Vector control"]
kpis = kpis[module_code == "CM", gf_module:="Case Management"]
kpis = kpis[module_code == "TB/HIV" | module_code == "TB/HIV I", gf_module:="TB/HIV"]
kpis = kpis[module_code == "M&E", gf_module:="Health management information system and monitoring and evaluation"]
kpis = kpis[module_code == "PMTCT", gf_module:="Prevention of mother-to-child transmission"]
kpis = kpis[module_code == "SPI", gf_module:="Specific prevention interventions"]
kpis = kpis[module_code == "TCP", gf_module:= "TB care and prevention"]
kpis = kpis[module_code == "TCS", gf_module:="Treatment, care & support"]
kpis = kpis[module_code == "PSM", gf_module:="Procurement and supply chain management systems"]
kpis[module_code == "SSRP", gf_module:="Integrated service delivery and quality improvement"]
kpis[module_code == "TB I" | substring(module_code, 1, 3) == "HSS", gf_module:="National health strategies"]

kpis = kpis[kpi_code == "HIV O-6(M)", gf_module:="Comprehensive prevention programs for people who inject drugs and their partners"]
kpis = kpis[kpi_code == "KP-1a(M)" | kpi_code == "KP-3a(M)", gf_module:="Comprehensive prevention programs for men who have sex with men"] #Some of these could be KVP or HIV testing services? 
kpis = kpis[kpi_code == "KP-1c(M)" | kpi_code == "KP-3c(M)", gf_module:="Comprehensive prevention programs for sex workers and their clients"]
kpis = kpis[kpi_code == "KP-1d(M)" | kpi_code == "KP-3d(M)", gf_module:="Comprehensive prevention programs for people who inject drugs and their partners"]
kpis = kpis[kpi_code == "HTS-1" | kpi_code == "HIV I-14" | kpi_code == "HIV I-6" | kpi_code == "GP other-1" | kpi_code == "GP other-2", gf_module:="HIV Testing Services"]
kpis = kpis[kpi_code == "KP Other-1" | kpi_code == "KP Other -2" | kpi_code == "HIV O-1(M)", gf_module:="Treatment, care & support"]
kpis = kpis[substring(kpi_code, 1, 4) == "TB O", gf_module:= "TB care and prevention"] #Some of these are blended between TB and MDRTB
kpis = kpis[substring(kpi_code, 1, 4) == "DOTS", gf_module:= "TB care and prevention"] #Some of these are blended between TB and MDRTB
kpis = kpis[kpi_code == "YP-2" | kpi_code == "YP-3", gf_module:="Prevention programs for adolescents and youth, in and out of school"]
kpis = kpis[kpi_code =="Malaria I-6", gf_module:="National health strategies"]

nhs_activities <- c("Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria")
kpis[kpi_text %in% nhs_activities | kpi_code == "Malaria I-1(M)" | kpi_code == "Malaria I-10(M)", gf_module:="National health strategies"]

to_classify = kpis[is.na(kpis$gf_module)]
#stopifnot(nrow(to_classify)==0)
#Verify all of these classifications are accurate 
#unique(kpis[!is.na(gf_module), .(kpi_text, gf_module)])

#---------------------------------------------------------
# Use gos data to calculate historical absorption by grant
#---------------------------------------------------------
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/'
inFile = paste0(dir, 'mapping/final_expenditures.csv')

# load
data = fread(inFile)
pudrs_2018 = data[data_source == "pudr" & year == "2018"]
print(unique(pudrs_2018[, .(grant_number, fileName)]))

duplicate_files <- c('Copy of LFA Review_COD-H-MOH_Progress  Report_30Jun2018_07092018 ok_Sent....xlsb.xlsx', 
                     'COD-M-SANRU_Progress Report_30Jun2018_v4_15092018.xlsx', 
                     'Copy of LFA_Review_COD-T-MOH_Progress Report_30Jun2018_Sent_02102018-Brk....xlsx')
pudrs_2018 = pudrs_2018[!fileName %in% duplicate_files]
stopifnot(length(unique(pudrs_2018$grant_number)) == length(unique(pudrs_2018$fileName)))


byVars = c('grant_number','grant_period','gf_module', 'year')
# collapse to grant level
pudrs_2018$expenditure <- as.numeric(pudrs_2018$expenditure)
pudrs_2018$budget <- as.numeric(pudrs_2018$budget)
pudrs_2018 = pudrs_2018[, .(expenditure=sum(expenditure, na.rm=T), budget=sum(budget, na.rm=T)), by=byVars]


# compute absorption
pudrs_2018[, absorption:=expenditure/budget]
pudrs_2018[!is.finite(absorption), absorption:=NA]

# exclude huge values
pudrs_2018[absorption>1, absorption_trimmed:=1]
pudrs_2018[absorption<=0, absorption_trimmed:=0]
pudrs_2018[!is.na(absorption_trimmed), absorption:=absorption_trimmed]

# take simple average
pudrs = pudrs_2018[, absorption:=mean(absorption, na.rm=T),by=byVars]

#Grabbed prepped absorption data and reshape by grant and module 
absorption_by_grant = prepped[, .(gf_module, disease, absorption_q1_2018, grant, budget)]
# check = absorption_by_grant[, max(absorption_q1_2018), by = grant]
# grants_with_no_data <- check$grant
# 
# absorption_by_grant[!grant %in% grants_with_no_data]
absorption_by_grant = absorption_by_grant[!is.na(budget)]
absorption_by_grant = absorption_by_grant[budget!=0]
absorption_by_grant[, .(gf_module, absorption_q1_2018, grant)]
absorption_by_grant <- melt(absorption_by_grant, id = c("gf_module", "grant")) 
absorption_by_grant$value <- as.numeric(absorption_by_grant$value)
setDT(absorption_by_grant)
absorption_by_grant<-dcast(absorption_by_grant, gf_module+grant~variable, fun = mean)

#Clean up grant names 
absorption_by_grant$grant = gsub("\\(", "", absorption_by_grant$grant)
absorption_by_grant$grant = gsub("\\)", "", absorption_by_grant$grant)
absorption_by_grant$grant = gsub("Q1-Q2 2018", "", absorption_by_grant$grant)
absorption_by_grant$grant = gsub("Q3 2017", "", absorption_by_grant$grant)
absorption_by_grant$grant = gsub("ONLY FOR ANCS CIVIL SOCIETY", "", absorption_by_grant$grant)

#Pick which dataset to use for absorption merge, PUDRS or google sheets 
absorption_merge <- copy(absorption_by_grant[!is.na(budget), .(gf_module, grant, absorption=absorption_q1_2018)])
#----------------------------------------------
#Format absorption data for merge 
#----------------------------------------------

#Remove NAs from absorption, and 
absorption_merge = absorption_merge[!is.na(absorption)]
#absorption_merge = absorption_merge[, absorption:=absorption*100] #Multiply absorption so it will show nicely as percentage
absorption_merge$country = substring(absorption_merge$grant, 1, 3)
#Trimming really extreme outliers (100% absorption trimmed to 150%)
absorption_merge = absorption_merge[absorption>1000, absorption:=150]

#----------------------------------------------
#Format KPI data for merge 
#----------------------------------------------
kpi_merge = kpis[, .(country, grant, kpi, target, result, achievement_ratio, gf_module)]

#Match country names 
kpi_merge[country == "Cambodia", country:="KHM"]
kpi_merge[country == "DRC", country:="COD"]
kpi_merge[country == "Mozambique", country:="MOZ"]
kpi_merge[country == "Myanmar", country:="MMR"]
kpi_merge[country == "Senegal", country:="SEN"]
kpi_merge[country == "Sudan", country:="SDN"]

#Trimming outliers in achievement ratio (over 200%)
kpi_merge = kpi_merge[achievement_ratio>=200, achievement_ratio:=200]

#----------------------------------------------
# Merge two datasets together and plot
#----------------------------------------------
absorption_to_kpis <- merge(absorption_merge, kpi_merge, by = c("gf_module", "country"), all = TRUE)
#Remove values without an achievement ratio or absorption 
absorption_to_kpis <- absorption_to_kpis[!is.na(achievement_ratio) & !is.na(absorption)]
 
#Generate a line of best fit (done below with geom_smooth())
lr1 <-lm(achievement_ratio ~ absorption, data = absorption_to_kpis)
summary(lr1)
cor(absorption_to_kpis$absorption, absorption_to_kpis$achievement_ratio)

ggplot(data = absorption_to_kpis, aes(x = absorption, y = achievement_ratio)) + geom_point() +
  labs(x = "Absorption, %", y = "KPI Achievement Ratio, %", caption = "Observations with achievement ratio >200% \n or absorption >1000% trimmed to fit") + 
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw(base_size = 18) 

ggsave("I:/RTs_and_Projects/Evaluations/Global Fund PCE/Reports/Synthesis Reports/2018/absorption_to_kpis.png", width = 25, height = 25, units = "cm", dpi = "retina")

