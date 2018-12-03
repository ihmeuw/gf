

# ---------------------------------------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# ---------------------------------------------------


# ---------------------------------------------------
# Directories

# # root directory - change this
# dir = 'C:/Users/davidp6/Google Drive/Work/IHME Work/GF/Workshops/gtm_aug2018/'
# 
# # resource tracking data
# rtFile = paste0(dir, 'Day 1/total_resource_tracking_data_tb.csv')
# 
# # drug distribution data
# distrFile = paste0(dir, 'Day 2/GTM-TB-distribution-2013-2018.csv')
# 
# # cohort data
# cohortFile = paste0(dir, 'Day 2/GTM - Tx cohort data 2012-2016.csv')
# 
# # output file
# outFile = paste0(dir, 'Day 2/Resource Comparison.pdf')

outcome_dir = "J:/Project/Evaluation/GF/outcome_measurement/gtm/"
rtFile = "J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/total_resource_tracking_data.csv"
distrFile = paste0(outcome_dir, "prepped_data/GTM-TB-distribution-2013-2018_with_Unit_Cost.csv")
cohortFile = paste0(outcome_dir, "prepped_data/GTM - Tx cohort data 2012-2016.csv")
outFile = paste0(outcome_dir, "Resource Comparison.pdf")
outFile_two = paste0(outcome_dir, "UnitCostOverTime_sameScale.pdf")

# ---------------------------------------------------


# ---------------------------------------------------
# Load prep RT data

# load
rtData = fread(rtFile)

# subset to TB only
rtData = rtData[disease=='tb']

# subset to GHE/external from sicoin and GF from budgets
rtData = rtData[country=='Guatemala' & 
                  ((data_source=='sicoin' & financing_source %in% c('ghe','other_dah')) | 
                     (data_source%in%c('fgh','fpm') & financing_source=='gf')) ]

# aggregate by year
rtData = rtData[, .(budget=sum(budget), disbursement=sum(disbursement)), by=c('financing_source','year')]

# make total aggregate
rtAgg = rtData[, .(budget=sum(budget), disbursement=sum(disbursement)), by='year']
# ---------------------------------------------------


# ---------------------------------------------------
# Load/prep distribution data
# load
distrData = fread(distrFile)
setnames(distrData, old = "Unit Cost (USD)", new = "unitCost")

distrData$AmountSpent = distrData$Amount * distrData$unitCost

# aggregate
distrAgg = distrData[Medicine=='RIFAMPICINA',
                     .(rifampicin=sum(AmountSpent, na.rm=TRUE)), by=c("Year")]


# aggregate all drugs
distrAgg_all_drugs = distrData[, 
                               .(all_drugs=sum(AmountSpent, na.rm=TRUE)), by=c("Year")]

# aggregate all drugs not Inclued Isoniazida
distrAgg_all_drugs_sans = distrData[Medicine != "ISONIAZIDA", 
                                    .(all_drugs_sans=sum(AmountSpent, na.rm=TRUE)), by=c("Year")]

# find unit cost over time
unit_cost_per_year = unique(distrData[,c("Product", "Year", "unitCost")])
unit_distr_per_year = distrData[,c("Product", "Year", "Amount")]
unit_distr_per_year[,distr_amount := sum(Amount, na.rm = TRUE), by = c("Product", "Year")]
unit_distr_per_year = unique(unit_distr_per_year[,c("Product", "Year", "distr_amount")])

# aggregate all drugs not Inclued Isoniazida
distrAgg_per_department = distrData[Medicine != "ISONIAZIDA", 
                                    .(all_drugs_sans=sum(AmountSpent, na.rm=TRUE)), by=c("Year", "code_dept")]
# convert deptocode to department name

departments = unique(distrData[,c("code_dept", "Department")])
departments$Department = ifelse(departments$code_dept == 1, "GUATEMALA", 
                                ifelse(departments$code_dept == 14, "QUICHE",
                                       ifelse(departments$code_dept == 17, "PETEN", departments$Department)))
departments = unique(departments)

# ---------------------------------------------------


# ---------------------------------------------------
# Load/prep cohort data

# load
cohortDataAll = fread(cohortFile)

# subset to totals
cohortData = cohortDataAll[col_name=='TOTAL' & deptocode==0]
cohortDept = cohortDataAll[col_name=='TOTAL' & deptocode!=0]

# subset to avoid overlapping types
cohortData = cohortData[table %in% c('Nuevos Pulmonares BK+', 
                                     'Nuevos Pulmonares BK-', 'Nuevos Pediatricos', 
                                     'Nuevos Extrapulmonares', 'Nuevos TB/VIH', 'Retratamiento')]

# subset to avoid overlapping types
cohortDept = cohortDept[table %in% c('Nuevos Pulmonares BK+', 
                                     'Nuevos Pulmonares BK-', 'Nuevos Pediatricos', 
                                     'Nuevos Extrapulmonares', 'Nuevos TB/VIH', 'Retratamiento')]

# aggregate
deptAgg = cohortDept[row_name %in% c('CURADOS','TRATAMIENTOS COMPLETOS'), 
                       .(cured=sum(value, na.rm=TRUE)), by=c('year', 'deptocode')]
deptAgg = merge(deptAgg, cohortDept[, .(total=sum(value, na.rm=TRUE)), by=c('year', 'deptocode')], by=c('year', 'deptocode'))


# aggregate
cohortAgg = cohortData[row_name %in% c('CURADOS','TRATAMIENTOS COMPLETOS'), 
                       .(cured=sum(value, na.rm=TRUE)), by='year']
cohortAgg = merge(cohortAgg, cohortData[, .(total=sum(value, na.rm=TRUE)), by='year'], by='year')

# aggregate including MDR
cohortAgg = cohortData[row_name %in% c('CURADOS','TRATAMIENTOS COMPLETOS'), 
                       .(cured=sum(value, na.rm=TRUE)), by='year']
cohortAgg = merge(cohortAgg, cohortData[, .(total=sum(value, na.rm=TRUE)), by='year'], by='year')

# aggregate MDR started treatment only
cohortAggMDR = cohortDataAll[deptocode==0 & grepl('2nd line treatment begun', table), 
                             .(second_line_begun=sum(value,na.rm=TRUE)), by='year']

# aggregate excluding MDR
cohortAggNoMDR = merge(cohortAgg, cohortAggMDR, by='year')
cohortAggNoMDR[, cured:=cured-second_line_begun]
cohortAggNoMDR[, total:=total-second_line_begun]
cohortAggNoMDR$second_line_begun = NULL
setnames(cohortAggNoMDR, old = c('cured','total'), new = c('cured_first_line','total_first_line'))
# ---------------------------------------------------


# ---------------------------------------------------
# Ratios

# merge data
data = merge(rtAgg, cohortAgg, by='year')
data = merge(data, distrAgg, by.x='year', by.y='Year', all.x=TRUE)
data = merge(data, cohortAggMDR, by='year', all.x=TRUE)
data = merge(data, cohortAggNoMDR, by='year', all.x=TRUE)
data = merge(data, distrAgg_all_drugs, by.x='year', by.y='Year', all.x=TRUE)
data = merge(data, distrAgg_all_drugs_sans, by.x='year', by.y='Year', all.x=TRUE)

# clean up department data
department_data = merge(deptAgg, distrAgg_per_department, by.x=c('year', 'deptocode'), by.y = c("Year", "code_dept"))
department_data[, all_sans_cured:=all_drugs_sans/cured]
department_data$all_drugs_sans = NULL
department_data$total = NULL
department_data$cured = NULL

department_data = merge(department_data, departments, by.x = 'deptocode', by.y = "code_dept")

# dollars per case cured over time
data[, budget_cured:=budget/cured]
data[, disbursement_cured:=disbursement/cured]

# dollars per case treated with 2nd line over time
data[, budget_second_line:=budget/second_line_begun]
data[, disbursement_second_line:=disbursement/second_line_begun]

# dollars per case treated with 1st line over time
data[, budget_cured_first:=budget/cured_first_line]
data[, disbursement_cured_first:=disbursement/cured_first_line]

# drugs distributed per case cured over time
data[, rifa_cured:=rifampicin/cured]
data[, all_drugs_cured:=all_drugs/cured]
data[, all_sans_cured:=all_drugs_sans/cured]

# set up to graph
graphData = melt(data, id='year')
graphData[variable=='budget_cured', label:='Budget/Cured']
graphData[variable=='disbursement_cured', label:='Disbursement/Cured']
graphData[variable=='budget_second_line', label:='Budget/Second-Line']
graphData[variable=='disbursement_second_line', label:='Disbursement/Second-Line']
graphData[variable=='budget_cured_first', label:='Budget/Cured']
graphData[variable=='disbursement_cured_first', label:='Disbursement/Cured']

graphData[!is.finite(value), value:=NA]
# ---------------------------------------------------


# ---------------------------------------------------

# Graph
p1 = ggplot(graphData[variable %in% c('budget_cured','disbursement_cured')], 
            aes(y=value, x=year, color=label)) + 
  geom_line(size=1.5) +
  geom_point(size=3.5, color='grey45') + 
  labs(title='Dollars Budgeted and Disbursed Compared to Cases Cured', 
       y='USD per Case Cured', x='Year',color='',
       caption='Resources from Government and Global Fund Combined') + 
  theme_bw()

p2 = ggplot(graphData[variable %in% c('budget_second_line','disbursement_second_line')], 
            aes(y=value, x=year, color=label)) + 
  geom_line(size=1.5) +
  geom_point(size=3.5, color='grey45') + 
  labs(title='Dollars Budgeted and Disbursed Compared to Cases Started Second-Line Treatment', 
       y='USD per Case Started Second-Line Treatment', x='Year',color='',
       caption='Resources from Government and Global Fund Combined') + 
  theme_bw()

p3 = ggplot(graphData[variable %in% c('budget_cured_first','disbursement_cured_first')], 
            aes(y=value, x=year, color=label)) + 
  geom_line(size=1.5) +
  geom_point(size=3.5, color='grey45') + 
  labs(title='Dollars Budgeted and Disbursed Compared to Cases Cured (Excluding MDR)', 
       y='USD per Case Cured (Excluding MDR)', x='Year',color='',
       caption='Resources from Government and Global Fund Combined') + 
  theme_bw()

data = na.omit(data)
p4 = ggplot(data, aes(y=rifa_cured, x=year, label=rifampicin)) + 
  geom_line(size=1.5) +
  geom_point(size=3.5, color='grey45') + 
  #geom_text(hjust=1, vjust=0) + 
  labs(title='Rifampicin Distributed Compared to Cases Cured', 
       y='Amount Spent per Case Cured (USD)', x='Year') + 
  theme_bw()

p5 = ggplot(data, aes(y=all_drugs_cured, x=year, label=all_drugs)) + 
  geom_line(size=1.5) +
  geom_point(size=3.5, color='grey45') + 
  #geom_text(hjust=1, vjust=0) + 
  labs(title='All Medicine Distributed Compared to Cases Cured', 
       y='Amount Spent per Case Cured (USD)', x='Year') + 
  theme_bw()


p6 = ggplot(data, aes(y=all_sans_cured, x=year, label=all_drugs_sans)) + 
  geom_line(size=1.5) +
  geom_point(size=3.5, color='grey45') + 
  #geom_text(hjust=1, vjust=0) + 
  labs(title='All Medicine not including Isoniazida Distributed Compared to Cases Cured', 
       y='Amount Spent per Case Cured (USD)', x='Year') + 
  theme_bw()

unit_cost_per_year = na.omit(unit_cost_per_year)

p7 = ggplot(unit_cost_per_year, aes(y=unitCost, x=Year, colour=Product)) + 
  geom_line(size=1) +
  geom_point(size=3, color='grey45') + 
  #geom_text(hjust=1, vjust=0) + 
  labs(title='Unit Cost per All Drug Treatments', 
       y='Cost (USD)', x='Year') + 
  theme_bw() +
  theme(legend.position="bottom", legend.text = element_text(size = 5)) +
  guides(col = guide_legend(ncol = 2))


firstLine = unit_cost_per_year[Product ==  "ISONIAZIDA, TABLETA DE 100 MG." | 
                                 Product ==  "ISONIAZIDA, TABLETA DE 300 MG."|
                                 Product == "RIFAMPICINA, TABLETA DE 300 MG."  |
                                 Product == "RIFAMPICINA SUSPENSION 100MG/5ML., FRASCO DE 120 ML." |
                                 Product == "PIRAZINAMIDA, TABLETA DE 500 MG." |
                                 Product == "ETAMBUTOL, TABLETA DE 400 MG."]


commonDrugs = unit_distr_per_year[Product == "ETAMBUTOL, TABLETA DE 400 MG." |
                                   Product == "ETHIONAMIDA, COMPRIMIDO DE 250 MG." |
                                   Product ==  "ISONIAZIDA, TABLETA DE 300 MG."|
                                   Product == "RIFAMPICINA, TABLETA DE 300 MG."  |
                                   Product == "PIRAZINAMIDA, TABLETA DE 500 MG." |
                                   Product == "LEVOFLOXACINA, COMPRIMIDO DE 250 MG." |
                                   Product == "LEVOFLOXACINA, COMPRIMIDO DE 500 MG." |
                                   Product ==  "CICLOSERINA, CAPSULA DE 250 MG." ]

secondLineDrugs = unit_distr_per_year[Product == "AMOXICILINA/ACIDO CLAVULANICO, COMPRIMIDO DE 875MG/125MG." |
                                    Product ==  "CAPREOMICINA VIAL 1 G"|
                                    Product ==  "CICLOSERINA, CAPSULA DE 250 MG." |  
                                    Product == "ETHIONAMIDA, COMPRIMIDO DE 250 MG." |      
                                    Product == "IMIPENEM/CILASTATINA 1 GRAMOS" |
                                    Product == "LEVOFLOXACINA, COMPRIMIDO DE 250 MG." |
                                    Product == "MOXIFLOXACINA, TABLETA DE 400 MG." |
                                    Product ==  "LINEZOLID 600 MG TABLETA"]

commonDrugs$first = ifelse(commonDrugs$Product ==  "ISONIAZIDA, TABLETA DE 300 MG."|
                             commonDrugs$Product == "RIFAMPICINA, TABLETA DE 300 MG."  |
                             commonDrugs$Product == "PIRAZINAMIDA, TABLETA DE 500 MG." |
                             commonDrugs$Product == "ETAMBUTOL, TABLETA DE 400 MG." , "First-Line Treatments", "Second-Line Treatments")
                                   

commonDrugs[grepl("ISONIAZIDA", Product), Product := "Isoniazid"]
commonDrugs[grepl("ETAMBUTOL", Product), Product := "Ethambutol"]
commonDrugs[grepl("ETHIONAMIDA", Product), Product := "Ethionamide"]
commonDrugs[grepl("RIFAMPICINA", Product), Product := "Rifampicin"]
#commonDrugs[grepl("LEVOFLOXACINA", Product), Product := "Levofloxacin"]
commonDrugs[grepl("PIRAZINAMIDA", Product), Product := "Pyrazinamide"]
commonDrugs[grepl("CICLOSERINA", Product), Product := "Cycloserine"]

commonDrugs$Product <- factor(commonDrugs$Product, levels=c('Rifampicin', 'Isoniazid', 'Pyrazinamide', 'Ethambutol', 'Cycloserine', 'Ethionamide',"LEVOFLOXACINA, COMPRIMIDO DE 250 MG.", "LEVOFLOXACINA, COMPRIMIDO DE 500 MG."))

p8 = ggplot(firstLine, aes(y=unitCost, x=Year, colour=Product)) + 
  geom_line(size=1) +
  geom_point(size=3, color='grey45') + 
  #geom_text(hjust=1, vjust=0) + 
  labs(title='Unit Cost per First Line Drug Treatment', 
       y='Cost (USD)', x='Year') + 
  theme_bw() +
  theme(legend.position="bottom", legend.text = element_text(size = 5)) +
  guides(col = guide_legend(ncol = 2))

p9 = ggplot(commonDrugs, aes(y=unitCost, x=Year, colour=Product)) + 
  geom_line(size=1) +
  geom_point(size=3, color='grey45') + 
  #geom_text(hjust=1, vjust=0) + 
  labs(title='Unit Cost per Common Drugs Treatment', 
       y='Cost (USD)', x='Year') + 
  theme_bw() +
  theme(legend.position="bottom") +
  guides(col = guide_legend(ncol = 2))

p10 = ggplot(commonDrugs[Product == "LEVOFLOXACINA, COMPRIMIDO DE 250 MG." | Product == "LEVOFLOXACINA, COMPRIMIDO DE 500 MG." ], aes(y=distr_amount, x=Year, colour=Product)) + 
  geom_line(size=1) +
  geom_point(size=1, color='grey45') + 
  labs(title='Distrbution of LEVOFLOXACINA over time', 
       y='Distrbution', x='Year', colour = "") + 
  scale_y_continuous(labels = comma) +
  #facet_wrap(~first) + 
  theme_bw() +
  theme(legend.position="bottom") +
  guides(col = guide_legend(ncol = 2))


p11 = ggplot(department_data, aes(y=all_sans_cured, x=year, colour=Department)) + 
  geom_line(size=1.5) +
  geom_point(size=3.5, color='grey45') + 
  #geom_text(hjust=1, vjust=0) + 
  labs(title='Treatment not including Isoniazida Distributed Compared to Cases Cured by Department', 
       y='Amount Spent per Case Cured (USD)', x='Year') + 
  theme_bw() +
  theme(legend.position="bottom", legend.text = element_text(size = 5)) +
  guides(col = guide_legend(ncol = 2))


# ---------------------------------------------------


# ---------------------------------------------------
# save
pdf(outFile, height=5.5, width=7)
p1
p2
p3
p4
p5
p6
p7
p8
p9
p10
p11
dev.off()
# ---------------------------------------------------

pdf("J:/Project/Evaluation/GF/outcome_measurement/gtm/visualizations/TB Drug Cost/Lexo_Distribution.pdf", height=5.5, width=7)
p10
dev.off()