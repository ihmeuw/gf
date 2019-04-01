# --------------------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Explore supply chain data for PNLS, mainly on HIV test kits. 
# DATE: Last updated March 2019
# --------------------------------------------------------------------

rm(list=ls())
library(data.table)
library(ggplot2)
library(rgdal)

#--------------------------------------------------------------
#Read in data 
#--------------------------------------------------------------
# detect if operating on windows or on the cluster 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')
saveDir = paste0(dir, "outputs/pnls/")

dt = readRDS(paste0(dir, 'prepped/pnls_sets/pnls_drug_2017_01_01_2018_12_01.rds'))
shapefile = readOGR("J:/Project/Evaluation/GF/mapping/cod/gadm36_COD_shp/gadm36_COD_1.shp")

#--------------------------------------------------------------
#Clean the data 
#--------------------------------------------------------------
dt = dt[, -c('subpop', 'maternity', 'case')]

dt[stock_category=='Nbr de jours RS', stock_category:='number_of_days_stocked_out']
dt[stock_category=="Stock disponible utilisable", stock_category:='available_usable_stock']
dt[stock_category=='Stock Initial', stock_category:='initial_stock']
dt[stock_category=="Entrée", stock_category:='input']
dt[stock_category=='Sortie', stock_category:='output']


#Make a test kit data table 
test_kit_vars = c("HIV 1+2, Determine Complete, Kit de 100 tests", "HIV 1/2, Double Check Gold, Kit de 100 test", "HIV 1+2, Uni-Gold HIV, Kit de 20 tests")
test_kits = dt[element%in%test_kit_vars]
test_kits = test_kits[, .(element_id, date, element, value, stock_category)]
test_kits[, value:=sum(value), by=c('element_id', 'element', 'date', 'stock_category')]
test_kits = unique(test_kits)
#--------------------------------------------------------------
#Make some charts 
#--------------------------------------------------------------

dt[ ,total_facilities:=length(unique(org_unit_id)), by=dps]
dps_level = dt[, .(element, dps, value, stock_category, date, total_facilities)]
dps_level[, value:=sum(value), by=c('total_facilities', 'element', 'dps', 'stock_category', 'date')]
dps_level = unique(dps_level)

dps_level_so = dps_level[stock_category=="number_of_days_stocked_out"]
dps_level_so[, mean_fac_days_so:=value/total_facilities]

# •	Graph national time trends for each test kit variable. For example, a graph would be facet wrapped by stock category with scales=’free_y’. It doesn’t make sense to use stock category
#as color (all on the same graph) because they are in different units – days out of stock in days, stock available is in… stuff. So, we want to facet with a continuous y axis.

tt1 = ggplot(test_kits[element=="HIV 1+2, Determine Complete, Kit de 100 tests"], aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  labs(title="Determine stock categories over time, with free_y scales")

tt2 = ggplot(test_kits[element=="HIV 1/2, Double Check Gold, Kit de 100 test"], aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  labs(title="Double-check gold stock categories over time, with free_y scales")

tt3 = ggplot(test_kits[element=="HIV 1+2, Uni-Gold HIV, Kit de 20 tests"], aes(x=date, y=value)) + 
  facet_wrap(~stock_category, scales='free_y')+
  geom_point() + geom_line() + 
  labs(title="Uni-gold stock categories over time, with free_y scales")

# •	Make a DPS level map for each. I would start with a count of days out of stock per district (just sum by district) and make one map for each type of test kits 
#(answers the question: how many stock out days were there per district for first line versus second line test kits?). You can also facet wrap by year, 
#so you would have two maps (2017 and 2018) on the same page.

determine_so = test_kits[element=="HIV 1+2, Determine Complete, Kit de 100 tests" & stock_category == 'number_of_days_stocked_out', .(value=sum(value)), by=c('dps', 'date')]
#When were test kits stocked out? 
so1 <- ggplot(determine_so, aes(x=date, y=value, color=dps)) + 
  geom_point() + 
  theme_bw() + 
  labs(title= "Days stocked out by district, Determine Complete")

double_check_so = test_kits[element=="HIV 1/2, Double Check Gold, Kit de 100 test" & stock_category == 'number_of_days_stocked_out', .(value=sum(value)), by=c('dps', 'date')]
so2 <- ggplot(double_check_so, aes(x=date, y=value, color=dps)) + 
  geom_point() +
  theme_bw() + 
  labs(title= "Days stocked out, HIV 1/2 Double Check Gold")

uni_gold_so = test_kits[element=="HIV 1+2, Uni-Gold HIV, Kit de 20 tests" & stock_category == 'number_of_days_stocked_out', .(value=sum(value)), by=c('dps', 'date')]
so3 <- ggplot(uni_gold_so, aes(x=date, y=value, color=dps)) + 
  geom_point() +
  theme_bw() + 
  labs(title= "Days stocked out, HIV 1+2 Uni-Gold")

#Datasets with hypothetical outliers trimmed 
determine_so_trim = determine_so[value<5000]
double_check_so_trim = double_check_so[value<1000]
uni_gold_so_trim = uni_gold_so[value<10000]

#Run that same set of graphs, but with hypothetical outliers removed. These should be verified!  
so4 <- ggplot(determine_so_trim, aes(x=date, y=value, color=dps)) + 
  geom_point() + 
  theme_bw() + 
  labs(title= "Days stocked out, Determine Complete (Hypothetical outliers removed)")

so5 <- ggplot(double_check_so_trim, aes(x=date, y=value, color=dps)) + 
  geom_point() +
  theme_bw() + 
  labs(title= "Days stocked out, HIV 1/2 Double Check Gold (Hypothetical outliers removed)")

so6 <- ggplot(uni_gold_so_trim, aes(x=date, y=value, color=dps)) + 
  geom_point() +
  theme_bw() + 
  labs(title= "Days stocked out, HIV 1+2 Uni-Gold (Hypothetical outliers removed)")


#What are the ranges of days stocked-out here? There is a much higher max range for determine and uni_gold than for double_check. 
range(determine_so_trim$value)
range(double_check_so_trim$value)
range(uni_gold_so_trim$value)

#Standardize the axis scales here so they're more comparable. 

#Run that same set of graphs, but with hypothetical outliers removed. These should be verified!  
so7 <- ggplot(determine_so_trim, aes(x=date, y=value, color=dps)) + 
  geom_point() + 
  theme_bw() + 
  scale_y_continuous(limits = c(0, 5000)) + 
  labs(title= "Days stocked out, Determine Complete (Hypothetical outliers removed), Standardized y-scale")

so8 <- ggplot(double_check_so_trim, aes(x=date, y=value, color=dps)) + 
  geom_point() +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 5000) ) + 
  labs(title= "Days stocked out, HIV 1/2 Double Check Gold (Hypothetical outliers removed), Standardized y-scale")

so9 <- ggplot(uni_gold_so_trim, aes(x=date, y=value, color=dps)) + 
  geom_point() +
  theme_bw() + 
  scale_y_continuous(limits = c(0, 5000)) + 
  labs(title= "Days stocked out, HIV 1+2 Uni-Gold (Hypothetical outliers removed), Standardized y-scale")

# •	Then, I would have some fun by using a distinct metric – mean stock out days per facility. The reason we want this is that you could have a district with a 
#lot of facilities that were rarely stocked out and it would look like there were more stock out days there than in a district with few facilities with long stock outs.
#So, create a data table dt[ ,total_facilities=length(unique(org_unit_id)), by=dps] and merge it into your summed data, then divide the total stockout days by the facilities.

mean_fac_so1 = ggplot(dps_level_so, aes(x=date, y=mean_fac_days_so, color=dps)) + 
  geom_point() + 
  theme_bw() + 
  labs(title = "Mean facility-days stocked out, by district, for all commodities")

mean_fac_so2 = ggplot(dps_level_so[element=="HIV 1+2, Uni-Gold HIV, Kit de 20 tests"], aes(x=date, y=mean_fac_days_so, color=dps)) + 
  geom_point() + 
  theme_bw() + 
  labs(title = "Mean facility-days stocked out, by district, uni-gold")

mean_fac_so3 = ggplot(dps_level_so[element=="HIV 1/2, Double Check Gold, Kit de 100 test"], aes(x=date, y=mean_fac_days_so, color=dps)) + 
  geom_point() + 
  theme_bw() + 
  labs(title = "Mean facility-days stocked out, by district, double-check")

mean_fac_so4 = ggplot(dps_level_so[element=="HIV 1+2, Determine Complete, Kit de 100 tests"], aes(x=date, y=mean_fac_days_so, color=dps)) + 
  geom_point() + 
  theme_bw() + 
  labs(title = "Mean facility-days stocked out, by district, determine")

#--------------------------------------------------------------
#Make the graphs, and write to a .PDF
#--------------------------------------------------------------
outFile = paste0(saveDir, 'drug_test_kits.pdf')
pdf(outFile, height=5.5, width=7)

tt1
tt2
tt3

# so1
# so2
# so3
# so4
# so5
# so6
# so7
# so8
# so9

# mean_fac_so1
# mean_fac_so2
# mean_fac_so3
# mean_fac_so4

dev.off()


#The other thing was just to run a PDF loop with a time trend for every variable (50 page PDF). Make sure you aggregate
#to the level of variable/date/stock category 
#before you do that and facet by stock category. An extra credit alternative would be to add an if statement 
#that says if(!is.na(sex)) for a given variable and then embed 
#a ggplot where color = sex, so that the ones that have sex are stratified by sex and the ones that don’t have sex have a single line on each graph.
outFile = paste0(saveDir, 'drug_all_elements.pdf')
pdf(outFile, height=5.5, width=7)

for (var in unique(dt$element)){
  temp = dt[element==var, .(date, value, element, stock_category)]
  temp[, value:=sum(value), by=.(date, element, stock_category)]
  temp = unique(temp)
  
  plot = ggplot(temp, aes(x=date, y=value)) + 
    geom_point() + geom_line() + 
    theme_bw() + 
    facet_wrap(~stock_category, scales='free_y')+
    labs(title= var)
  print(plot) 
}
dev.off()
