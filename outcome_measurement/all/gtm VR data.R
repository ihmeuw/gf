# Deaths data. Compare IHME GBD death certificates reclassiffication with INE database
library(data.table)
library(ggplot2)
library(haven)

codePath = "PCE/gf/"

# Requirements:
source(paste0(codePath, "core/GT_load_data.R"), encoding = "UTF-8")
source(paste0(codePath, "core/GT_helper_functions.R"), encoding = "UTF-8")
collapse_large_dataset = FALSE
if (collapse_large_dataset) {
    # IHME  deaths data
    IHMEDefsData = read.csv("PCE/Outcome Measurement Data/MULTI/VR/redistribution_20180716.csv")
    # Update 23-07-2018
    # Since the new file is huge and takes too much space on memory, 
    # I have decided to collapse the counts by the diseases of interest:
    
    nrow(IHMEDefsData) # 32060574
    length(unique(IHMEDefsData$site_id)) # 1
    length(unique(IHMEDefsData$cause_id)) # 318
    length(unique(IHMEDefsData$location_id)) # 340
    length(unique(IHMEDefsData$year_id)) # 8
    length(unique(IHMEDefsData$age_group_id)) # 23
    
    collapse_IHME_deaths <- function (causes, disease_name) {
        collapsed = data.table(IHMEDefsData[(IHMEDefsData$cause_id %in% causes),])[, 
                               .(deaths = sum(deaths)), 
                               by=.(year_id, age_group_id, sex_id, location_id)] 
        print("Sum of deaths by year")
        print(collapsed[, sum(deaths), by=year_id])
        collapsed[, disease := disease_name]
        collapsed
    }
    
    causes_tb = c(297, 954, 934, 946, 947)
    causes_tb_vih = c(948, 949, 950)
    causes_vih = c(298, 948, 949, 950, 300)
    causes_malaria = c(345, 856, 857, 858)
    
    IHME_tb_collapsed = collapse_IHME_deaths(causes_tb, "TB")
    IHME_tb_vih_collapsed = collapse_IHME_deaths(causes_tb_vih, "TB-HIV")
    IHME_vih_collapsed = collapse_IHME_deaths(causes_vih, "HIV")
    IHME_malaria_collapsed = collapse_IHME_deaths(causes_malaria, "Malaria")
    
    IHME_deaths_collapsed = rbind(IHME_tb_collapsed, 
                                  IHME_tb_vih_collapsed, 
                                  IHME_malaria_collapsed, 
                                  IHME_vih_collapsed)
    
    # Lets compare with Irena's dataset
    irena_coll = read.csv("PCE/Outcome Measurement Data/MULTI/VR/vr_redistributed_collapsed.csv")
    irena_coll = data.table(irena_coll)
    irena_coll[death_id_collapsed == "tb", sum(deaths), by=.(year_id)]
    IHME_deaths_collapsed[disease %in% c("TB", "TB-HIV"), sum(deaths), by=.(year_id)]
    # The numbers are the same. 
    # Now replacing Irena's with this file because it is more complete as it covers the other 3 diseases.
    write.csv(IHME_deaths_collapsed, 
              "./PCE/Outcome Measurement Data/MULTI/VR/vr_redistributed_collapsed_diseases.csv")
}

collapse_ine_datasets = FALSE
if (collapse_ine_datasets) {
    tb_pre = c("A15", "A16", "A17", "A18", "A19","B90")
    tb_other = c("A301", "A302", "J65X", "K230", "K673", "M011",
                   "N330", "M490", "M900", "N740", "N741", "O980", "K930",
                   "P370", "Z030", "Z111", "Z201", "Z232", "U843", "B200") 
    hiv_pre = c("B20", "B21", "B22", "B23", "B24")
    hiv_other = c("R75X", "Z114", "Z206", "Z21X", "Z717")
    malaria_pre = c("B50", "B51", "B52", "B53", "B54")
    malaria_other = c("P373", "P374")
    # Load deaths INE data up to 2016.
    defsData = loadDeathsData()
    collapsed_data = NULL
        
    collapse_year_data <- function (data, codes, pre_codes, disease_str) {
        names(data) = str_to_lower(names(data))
        data[(caudefpre %in% pre_codes) | 
                     (caudef %in% codes), 
                     .(deaths = .N, disease = disease_str), 
                by = .(agegroup = (4 + ceiling((edadif+1)/5)), sex = sexo, municode = mupocu,
                       year = if(exists("añoocu")) añoocu else añoreg ) ]
    }
    for (dataset in defsData) {
        if (!is.null(dataset)) {
            tb_data = collapse_year_data(dataset, tb_other, tb_pre, "TB")
            hiv_data = collapse_year_data(dataset, hiv_other, hiv_pre, "HIV")
            mlr_data = collapse_year_data(dataset, malaria_other, malaria_pre, "Malaria")
            if (is.null(collapsed_data)) {
                collapsed_data = rbind(tb_data, hiv_data, mlr_data)
            }
            else {
                collapsed_data = rbind(collapsed_data, tb_data, hiv_data, mlr_data)
            }
        }
    }
    write.csv(collapsed_data, "./PCE/Outcome Measurement Data/MULTI/VR/ine_collapsed_data.csv")
}

# Load IHME data
IHME_deaths_collapsed = read.csv("./PCE/Outcome Measurement Data/MULTI/VR/vr_redistributed_collapsed_diseases.csv",
                                 row.names = 1)
IHME_deaths_collapsed = data.table(IHME_deaths_collapsed)
IHME_deaths_collapsed = rbind(IHME_deaths_collapsed, 
                              IHME_deaths_collapsed[disease %in% c("TB", "TB-HIV"), 
                                                    .(disease = "TB_all" , deaths = sum(deaths)), 
                                                    by=.(year_id, age_group_id, sex_id, location_id)])
IHMELocations = data.table(read.csv("PCE/Covariates and Other Data/GIS/GTM_muni_merges_2009_2016.csv"))
IHMEAgeGroups = data.table(read.csv("PCE/Outcome Measurement Data/MULTI/VR/age_group_ids.csv"))

INE_data = read.csv("PCE/Outcome Measurement Data/MULTI/VR/ine_collapsed_data.csv")
INE_data = data.table(INE_data)
INE_data[year == 9,year:=2009]
# Count of deaths by year of each disease according to INE data.
dcast(INE_data[,.(t = sum(deaths)), by = . (year, disease)], formula = year ~ disease) 
# According to INE data, the count of deaths caused by TB in 2016 was is 313
dcast(IHME_deaths_collapsed[, .(t = round(sum(deaths),1) ), by= .(year_id, disease)], formula = year_id ~disease) 

# Mujeres en edad reproductiva
dcast(IHME_deaths_collapsed[ sex_id == 2 & age_group_id >= 7 & age_group_id <= 15, 
                             .(round(sum(deaths),1)), by=.(disease, year_id)],
      disease~ year_id)

dcast(INE_data[sex == 2 & agegroup >= 7 & agegroup <= 15, 
                .(round(sum(deaths),1)), by=.(disease, year)],
      disease~ year)

# Population of women in reproductive age estimations from INE:
repr_pop = data.table(year = 2009:2016, repr_pop = c(
                                          4514410,
                                          4643678,
                                          4777842,
                                          4914230,
                                          5053437,
                                          5194887,
                                          5337050,
                                          5481150))
# Rates
#   INE
round(data.table(merge(dcast(INE_data[ sex == 2 & agegroup >= 7 & agegroup <= 15, 
                .(round(sum(deaths),1)), by=.(disease, year)],
      year ~ disease), repr_pop, by="year"))[, .( HIV_rate = 100000*HIV/repr_pop,
                                                 TB_rate = 100000*TB/repr_pop,
                                                 Mlr_rate = 100000*Malaria/repr_pop)], 1)
#   IHME
round(data.table(merge(dcast(IHME_deaths_collapsed[ sex_id == 2 & age_group_id >= 7 & age_group_id <= 15, 
                                       .(round(sum(deaths),1)), by=.(disease, year = year_id)],
                             year ~ disease), repr_pop, by="year"))[, .( HIV_rate = 100000*HIV/repr_pop,
                                                                         TB_rate = 100000*TB_all/repr_pop,
                                                                         Mlr_rate = 100000*Malaria/repr_pop)], 1)
# According to IHME data, before correction this was 22.7, after correction it is:
#    1:      TB 387.2574
#    2:  TB-HIV  45.9772

# Old data:
#   IHME Corrected data suggests a very large amount of TB deaths.
#   Deaths by cause:
#   IHME_deaths_collapsed[(year_id == 2016) & (cause_id %in% c(297, 954, 934, 946, 947,948, 949, 950)), 
#        .(t = round(sum(deaths), 1)), 
#              by = cause_id]
#   Results: 
#    cause_id  t
#    297       1.258910e-01 /
#    934       4.481442e+02 / Drug-susceptible tuberculosis
#    946       1.152385e-03 / Multidrug-resistant tuberculosis without extensive drug resistance
#    948       54 / HIV-AIDS - Drug-susceptible Tuberculosis

# This was done with uncollapsed INE data:
#   Now with INE data: Lets see how TB death is distr among causes
#   defsData[[2016]][(CaudefPRE %in% c("A15", "A16", "A17", "A18", "A19","B90")) | 
#                   (Caudef %in% c("A301", "A302", "J65X", "K230", "K673", "M011",
#                                  "N330", "M490", "M900", "N740", "N741", "O980", "K930",
#                                  "P370", "Z030", "Z111", "Z201", "Z232", "U843")), 
#                 .(conteo = .N), 
#                 by = .(Caudef) ]
#   Top 3 are: 
#   A162    209 / Tuberculosis de pulmón, sin mención de confirmación bacteriológica o histológica
#   A169     64 / Tuberculosis respiratoria no especificada, sin mención de confirmación bacteriológica o histológica
#   A199     16 / Tuberculosis miliar, sin otra especificación


# Now the geographical differences:
mapData = merge(IHMELocations[, .("municode" = factor(adm2_country_code), adm2_gbd_id)], 
                IHME_deaths_collapsed[(year_id == 2016) &
                          (disease %in% c("TB_all")), 
                        .(values_ = sum(deaths)), 
                        by = .(location_id)],
                 by.x = "adm2_gbd_id", by.y="location_id")
mapData$values = cut(mapData$values_, c(0, 1, 5, 10, 25, 50, 100), right = F)
#mapData$values = cut_number(mapData$values_, 5)
mapData$year = 2016
mapData$pop = GTMuniPopulation(as.integer(as.character(mapData$municode)), mapData$year)
plot = gtmap_muni(mapData, depto_color = "#00000055", muni_color = "#AAAAAAAA")
plot + labs(title="TB deaths counts by municipality \naccording to IHME corrected causes of death.") + 
    scale_fill_manual(values=c("#111129","#112255","#114466","#1256BA", "#6387CD", "#95DCEF"),  
                      na.value="#EFEFEF") 
mapData$values = 100000 * mapData$values_ / mapData$pop
mapData$values = cut(mapData$values, c(0,1, 5, 10, 25, 50, 100), right = F)
plot = gtmap_muni(mapData, depto_color = "#00000055", muni_color = "#AAAAAAAA")
plot + 
#    scale_fill_gradient2(na.value = "#444444", midpoint = 15,
#             low = "black", mid="#314278", high="#99BAFF", name="Rate") + 
    scale_fill_manual(values=c("#111129","#112255","#114466","#1256BA", "#6387CD", "#95DCEF"),  
                      na.value="#EFEFEF") + 
    labs(title="TB Mortality Rate per 100,000 population\naccording to IHME corrected causes of death")

ineMapData = INE_data[disease %in% c("TB") & (year == 2016),.(values_ = sum(deaths)), by = .(municode)]
mapData = ineMapData
mapData$values = cut(mapData$values_, c(0, 1, 5, 10, 25, 50, 100), right = F)
mapData$municode = str_replace(mapData$municode, "^0(\\d)", "\\1")
plot = gtmap_muni(mapData, depto_color = "#00000055", muni_color = "#AAAAAAAA")
plot + labs(title="TB deaths counts by municipality\naccording to national vital statistics") + 
    scale_fill_manual(values=c("#111129","#112255","#114466","#1256BA", "#6387CD", "#95DCEF"),  
                      na.value="#EFEFEF")
mapData$year = 2016
mapData$pop = GTMuniPopulation(as.integer(as.character(mapData$municode)), mapData$year)
mapData$values = 100000* mapData$values_ / mapData$pop
mapData$values = cut(mapData$values, c(0,1, 5, 10, 25, 50, 100), right = F)
plot = gtmap_muni(mapData, depto_color = "#00000055", muni_color = "#AAAAAAAA")
plot + 
#    scale_fill_gradient2(na.value = "#444444", midpoint = 150,
#                         low = "black", mid="#314278", high="#99BAFF", name="Rate") +
    scale_fill_manual(values=c("#111129","#112255","#114466","#1256BA", "#6387CD", "#95DCEF"),  
                      na.value="#EFEFEF") +
    labs(title="TB Mortality Rate per 100,000 habitants\naccording to National vital statistics.")

# Age distr. for HIV, TB and Malaria:
ggplot(data = 
           rbind(merge(IHME_deaths_collapsed[(year_id == 2016) &
                                     ( disease %in% c("HIV", "Malaria", "TB") ), 
                                 .(values_ = round(sum(deaths),2)), 
                                 by = .(age_group_id, disease)], 
                       IHMEAgeGroups, 
                       by.x="age_group_id", 
                       by.y = "age_group_id")[,.(age_group_name, values_,disease, source="IHME")],
                 merge(INE_data[(year == 2016) &
                                    ( disease %in% c("HIV", "Malaria", "TB") ), 
                                .(values_ = round(sum(deaths),2)), 
                                by = .(agegroup, disease)], 
                       IHMEAgeGroups, 
                       by.x="agegroup", 
                       by.y = "age_group_id")[,.(age_group_name, values_,disease, source="INE")]
             )) +
    geom_col(aes(age_group_name,values_, fill=source, group = source), position="dodge") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title="2016 deaths age distribution comparison", y = "count") +
    facet_grid(disease ~ ., scales = "free_y")

# Time series
tb_ts = rbind(INE_data[disease=="TB", 
               .(count = sum(deaths), source = "INE"), 
               by = . (year)],
      IHME_deaths_collapsed[disease=="TB_all", 
                            .(count = sum(deaths), source="IHME"), 
                            by = . (year = year_id)])
tb_ts[, pop := mapply(function (i)     sum(GTMuniPopulation(dt.munisGT$COD_MUNI__, rep(i, nrow(dt.munisGT))), na.rm = T), year) ]
tb_ts[, goal := 0]
tb_ts[, rate := count / pop]
new_pop = sum(GTMuniPopulation(dt.munisGT$COD_MUNI__, rep(max(tb_ts$year), nrow(dt.munisGT))), na.rm = T)
tb_ts = rbind(tb_ts, data.table( year = c(2016, 2016, 2017, 2017), 
                                 count = c(0,0,0,0),
                                 source = c("INE", "IHME", "INE", "IHME"),
                                 rate = c(tb_ts[source=="INE" & year == max(year), rate],
                                          tb_ts[source=="IHME" & year == max(year), rate],
                                          0.8*tb_ts[source=="INE" & year == max(year), count/new_pop],
                                          0.8*tb_ts[source=="IHME" & year == max(year), count]/new_pop),
                                 pop = c(0,0,0,0),
                                 goal = c(1,1,1,1)
                                 ))
ggplot(data = tb_ts) +
    geom_line(aes(year,100000 * rate,  colour = source, linetype= factor(goal)) ) +
    scale_color_discrete(name = "", 
             labels = c("TB deaths before correction", 
                        "TB deaths after correction")) + 
    scale_linetype_discrete(labels=c("Observed rate", "Target rate"), name="" ) +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5 )) +
    scale_x_continuous(breaks = unique(tb_ts$year)) +
    labs(title = "TB mortality rate (per 100,000 people) - National level", y="Rate", x= "Year")

# ------Trends----------
(IHME_deaths_collapsed, 