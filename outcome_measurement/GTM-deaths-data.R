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

collapse_ine_datasets = TRUE 
if (collapse_ine_datasets) {
    tb_pre = c("A15", "A16", "A17", "A18", "A19","B90")
    tb_other = c("A301", "A302", "J65X", "K230", "K673", "M011",
                   "N330", "M490", "M900", "N740", "N741", "O980", "K930",
                   "P370", "Z030", "Z111", "Z201", "Z232", "U843") 
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
IHME_deaths_collapsed = read.csv("./PCE/Outcome Measurement Data/MULTI/VR/vr_redistributed_collapsed_diseases.csv")
IHME_deaths_collapsed = data.table(IHME_deaths_collapsed)
IHMELocations = data.table(read.csv("PCE/Covariates and Other Data/GIS/GTM_muni_merges_2009_2016.csv"))
IHMEAgeGroups = data.table(read.csv("PCE/Outcome Measurement Data/MULTI/VR/age_group_ids.csv"))

INE_data = read.csv("PCE/Outcome Measurement Data/MULTI/VR/ine_collapsed_data.csv")
INE_data = data.table(INE_data)
# Count of deaths by year of each disease according to INE data.
dcast(INE_data[,.(t = sum(deaths)), by = . (year, disease)], formula = year ~ disease) 
# According to INE data, the count of deaths caused by TB in 2016 was is 313
dcast(IHME_deaths_collapsed[, .(t = sum(deaths)), by= .(year_id, disease)], formula = year_id ~disease) 
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

plot(merge(IHME_deaths_collapsed[(year_id == 2016) &
               ( disease %in% c("TB", "TB-HIV") ), 
             .(values_ = round(sum(deaths),2)), 
             by = .(age_group_id)], IHMEAgeGroups, by.x="age_group_id", by.y = "age_group_id")[,
       .(age_group_name, values_)], main="2016 TB deaths age distribution \n- IHME, o INE")

points(merge(INE_data[(year == 2016) &
        ( disease %in% c("TB", "TB-HIV") ), 
    .(values_ = round(sum(deaths),2)), 
    by = .(agegroup)], IHMEAgeGroups, by.x="agegroup", by.y = "age_group_id")[,
    .(age_group_name, values_)])


# Now the geographical differences:
mapData = merge(IHMELocations[, .("municode" = factor(adm2_country_code), adm2_gbd_id)], 
                IHME_deaths_collapsed[(year_id == 2016) &
                          (disease %in% c("TB", "TB-HIV")), 
                        .(values_ = sum(deaths)), 
                        by = .(location_id)],
                 by.x = "adm2_gbd_id", by.y="location_id")
mapData$values = cut(mapData$values_, c(0, 1, 2, 10, 20, 30, 40, 50, 100), right = F)
#mapData$values = cut_number(mapData$values_, 5)
mapData$year = 2016
mapData$pop = GTMuniPopulation(as.integer(as.character(mapData$municode)), mapData$year)
plot = gtmap_muni(mapData)
plot + labs(title="TB Deaths \naccording to IHME corrected causes of death.") + 
    scale_fill_manual(values=c("#112255","#114466","#2266AA", "#3377DD", "#55AAFF"),  
                      na.value="#333333")
mapData$values = 100000 * mapData$values_ / mapData$pop
mapData$values = cut(mapData$values, c(0,1, 5, 10, 25, 50, 100), right = F)
plot = gtmap_muni(mapData, depto_color = "#FFFFFF22")
plot + 
#    scale_fill_gradient2(na.value = "#444444", midpoint = 15,
#             low = "black", mid="#314278", high="#99BAFF", name="Rate") + 
    scale_fill_manual(values=c("#111111","#112255","#114466","#1256BA", "#6387CD", "#95DCEF"),  
                      na.value="#222222") +
    labs(title="TB Mortality Rate per 100,000 population\naccording to GBD corrected causes of death")

ineMapData = INE_data[disease %in% c("TB") & (year == 2016),.(values_ = sum(deaths)), by = .(municode)]
mapData = ineMapData
mapData$values = cut(mapData$values_, c(0, 1, 10, 20, 40, 75, 100), right = F)
mapData$municode = str_replace(mapData$municode, "^0(\\d)", "\\1")
plot = gtmap_muni(mapData)
plot + labs(title="TB deaths counts \naccording to national vital statistics") + 
    scale_fill_manual(values=c("#112255","#114466","#2266AA", "#3377DD", "#55AAFF"),  
                        na.value="#333333")
mapData$year = 2016
mapData$pop = GTMuniPopulation(as.integer(as.character(mapData$municode)), mapData$year)
mapData$values = 100000* mapData$values_ / mapData$pop
mapData$values = cut(mapData$values, c(0,1, 5, 10, 25, 50, 100), right = F)
plot = gtmap_muni(mapData, depto_color = "#FFFFFF22", )
plot + 
#    scale_fill_gradient2(na.value = "#444444", midpoint = 150,
#                         low = "black", mid="#314278", high="#99BAFF", name="Rate") +
    scale_fill_manual(values=c("#111111","#112255","#114466","#1256BA", "#6387CD", "#95DCEF"),  
                      na.value="#222222") +
    labs(title="TB Mortality Rate per 100,000 habitants\naccording to National vital statistics.")

# Now with HIV:
INECountsHIV = defsData[[2016]][(CaudefPRE %in% c("B20", "B21", "B22", "B23", "B24")) | 
                                 (Caudef %in% c("R75X", "Z114", "Z206", "Z21X", "Z717")),
                             .(conteo = .N), 
                             by = .(agegroup = (4 + ceiling((Edadif+1)/5)), Sexo, Mupocu)]
INECountsHIV[,.(t = sum(conteo))] # 358
IHMEDefsData[(year_id == 2016) & (cause_id %in% c(298, 948, 949, 950, 300)), .(t = sum(deaths))] # 703.7

# Malaria: 
INECountsMlr = defsData[[2016]][(CaudefPRE %in% c("B50", "B51", "B52", "B53", "B54")) |
                                (Caudef %in% c("P373", "P374")),
                                .(conteo = .N), 
                                by = .(agegroup = (4 + ceiling((Edadif+1)/5)), Sexo, Mupocu)]
INECountsMlr[,.(t = sum(conteo))] # 6
IHMEDefsData[(year_id == 2016) & (cause_id %in% c(345, 856, 857, 858)), .(t = sum(deaths))] # 8.1

INECountsHIV[, sum(conteo), by = agegroup]


# Mujeres en edad reproductiva
defsData[[2009]][ ((CaudefPRE %in% c("A15", "A16", "A17", "A18", "A19","B90"
                                     )) | 
                     (Caudef %in% c("A301", "A302", "J65X", "K230", "K673", "M011",
                                    "N330", "M490", "M900", "N740", "N741", "O980", "K930",
                                    "P370", "Z030", "Z111", "Z201", "Z232", "U843"
                                    )))
                  & Sexo == 2 & Edadif >10 & Edadif < 54, 
                 .(conteo = .N) ] # 60
ihme_temp = IHMEDefsData[ sex_id == 2 & age_group_id >= 7 & age_group_id <= 54  
              & (cause_id %in% c(297, 954, 934, 946, 947)), .(sum(deaths)), by=year_id] # 
ihme_temp[order(year_id)]
