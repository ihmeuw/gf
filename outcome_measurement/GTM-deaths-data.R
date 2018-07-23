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
    IHMEDefsData = read.csv("PCE/Outcome Measurement Data/VR/redistribution_20180716.csv")
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
    irena_coll = read.csv("PCE/Outcome Measurement Data/VR/vr_redistributed_collapsed.csv")
    irena_coll = data.table(irena_coll)
    irena_coll[death_id_collapsed == "tb", sum(deaths), by=.(year_id)]
    IHME_deaths_collapsed[disease %in% c("TB", "TB-HIV"), sum(deaths), by=.(year_id)]
    # The numbers are the same. 
    # Now replacing Irena's with this file because it is more complete as it covers the other 3 diseases.
    write.csv(IHME_deaths_collapsed, 
              "./PCE/Outcome Measurement Data/VR/vr_redistributed_collapsed.csv")
}

# Load IHME data
IHME_deaths_collapsed = read.csv("./PCE/Outcome Measurement Data/VR/vr_redistributed_collapsed.csv")
IHME_deaths_collapsed = data.table(IHME_deaths_collapsed)
IHMELocations = data.table(read.csv("PCE/Covariates and Other Data/GIS/GTM_muni_merges_2009_2016.csv"))
IHMEAgeGroups = data.table(read.csv("PCE/Outcome Measurement Data/VR/age_group_ids.csv"))


# Load deaths INE data up to 2016.
defsData = loadDeathsData()

# Lets explore TB deaths:
INECounts = defsData[[2016]][(CaudefPRE %in% c("A15", "A16", "A17", "A18", "A19","B90")) | 
                     (Caudef %in% c("A301", "A302", "J65X", "K230", "K673", "M011",
                                    "N330", "M490", "M900", "N740", "N741", "O980", "K930",
                                    "P370", "Z030", "Z111", "Z201", "Z232", "U843")), 
                 .(conteo = .N), 
                 by = .(agegroup = (4 + ceiling((Edadif+1)/5)), Sexo, Mupocu) ]

# First of all lets compare TB deaths according to INE and IHME.
INECounts[,.(t = sum(conteo))] # 313
IHME_deaths_collapsed[(year_id == 2016) & (disease %in% c("TB", "TB-HIV") ), .(t = sum(deaths)), by= disease] 
# Before correction this was 22.7, after correction it is:
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

# Now with INE data: Lets see how TB death is distr among causes
defsData[[2016]][(CaudefPRE %in% c("A15", "A16", "A17", "A18", "A19","B90")) | 
                   (Caudef %in% c("A301", "A302", "J65X", "K230", "K673", "M011",
                                  "N330", "M490", "M900", "N740", "N741", "O980", "K930",
                                  "P370", "Z030", "Z111", "Z201", "Z232", "U843")), 
                 .(conteo = .N), 
                 by = .(Caudef) ]
# Top 3 are: 
# A162    209 / Tuberculosis de pulmón, sin mención de confirmación bacteriológica o histológica
# A169     64 / Tuberculosis respiratoria no especificada, sin mención de confirmación bacteriológica o histológica
# A199     16 / Tuberculosis miliar, sin otra especificación

merge(IHME_deaths_collapsed[(year_id == 2016) &
               ( disease %in% c("TB", "TB-HIV") ), 
             .(values_ = round(sum(deaths),2)), 
             by = .(age_group_id)], IHMEAgeGroups, by.x="age_group_id", by.y = "age_group_id")[,
       .(age_group_name, values_)]

# Plot the age distribution
ggplot(data = merge(INECounts[, .(c = .N), by = .(agegroup= factor(agegroup), Sexo = factor(Sexo)) ],
                  IHMEAgeGroups, by.x="agegroup", by.y= "age_group_id"), 
       aes(age_group_name, c, group=Sexo)) + geom_col(aes(fill=Sexo)) + scale_fill_manual(values = c("blue", "red")) + 
      labs(title="Distribution of TB deaths by age group", caption="Sexo 1 means Males and 2 Females", 
           y="Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Now the geographical differences:
mapData = merge(IHMELocations[, .("municode" = factor(adm2_country_code), adm2_gbd_id)], 
                IHMEDefsData[(year_id == 2016) &
                          (cause_id %in% c(948,949,950, 297, 954, 934, 946, 947)), 
                        .(values_ = sum(deaths)), 
                        by = .(location_id)],
                 by.x = "adm2_gbd_id", by.y="location_id")
#mapData$values = cut(mapData$values_, c(0, 1, 2, 10, 20, 30, 40, 50, 100), right = F)
mapData$values = cut_number(mapData$values_, 5)
mapData$year = 2016
mapData$pop = GTMuniPopulation(as.integer(as.character(mapData$municode)), mapData$year)
plot = gtmap_muni(mapData)
plot + labs(title="TB Deaths \naccording to IHME corrected causes of death.") + scale_fill_manual(values=c("#112255","#114466","#2266AA", "#3377DD", "#55AAFF"),  na.value="#333333")
mapData$values = 100000* mapData$values_ / mapData$pop
plot = gtmap_muni(mapData, depto_color = "#22222277")
plot + scale_fill_distiller(name="Rate", palette = "Blues", direction = -1, na.value = "#888888") +
    labs(title="TB Mortality Rate per 100,000 population\naccording to GBD corrected causes of death")

ineMapData = INECounts[,.(values_ = sum(conteo)), by = .(municode = Mupocu)]
mapData = ineMapData
mapData$values = cut(mapData$values_, c(0, 1, 2, 10, 20, 30, 40, 50, 100), right = F)
mapData$municode = str_replace(mapData$municode, "^0(\\d)", "\\1")
plot = gtmap_muni(mapData, extra =   scale_fill_manual(values=c("#444444","#114466","#2266AA", "#3377DD", "#55AAFF"),  na.value="black") )
plot + labs(title="TB Mortality rate \naccording to national vital statistics")
mapData$year = 2016
mapData$pop = GTMuniPopulation(as.integer(as.character(mapData$municode)), mapData$year)
mapData$values = 1000* mapData$values_ / mapData$pop
plot = gtmap_muni(mapData, depto_color = "#22222277")
plot + scale_fill_distiller(name="Percent", palette = "Blues", direction = -1, na.value = "#444444") +
    labs(title="TB Mortality Rate per 1,000 habitants\naccording to National vital statistics.")



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
