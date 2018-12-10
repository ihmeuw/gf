# ----------------------------------------------
# Audrey Batzel
#
# 11/26/2018
# 
# Graph achievement ratios of performance indicators across all PCE countries
# ----------------------------------------------


# --------------------
## Set up R / install packages
# --------------------
rm(list=ls())

library(data.table)
library(stringr)
library(reshape2)
library(ggplot2)
library(stats)
library(Rcpp)
library(readxl)
library(grid)
library(gridExtra)
library(googlesheets)
library(ggrepel)
library(dplyr)


out_dir <- "J:/Project/Evaluation/GF/outcome_measurement/multi_country/"
# --------------------  


# ----------------------------------------------
# Read in data from google sheeet
# ----------------------------------------------
# register a googlesheet object using gs_title()
kpis <- gs_title("Milestones, Activites and KPIs Table")
# read in the contents of a given worksheet as a data frame using gs_read()
dt <- gs_read(kpis, ws = "KPI targets & achievements")
dt <- as.data.table(dt)
# ----------------------------------------------


# ----------------------------------------------
# clean up the data table and edit strings
# ----------------------------------------------
dt <- dt[-c(1:5),]
names(dt) <- as.character(unlist(dt[1,]))
dt <- dt[-c(1),]

# split out KPI code
dt <- dt[, c("KPI_code", "KPI_text") := tstrsplit(KPI, ": ", fixed=TRUE)]
  # fix a couple that didn't work
  dt[KPI=="TCP-Other(1) Number of contacts of Sputum and Genexpert positive TB patients who are evaluated for infection and disease", KPI_code:= "TCP-Other(1)"]
  dt[KPI=="TCP-Other(1) Number of contacts of Sputum and Genexpert positive TB patients who are evaluated for infection and disease", KPI_text:= "Number of contacts of Sputum and Genexpert positive TB patients who are evaluated for infection and disease"]
  dt[KPI=="TB O-5(M):Couverture de traitement de la TB:Pourcentage de nouveaux cas et rechutes qui ont été notifiés et traités parmi le nombre estimé de cas de TB dans la même année (toutes formes de TB -bactériologiquement confirmées et cliniquement diagnostiquées)", 
     KPI_code:="TB O-5(M)"]
  dt[KPI=="TB O-5(M):Couverture de traitement de la TB:Pourcentage de nouveaux cas et rechutes qui ont été notifiés et traités parmi le nombre estimé de cas de TB dans la même année (toutes formes de TB -bactériologiquement confirmées et cliniquement diagnostiquées)", 
     KPI_text:="Couverture de traitement de la TB:Pourcentage de nouveaux cas et rechutes qui ont été notifiés et traités parmi le nombre estimé de cas de TB dans la même année (toutes formes de TB -bactériologiquement confirmées et cliniquement diagnostiquées)"]
  dt[KPI=="Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria", KPI_code := NA]
  dt[KPI=="Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria", KPI_text := "Nombre et pourcentage de structures de santé (Centres de Santé intégrés) qui rapporte une rupture de stock des tests de diagnostic rapide (TDR) malaria"]
  dt[KPI=="GP other-2 Nombre de SVS ayant recu le kit PEP dans les 72h", KPI_code:= "GP other-2"]
  dt[KPI=="GP other-2 Nombre de SVS ayant recu le kit PEP dans les 72h", KPI_text:= "Nombre de SVS ayant recu le kit PEP dans les 72h"]

# split out module from the code
dt<- dt[, c("module_code", "intervention_code") := tstrsplit(KPI_code, "-", fixed=TRUE)]
dt[grepl('Other', dt$module_code, ignore.case = TRUE), intervention_code:= paste('other ', intervention_code)]
dt[grepl('Other', dt$module_code, ignore.case = TRUE), module_code:= gsub('Other', '', module_code, ignore.case=TRUE)]

dt$KPI_code <- trimws(dt$KPI_code, "both")
dt$module_code <- trimws(dt$module_code, "both")
dt$intervention_code <- trimws(dt$intervention_code, "both")

dt[, intervention_code := gsub(' ', '', intervention_code)]
dt[, intervention_code := tolower(intervention_code)]
dt[intervention_code == "other(1)", intervention_code:= "other1"]

dt[, X10 := NULL]
dt[, X11 := NULL]

names(dt) <- c("country", "grant", "program_area", "type", "kpi", "target", "result", "achievement_ratio", "notes", "kpi_code", "kpi_text", 
               "module_code", "intervention_code")

# remove percent signs
dt[, target:= gsub('%', '', target)]
dt[, result:= gsub('%', '', result)]
dt[, achievement_ratio:= gsub('%', '', achievement_ratio)]
dt$achievement_ratio <- as.numeric(dt$achievement_ratio)

# delete duplicates of indicators within countries where achievement ratio is the same
dt <- unique(dt)  # there is one row exactly duplicated for Sudan

# save a copy of cleaned data
saveRDS(dt, paste0(out_dir, "cleaned_indicator_performance_data.rds"))

####### CAN LOAD IN THE DATA AND START HERE IF YOU DON'T NEED TO REFRESH THE GOOGLE SHEET:
dt <- readRDS(paste0(out_dir, "cleaned_indicator_performance_data.rds"))

# pull out where indicators are duplicated (in different grants) for the same country
dups <- dt[duplicated(dt, by=c("country", "program_area", "module_code", "intervention_code")), ]
# anti_join will remove those rows in the data table created
dt_dups_removed <- anti_join(dt, dups)

# merge duplicated back with their duplicate indicators in dt_dups_removed to compare achievement ratios between them (and make sure the reported numbers are actually the same/similar)
check_dups <- merge(dups, dt_dups_removed, by=c("country", "program_area", "module_code", "intervention_code"))
equal_ach_rat <- check_dups[achievement_ratio.x == achievement_ratio.y | (is.na(achievement_ratio.x) & is.na(achievement_ratio.y)), ] # these we definitely want to remove from dt

# where achievement ratios are not equal between them, examine further (it might be that they're off by <=1% because of how they were calculated but we want to remove those too)
not_equal_ach_rat <- check_dups[achievement_ratio.x != achievement_ratio.y | is.na(achievement_ratio.x) | is.na(achievement_ratio.y) , ]
subset <- not_equal_ach_rat[, .(country, program_area, module_code, intervention_code, grant.x, achievement_ratio.x, grant.y, achievement_ratio.y)]
  # based on this ^ modify "equal_ach_rat" to include cases where the achievement ratios are off by 1...

equal_ach_rat <- check_dups[achievement_ratio.x >= (achievement_ratio.y - 1) &
                            achievement_ratio.x <= (achievement_ratio.y + 1) |
                            (is.na(achievement_ratio.x & is.na(achievement_ratio.y))), ] 

# we want to remove the 15 cases where achievement ratios are equal from the data 
remove_rows <- equal_ach_rat[, .(country, program_area, module_code, intervention_code, grant.y, achievement_ratio.y)]
setnames(remove_rows, "grant.y", "grant")
setnames(remove_rows, "achievement_ratio.y", "achievement_ratio")

# do an anti_join with remove_rows and the original data table to remove those duplicates from the original
dt_without_dups <- anti_join(dt, remove_rows)
    
dt_orig <- copy(dt)
dt <- copy(dt_without_dups)
dt <- as.data.table(dt)
    
# add a remove_row column to dt_orig and resave that so it's easy to load in. 
remove_rows[, remove_dups:= TRUE]
dt_orig <- merge(dt_orig, remove_rows, by= c("country", "program_area", "module_code", "intervention_code", "grant", "achievement_ratio"), all=TRUE)
dt_orig[is.na(remove_dups), remove_dups:= FALSE]
saveRDS(dt_orig, paste0(out_dir, "cleaned_indicator_performance_data.rds"))

dt_orig[, avg_ach_rat := mean(achievement_ratio, na.rm=TRUE), by=c("country", "grant")]
dt_orig[avg_ach_rat==0, remove_whereGrantHasAllZeroes := TRUE]
dt_orig[, avg_ach_rat := NULL]
saveRDS(dt_orig, paste0(out_dir, "cleaned_indicator_performance_data.rds"))
# ----------------------------------------------


# ----------------------------------------------
# calculations for report
# ----------------------------------------------
calc_median <- function(dt, disease){
  dt2 <- dt[grepl(disease, program_area, ignore.case=TRUE),]
  dt2 <- dt2[!is.na(achievement_ratio),]
  result = median(dt2$achievement_ratio)
  countries_included = unique(dt2$country)
  my_list <- list("median" = result, "countries" = countries_included)
  return(my_list)
}

avgs_by_country_program_area <- dt[!is.na(achievement_ratio) & (remove_row != "X" | is.na(remove_row)), .(mean(achievement_ratio), .N), by=c("program_area", "country")]

dt[kpi_code== "TCS-1(M)", mean(achievement_ratio)]
dt[kpi_code== "TCS-1(M)", max(achievement_ratio)]
dt[kpi_code== "TCS-1(M)", min(achievement_ratio)]
# ----------------------------------------------


# ----------------------------------------------
# graph
# ----------------------------------------------
dt_graph <- dt[remove_row!="X" | is.na(remove_row), ]
dt_graph <- dt_graph[!is.na(module_code)]
dt_graph[type!="Coverage", module_code:= "Impact"]

dt_graph1 <- copy(dt_graph)
dt_graph1[achievement_ratio>200, achievement_ratio := 200]
dt_graph1 <- dt_graph1[!is.na(achievement_ratio)]

dt_graph1 <- dt_graph1[, setorderv(dt_graph1, c("module_code", "achievement_ratio"))]

g1 <- ggplot(dt_graph1, aes(x=module_code, y=achievement_ratio, color= country)) + theme_bw()+
  geom_point(size = 2, position = "jitter") + 
  ggtitle(paste0("Achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module") + 
  labs(caption = "Note: 5 points that had an achievement ratio of greater than 200% are graphed at 200% for clarity") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16), legend.title=element_blank(), 
        legend.text =element_text(size=14), plot.title = element_text(size=20)) 
g1


# collapse countries/grant where module_code/intervention_code is the same
dt_subset <- dt_graph[, .(module_code, intervention_code, achievement_ratio)]
dt_subset <- dt_subset[!is.na(achievement_ratio)]
dt_subset <- dt_subset[, .(avg_ach_ratio= mean(achievement_ratio),
                           max_ach_ratio= max(achievement_ratio),
                           min_ach_ratio= min(achievement_ratio)),
                         by=c("module_code", "intervention_code")]
dt_subset[max_ach_ratio>200, max_ach_ratio := 200]
dt_subset[avg_ach_ratio>200, avg_ach_ratio := 200]
dt_subset[min_ach_ratio>200, min_ach_ratio := 200]

dt_subset[, kpi_code:= paste(module_code, intervention_code)]

no_country_per_indicator <- unique(dt_graph[, .(country, module_code, intervention_code)])
no_country_per_indicator <- no_country_per_indicator[, .(no_observ = .N), by=c("module_code", "intervention_code")]

dt_subset <- merge(dt_subset, no_country_per_indicator, by=c("module_code", "intervention_code"))

pdf(paste0(out_dir, "achievement_ratios_by_kpi.pdf"), height = 9, width = 15)
g2 <- ggplot(dt_subset[], aes(x=module_code, y=avg_ach_ratio, color=module_code, size = no_observ)) + theme_bw()+
  geom_pointrange( aes(ymin= min_ach_ratio, ymax= max_ach_ratio), position= position_jitter(width = 0.40), shape = 21, fill = "white") + guides(color=FALSE) +
  scale_size_continuous(breaks= c(1, 2, 3, 4, 5), range=c(0.5,1.5), name = "Number of \ncountries reporting") + 
  ggtitle(paste0("Average achievement ratios of performance indicators by module")) +
  ylab("Achievement ratio") + xlab("Module")  +
  labs(caption = "Bars represent the max and min and points are the mean by module-intervention across countries and grants.
       Note: Where the max, mean, or min for a given module-intervention was higher than 200, it was changed to 200 for clarity in this figure.") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16), legend.title=element_text(size = 14), 
        legend.text =element_text(size=14), plot.title = element_text(size=20), plot.caption = element_text(size=12),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  geom_hline(yintercept=100, linetype="dashed", color="grey", alpha=0.6, size=2) 

# + 
#   geom_label(data= dt_subset[ kpi_code %in% c("CM 1b(m)", "TCS 1(m)"), ], 
#             aes(label= paste(module_code, "-", intervention_code)),
#             position = position_jitter(seed = 1)) 
g2
dev.off()
# ----------------------------------------------


