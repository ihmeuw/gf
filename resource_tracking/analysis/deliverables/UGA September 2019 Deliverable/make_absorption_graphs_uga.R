#-------------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Prepare absorption graphs for Uganda TB grants
# Modified from code for DRC June tb deliverable 
# DATE: July 10, 2019
#--------------------------------------------------------

# Is it possible to get a similar absorption graphic to this one for Uganda’s TB (MoFPED)
# and TB-HIV (TASO) (only TB related modules)?  (or using the Tableau absorption dashboard 
#                                                if that makes most sense at this stage?)

rm(list=ls())
setwd("C:/Users/elineb/Documents/gf") #Set to root of repository 
source("./resource_tracking/prep/_common/set_up_r.r") #Set up base resource tracking filepaths. 
output_dir = "J:/Project/Evaluation/GF/resource_tracking/visualizations/random/uga/"

#Read in data 
absorption = readRDS(paste0(dir, "_gf_files_gos/uga/prepped_data/absorption_uga.rds"))
absorption[, absorption:=(expenditure/budget)*100]
absorption = absorption[disease%in%c('rssh', 'tb') & grant%in%c('UGA-C-TASO', 'UGA-T-MoFPED') & grant_period=="2018-2020"]
budgets = readRDS(paste0(dir, "_gf_files_gos/combined_prepped_data/final_budgets.rds"))

budgets[grant_period == "2018-2020" & grant_disease%in%c('tb', 'rssh') & grant%in%c('UGA-C-TASO', 'UGA-T-MoFPED'), 
        .(budget = sum(budget, na.rm=T)), by=c('grant', 'gf_module', 'gf_intervention')]

all_ints = fread(paste0(mapping_dir, "all_interventions.csv"))
all_ints = unique(all_ints[disease%in%c("tb", "rssh"), .(code, abbrev_mod_eng, abbrev_int_eng)])

#Create tables of all unique values of intervention/semester for graphing later. 
absorption = merge(absorption, all_ints, by='code', all.x=T)
absorption[is.na(abbrev_int_eng)|abbrev_int_eng=="", abbrev_int_eng:=gf_intervention]

absorption[abbrev_int_eng=="Collaborative activities with other programs and sectors (MDR-TB)", abbrev_int_eng:='Collaborative activities MDR-TB']
absorption[abbrev_int_eng=='Policy, planning, coordination and management of national disease control programs', abbrev_int_eng:='National disease programs']
absorption[abbrev_int_eng=="Key populations (TB care and prevention) – Others", abbrev_int_eng:='Key populations-Other']
stopifnot(nrow(absorption[is.na(abbrev_int_eng) | abbrev_int_eng==""])==0)

potential_ints = expand.grid(abbrev_int_eng = unique(absorption$abbrev_int_eng), grant = c("UGA-C-TASO", 'UGA-T-MoFPED'), semester=c('Semester 1', 'Semester 2', 'Semester 1-2'))
#----------------------------------------------------------
#1. Print off descriptive stats using different groupings
#----------------------------------------------------------

#For all of 2018, what was the overall absorption rate for the two tb grants?
dt1 = absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'semester')]
dt1[, absorption:=(expenditure/budget)*100]

#For both S1 and S1-2, what was the absorption by intervention for the target grants? 
byVars = c('grant','abbrev_int_eng', 'semester')
dt2 = absorption[, .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=byVars]

dt2[, absorption:=(expenditure/budget)*100]

#Need to find a way to handle infinite absorption! 
dt2[is.infinite(absorption), inf_absorption:=TRUE]
dt2[is.infinite(absorption), absorption:=200.00]

#Rectangularize data
dt2 = merge(dt2, potential_ints, by=c('abbrev_int_eng', 'grant', 'semester'), allow.cartesian=T, all.y=T)
dt2[is.na(absorption), absorption:=0]

range(dt2$absorption)

#---------------------------------------------------------
# 2. Graph data 
#---------------------------------------------------------
dt2[absorption>200, absorption:=200]
dt2[round(absorption)!=0, label:=paste0(round(absorption), "%")]

ggplot(dt2, aes(x=abbrev_int_eng, y=absorption, fill=grant)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_bw(base_size=12) + 
  coord_flip() + 
  facet_wrap(~semester) + 
  labs(title = "2018 absorption by intervention for current Uganda\nTB + HIV/TB grants",
       x="Intervention", y="Absorption (%)", fill="Grant", 
       caption="*Absorption>200% has been capped at 200%\nShowing TB and RSSH modules only")
  
ggsave(paste0(output_dir, "2018_absorption_tb_", Sys.Date(), ".png"))  

ggplot(dt2[grant=="UGA-C-TASO" & semester!='Semester 2'], aes(x=abbrev_int_eng, y=absorption, fill=grant)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_bw(base_size=12) + 
  coord_flip() + 
  facet_wrap(~semester) + 
  geom_text(aes(label=label)) +
  labs(title = "2018 absorption by intervention for UGA-C-TASO",
       x="Intervention", y="Absorption (%)", fill="Grant", 
       caption="*Absorption>200% has been capped at 200%\nShowing TB and RSSH modules only")

ggsave(paste0(output_dir, "2018_absorption_tb_TASO_", Sys.Date(), ".png"))  

ggplot(dt2[grant=="UGA-T-MoFPED" & semester!='Semester 1-2'], aes(x=abbrev_int_eng, y=absorption, fill=grant)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_bw(base_size=11) + 
  coord_flip() + 
  facet_wrap(~semester) + 
  geom_text(aes(label=label)) +
  labs(title = "2018 absorption by intervention for UGA-T-MoFPED",
       x="Intervention", y="Absorption (%)", fill="Grant", 
       caption="*Absorption>200% has been capped at 200%\nShowing TB and RSSH modules only")

ggsave(paste0(output_dir, "2018_absorption_tb_MoFPED_", Sys.Date(), ".png"))  

#Want one more graph comparing absorption to the amount budgeted - does 0% absorption mean 0 budgeted? 

dt2[!is.na(budget) & budget==0, int_modified:=paste0("***", abbrev_int_eng)]
dt2[is.na(int_modified), int_modified:=abbrev_int_eng]
ggplot(dt2[grant=="UGA-T-MoFPED" & semester == "Semester 2"], aes(x=int_modified, y=absorption, fill=grant)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme_bw(base_size=11) + 
  coord_flip() + 
  labs(title = "2018 absorption by intervention for Uganda TB grants\nObservations with $0 budgeted marked with ***",
       x="Intervention", y="Absorption (%)", fill="Grant", 
       caption="*Absorption>200% has been capped at 200%\nShowing TB and RSSH modules only")

ggsave(paste0(output_dir, "2018_absorption_tb_0budget_", Sys.Date(), ".png"))  

#-----------------------------------------------------------
#Make one more series of graphs, for each PUDR individually, 
# that only show the interventions included in that PUDR. 
#-----------------------------------------------------------
absorption[, concat:=paste0(grant, "_", grant_period, "_", semester)]
absorption[, label:=paste0(round(absorption), "%")]
absorption[absorption>200, absorption:=200]
for (c in unique(absorption$concat)){
  grant = absorption[c==concat, unique(grant)]
  period = absorption[c==concat, unique(grant_period)]
  semester = absorption[c==concat, unique(semester)]
  
  ggplot(absorption[c==concat], aes(x=gf_intervention, y=absorption)) + 
    geom_bar(stat="identity", fill="indianred1") + 
    theme_bw(base_size=11) + 
    coord_flip() + 
    geom_text(aes(label=label)) +
    labs(title = paste0("Absorption for ", grant, " ", semester ),
         x="Intervention", y="Absorption (%)", 
         caption="*Showing TB and RSSH modules only\n*Absorption has been capped at 200%")
  ggsave(paste0(output_dir, "2018_absorption_", grant, "_", semester, ".png"), width=15, height=10)  
  
}
  
  
  