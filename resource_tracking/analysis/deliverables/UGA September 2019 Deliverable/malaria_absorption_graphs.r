
#Author - Emily Linebarger 
# Running quick analysis for TERG meeting background information. Trying to answer the question - 
#Were they spending any money on prevention interventions in 2017 that they weren't in 2018? 

# dt = mapped_data[loc_name=="uga" & grant_disease=="malaria" & data_source=="pudr" & grant_period%in%c('2015-2017', '2018-2020') & file_iteration=='final', 
#                  .(budget=sum(budget, na.rm=T), expenditure=sum(expenditure, na.rm=T)), by=c('grant', 'grant_period', 'pudr_semester', 'gf_module', 'gf_intervention', 'disease')]
# saveRDS(dt, "C:/Users/elineb/Desktop/temp_uga_mal_pudr.rds")

#This is just summed PUDR data
dt = readRDS("C:/Users/elineb/Desktop/temp_uga_mal_pudr.rds")
source("./resource_tracking/visualizations/graphing_functions.r")

unique(dt[, .(grant, grant_period, pudr_semester)][order(grant_period, grant, pudr_semester)])

#First, break down each PUDR by module. 
#Break everything down by grant and semester! 

#Subset to only the semesters you want. 
dt = dt[(grant=="UGA-M-TASO" & grant_period=="2015-2017" & pudr_semester=="3-AB") | 
          (grant=="UGA-M-TASO" & grant_period=="2018-2020" & pudr_semester=="1-AB") | 
          (grant=="UGA-M-MoFPED" & grant_period=="2015-2017" & pudr_semester=="2-B:3-A") | 
          (grant=="UGA-M-MoFPED" & grant_period=="2018-2020" & pudr_semester=="1-A") | 
          (grant=="UGA-M-MoFPED" & grant_period=="2018-2020" & pudr_semester=="1-B")]

#Merge on abbreviated modules 
abbrev = readRDS("J:/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/all_interventions.rds")
setnames(abbrev, c('module_eng', 'intervention_eng'), c('gf_module', 'gf_intervention'))
dt = merge(dt, abbrev[, .(disease, gf_module, gf_intervention, abbrev_mod_eng, abbrev_int_eng)], by=c('disease', 'gf_module', 'gf_intervention'), all.x=T)


dt[pudr_semester=="3-AB", pudr_semester:="Full-year 2017"]
dt[pudr_semester=="1-AB", pudr_semester:="Full-year 2018"]
dt[pudr_semester=="2-B:3-A", pudr_semester:="Semester 2 2016-Semester 1 2017"]
dt[pudr_semester=="1-A", pudr_semester:="Semester 1 2018"]
dt[pudr_semester=="1-B", pudr_semester:="Semester 2 2018"]

dt[, absorption:=round((expenditure/budget)*100, 2)]
dt[absorption>150, absorption:=150]

p1 = ggplot(dt[grant=="UGA-M-MoFPED"], aes(x=abbrev_mod_eng, y=absorption, fill=pudr_semester)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  theme(legend.position="none") + 
  coord_flip() +
  labs(x="Module", y="Absorption (%)", title="Absorption by module for UGA-M-MoFPED", fill="")

p2 = ggplot(dt[grant=="UGA-M-TASO"], aes(x=abbrev_mod_eng, y=absorption, fill=pudr_semester)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  theme(legend.position="none") + 
  coord_flip() +
  labs(x="Module", y="Absorption (%)", title="Absorption by module for UGA-M-MoFPED", fill="")


#Then, for specific interventions, compare 2017 to 2018 for each grant. 

#Start with vector control. 
p3 = ggplot(dt[grant=="UGA-M-MoFPED" & abbrev_mod_eng=="Vector control"], aes(x=abbrev_int_eng, y=absorption, fill=abbrev_int_eng)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  theme(legend.position="none") + 
  labs(x="Module", y="Absorption (%)", title="Absorption for Vector Control for UGA-M-MoFPED", fill="")

p4 = ggplot(dt[grant=="UGA-M-TASO" & abbrev_mod_eng=="Vector control"], aes(x=abbrev_int_eng, y=absorption, fill=abbrev_int_eng)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  theme(legend.position="none") + 
  labs(x="Module", y="Absorption (%)", title="Absorption for Vector Control for UGA-M-TASO", fill="")


#Then specific prevention interventions.  
p5 = ggplot(dt[grant=="UGA-M-MoFPED" & abbrev_mod_eng=="Specific prev. interventions"], aes(x=abbrev_int_eng, y=absorption, fill=abbrev_int_eng)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  theme(legend.position="none") + 
  labs(x="Module", y="Absorption (%)", title="Absorption for specific prevention interventions for UGA-M-MoFPED", fill="")

p6 = ggplot(dt[grant=="UGA-M-TASO" & abbrev_mod_eng=="Specific prev. interventions"], aes(x=abbrev_int_eng, y=absorption, fill=abbrev_int_eng)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  theme(legend.position="none") + 
  labs(x="Module", y="Absorption (%)", title="Absorption for specific prevention interventions for UGA-M-TASO", fill="")


#Same last 4 graphs but with budget! 
#Start with vector control. 
p7 = ggplot(dt[grant=="UGA-M-MoFPED" & abbrev_mod_eng=="Vector control"], aes(x=abbrev_int_eng, y=budget, fill=abbrev_int_eng)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  theme(legend.position="none") + 
  labs(x="Module", y="Budget", title="Budget for Vector Control for UGA-M-MoFPED", fill="")

p8 = ggplot(dt[grant=="UGA-M-TASO" & abbrev_mod_eng=="Vector control"], aes(x=abbrev_int_eng, y=budget, fill=abbrev_int_eng)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  theme(legend.position="none") + 
  labs(x="Module", y="Budget", title="Budget for Vector Control for UGA-M-TASO", fill="")


#Then specific prevention interventions.  
p9 = ggplot(dt[grant=="UGA-M-MoFPED" & abbrev_mod_eng=="Specific prev. interventions"], aes(x=abbrev_int_eng, y=budget, fill=abbrev_int_eng)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  theme(legend.position="none") + 
  labs(x="Module", y="Budget", title="Budget for specific prevention interventions for UGA-M-MoFPED", fill="")

p10 = ggplot(dt[grant=="UGA-M-TASO" & abbrev_mod_eng=="Specific prev. interventions"], aes(x=abbrev_int_eng, y=budget, fill=abbrev_int_eng)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  theme(legend.position="none") + 
  labs(x="Module", y="Budget", title="Budget for specific prevention interventions for UGA-M-TASO", fill="")

#Final two graphs with both prevention modules pooled together. 
p11 = ggplot(dt[grant=="UGA-M-MoFPED" & (abbrev_mod_eng=="Specific prev. interventions"| abbrev_mod_eng=="Vector control")], aes(x=abbrev_int_eng, y=budget, fill=abbrev_int_eng)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  coord_flip() + 
  theme(legend.position="none") + 
  labs(x="Module", y="Budget", title="Budget for all prevention interventions for UGA-M-MoFPED", fill="")

p12 = ggplot(dt[grant=="UGA-M-TASO" & (abbrev_mod_eng=="Specific prev. interventions"| abbrev_mod_eng=="Vector control")], aes(x=abbrev_int_eng, y=budget, fill=abbrev_int_eng)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~pudr_semester) + 
  theme_bw(base_size=16)+
  coord_flip() + 
  theme(legend.position="none") + 
  labs(x="Module", y="Budget", title="Budget for all prevention interventions for UGA-M-TASO", fill="")

pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/UGA Malaria September 2019/uga_malaria_graphs2.pdf", height=10, width=15)
p1 
p2 
p11
p12
p7
p8
p9
p10
p3
p4
p5
p6


dev.off() 

#Descriptive stats 
dt[abbrev_mod_eng=="Vector control", .(budget=sum(budget, na.rm=T)), by=c('grant', 'grant_period', 'pudr_semester', 'abbrev_int_eng')][order(grant_period, grant, abbrev_int_eng)]
