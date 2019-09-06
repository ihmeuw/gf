#-------------------------------------------------------------
# RUN SIMPLE LINEAR REGRESSIONS BETWEEN 
# SECOND LINE DRUGS, GENEXPERT TESTING, AND GF SPENDING ON THESE 
# MODULES SPECIFICALLY FOR EFFICIENCY ANALYSIS 
#--------------------------------------------------------------

# Read in expenditures data - don't want to redistribute to subnational level, but do want to cumulatively sum. 
source('./impact_evaluation/gtm/set_up_r.r')
options(scipen=100)


resource_tracking <- readRDS(outputFile2a)
resource_tracking[, year:=floor(date)]
byVars = names(resource_tracking)[!names(resource_tracking)%in%c('date', 'year')]
resource_tracking = resource_tracking[, lapply(.SD, sum), by=year, .SDcols =! 'date']
setnames(resource_tracking, 'year', 'date')
resource_tracking = resource_tracking[, .(date, gf_mdrtb)]

#Run data transformations on this data - backcast if needed, cumulatively sum.  
resource_tracking[, gf_mdrtb_cumulative:=cumsum(gf_mdrtb)] #No NAs here, so don't need to worry about that. 

#read in already prepped data. - this will be 'data' for the regressions below. 
load(outputFile4a)
# data = data[, .(department, date, Number_of_Cases_Screened_for_MDR_act_cumulative, Secondline_Distributed_act_cumulative, 
#                 MDR_Cases_Started_Treatment_out_cumulative, gf_mdrtb_cumulative, Cases_Notified_out_cumulative, MDR_Cases_Started_Treatment_out)]

#Merge national level resource tracking with activity/output variables only. 
data2 = data[, .(Number_of_Cases_Screened_for_MDR_act_cumulative=sum(Number_of_Cases_Screened_for_MDR_act_cumulative, na.rm=TRUE), 
                 Secondline_Distributed_act_cumulative=sum(Secondline_Distributed_act_cumulative, na.rm=TRUE), 
                 MDR_Cases_Started_Treatment_out_cumulative=sum(MDR_Cases_Started_Treatment_out_cumulative, na.rm=TRUE), 
                 Cases_Notified_out_cumulative=sum(Cases_Notified_out_cumulative, na.rm=TRUE)), by='date']
data2 = merge(data2, resource_tracking, by='date', all=T)

#----------------------------------------------------------------------------------------------
#Using data prepared for GLM model, where inputs have been redistributed to subnational level. 
#---------------------------------------------------------------------------------------------
lm1 = lm(Number_of_Cases_Screened_for_MDR_act_cumulative ~ gf_mdrtb_cumulative + department + date, data=data)
lm2 = lm(Secondline_Distributed_act_cumulative ~ gf_mdrtb_cumulative + department + date, data=data)
lm3 = lm(MDR_Cases_Started_Treatment_out_cumulative ~ gf_mdrtb_cumulative + department + date, data=data)

# Try again without department and date - we know these are creating high collinearity. 
lm4 = lm(Number_of_Cases_Screened_for_MDR_act_cumulative ~ gf_mdrtb_cumulative, data=data)
lm5 = lm(Secondline_Distributed_act_cumulative ~ gf_mdrtb_cumulative, data=data)
lm6 = lm(MDR_Cases_Started_Treatment_out_cumulative ~ gf_mdrtb_cumulative, data=data)

# Using national-level resource tracking. 
lm7 = lm(Number_of_Cases_Screened_for_MDR_act_cumulative ~ gf_mdrtb_cumulative + date, data=data2)
lm8 = lm(Secondline_Distributed_act_cumulative ~ gf_mdrtb_cumulative + date, data=data2)
lm9 = lm(MDR_Cases_Started_Treatment_out_cumulative ~ gf_mdrtb_cumulative + date, data=data2)


#--------------------------------------------------
# Make some graphs. 
#--------------------------------------------------
# David used geom_smooth(). 

# Create ratios of GF $ spent on MDR per activity/output. 
data2[, genexpert_ratio:=(gf_mdrtb_cumulative/Number_of_Cases_Screened_for_MDR_act_cumulative), by='date']
data2[, secondline_ratio:=(gf_mdrtb_cumulative/Secondline_Distributed_act_cumulative), by='date']
data2[, mdr_treatment_ratio:=(gf_mdrtb_cumulative/MDR_Cases_Started_Treatment_out_cumulative), by='date']

p1 = ggplot(data2, aes(x=date, y=genexpert_ratio)) + 
  geom_point(color="red") + 
  geom_smooth(color="red") + 
  theme_bw() + 
  labs(title="Cost to Global Fund of each Genexpert test over time", x="Date", y="Cost per unit ($)", 
       caption="*There was a gap in grant spending in 2016")

p2 = ggplot(data2, aes(x=date, y=mdr_treatment_ratio)) + 
  geom_point(color="blue") + 
  geom_smooth(color="blue") + 
  theme_bw(base_size=18) + 
  scale_x_continuous(breaks=seq(2009, 2018, by=2), labels=as.character(seq(2009, 2018, by=2)), limits=c(2009, 2018)) + 
  labs(title="Cost to Global Fund of each MDR case started on treatment over time", x="Date", y="Cost per unit ($)", 
       caption="*There was a gap in grant spending in 2016")

p3 = ggplot(data2, aes(x=date, y=secondline_ratio)) + 
  geom_point(color="purple") + 
  geom_smooth(color="purple") + 
  theme_bw() + 
  labs(title="Cost to Global Fund of each unit of second-line drugs over time", x="Date", 
       y="Cost per unit ($)", caption="*There was a gap in grant spending in 2016")

p4 = ggplot(data2, aes(x=date, y=MDR_Cases_Started_Treatment_out)) + 
  geom_point(color="red") + 
  geom_line(color="red") + 
  theme_bw() + 
  labs(title="Number of MDR Cases Started Treatment", x="Date", y="# Cases")

p5 = grid.arrange(p4, p2, ncol=1)
#File paths 
save_loc = "J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/september_terg_presentation/"
ggsave(paste0(save_loc, "genexpert_efficiency.png"), p1, height=10, width=10)
ggsave(paste0(save_loc, "mdr_treatment_efficiency.png"), p2, height=10, width=18)
ggsave(paste0(save_loc, "secondline_drug_efficiency.png"), p3, height=10, width=10)
ggsave(paste0(save_loc, "mdr_started_treatment_grid.png"), p5, height=10, width=10)
