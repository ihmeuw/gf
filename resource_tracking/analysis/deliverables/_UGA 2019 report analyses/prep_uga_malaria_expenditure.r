#---------------------------------------------------------
# Prepping data for Uganda malaria regressions 
# Emily Linebarger 
# October 10, 2019
#---------------------------------------------------------

#Load packages and set file paths.
library(data.table) 
library(ggplot2)

#Read in data 
options(scipen=20)
box_path = "C:/Users/elineb/Box Sync/Global Fund Files/" #Set to your Box location!
gf = readRDS(paste0(box_path, "tableau_data/final_expenditures_uga.rds"))
ghe = readRDS("J:/Project/Evaluation/GF/resource_tracking/_ghe/fgh_ghe_actuals_malaria/prepped_data/ghe_actuals_malaria.rds")
odah = readRDS("J:/Project/Evaluation/GF/resource_tracking/_odah/prepped_data/other_dah_actuals_all_uga.rds")

#----------------------------------------------------
# Data prep - subset and shape to the quarter-level. 
#-----------------------------------------------------
#Global fund - Only keep cases where grant disease is "malaria" 
gf_malaria = gf[substr(code, 1, 2)=="M2", .(gf_cm_expenditure=sum(expenditure, na.rm=T)), by=c('start_date', 'end_date')]
gf_malaria = gf_malaria[year(start_date)>=2004]
gf_malaria[is.na(end_date), end_date:=start_date+90] #There are some cases where start date is NA. 
stopifnot(unique(gf_malaria$end_date-gf_malaria$start_date)%in%c(89, 90, 91)) #Everything is at the quarter-level. 

gf_malaria[, quarter:=quarter(start_date)]
gf_malaria[, year:=year(start_date)]
gf_malaria = gf_malaria[, .(gf_cm_expenditure, quarter, year)]

#--------------------------------------------
#GHE - only keep NHAs. Can't subset to "case management" with the level of disaggregation we have in this data. 
ghe_malaria = ghe[loc_name=="uga" & source_type=="nha", .(ghe_expenditure=sum(value, na.rm=T)), by=c('year')] 
ghe_frame = expand.grid(year=unique(ghe_malaria$year), quarter=c(1, 2, 3, 4))

#Expand to quarter-level. 
nrow_ghe = nrow(ghe_malaria) 
ghe_malaria = merge(ghe_malaria, ghe_frame, by='year')
stopifnot(nrow(ghe_malaria)==nrow_ghe*4) #You should exactly expand this by 4.

# Add variables 
ghe_malaria[, ghe_expenditure:=ghe_expenditure/4]
ghe_malaria = ghe_malaria[, .(ghe_expenditure, quarter, year)]

#-----------------------------------------
# ODAH
odah_malaria = odah[activity_description%in%c('mal_treat_dah_18', 'mal_diag_dah_18'), .(odah_cm_expenditure=sum(disbursement, na.rm=T)), by=c('year')]
odah_frame = expand.grid(year=unique(odah_malaria$year), quarter=c(1, 2, 3, 4))

#Expand to quarter-level. 
nrow_odah = nrow(odah_malaria) 
odah_malaria = merge(odah_malaria, odah_frame, by='year')
stopifnot(nrow(odah_malaria)==nrow_odah*4) #You should exactly expand this by 4.

# Add variables 
odah_malaria[, odah_cm_expenditure:=odah_cm_expenditure/4]
odah_malaria = odah_malaria[year>=2004, .(odah_cm_expenditure, quarter, year)]


# Combine data 
dt = merge(gf_malaria, ghe_malaria, by=c('year', 'quarter'), all=T)
dt = merge(dt, odah_malaria, by=c('year', 'quarter'), all=T)

#-------------------------------------------------
# Linearly extrapolate variables 
#-------------------------------------------------
dt[, date:=year+((quarter/4)-0.25)]
untransformed = copy(dt) 

i=1
for(v in names(dt)[!names(dt)%in%c('year', 'quarter', 'date')]) {
  i=i+1
  #First, check whether all values for this department and this variable are zero. 
  # if they are, don't backcast. 
  values = unique(dt[, as.vector(get(v))]) #Get a vector of the unique values of the variable.
  values[is.na(values)] = 0
  
  #Backcast if it doesn't fall into this category. 
  if (!any(is.na(dt[[v]]))) next
  if (!any(!is.na(dt[[v]]))) next
  form = as.formula(paste0(v,'~date'))
  lmFit = glm(form, dt, family='poisson')
  dt[, tmp:=exp(predict(lmFit, newdata=dt))]
  lim = max(dt[[v]], na.rm=T)+sd(dt[[v]], na.rm=T)
  dt[tmp>lim, tmp:=lim]
  dt[is.na(get(v)), (v):=tmp]
}
dt$tmp = NULL


#----------------------
#Save data 
#----------------------
write.csv(dt, "J:/Project/Evaluation/GF/resource_tracking/_gf_files_gos/uga/prepped_data/uga_malaria_expenditure.csv", row.names=F)


#Look at each variable as a time trend. 

p1 = ggplot(untransformed, aes(x=date, y=gf_cm_expenditure)) + 
  geom_point(color="blue") + 
  geom_line(color="blue") + 
  theme_bw() +
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="GF Case Management malaria expenditure - untransformed")

p2 = ggplot(dt, aes(x=date, y=gf_cm_expenditure)) + 
  geom_point(color="red") + 
  geom_line(color="red") + 
  theme_bw() +
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="GF Case Management malaria expenditure - transformed")

p3 = ggplot(untransformed, aes(x=date, y=ghe_expenditure)) + 
  geom_point(color="blue") + 
  geom_line(color="blue") + 
  theme_bw() +
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="GHE malaria expenditure - untransformed")

p4 = ggplot(dt, aes(x=date, y=ghe_expenditure)) + 
  geom_point(color="red") + 
  geom_line(color="red") + 
  theme_bw() +
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="GHE malaria expenditure - transformed")

p5 = ggplot(untransformed, aes(x=date, y=odah_cm_expenditure)) + 
  geom_point(color="blue") + 
  geom_line(color="blue") + 
  theme_bw() +
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="ODAH Case Management malaria expenditure - untransformed")

p6 = ggplot(dt, aes(x=date, y=odah_cm_expenditure)) + 
  geom_point(color="red") + 
  geom_line(color="red") + 
  theme_bw() +
  scale_y_continuous(labels=scales::dollar) + 
  labs(title="ODAH Case Management expenditure - transformed")

pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/UGA Malaria September 2019/uga_mal_regression_exploratory.pdf")
p1
p2
p3
p4
p5
p6
dev.off() 
