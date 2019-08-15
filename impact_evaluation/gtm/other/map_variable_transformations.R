#------------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Map variable transformations. 
# DATE: August 5, 2019 
#------------------------------------------------

rm(list=ls())
library(data.table) 
source('./impact_evaluation/gtm/set_up_r.r')
options(scipen=10)

set.seed(1)

#-----------------------------------------
# DT 0 - raw data 
#-----------------------------------------
activities = fread(actFile)
outputs = fread(outputsFile)

#Add _ to names of data. 
names(activities) = gsub(" ", "_", names(activities))
names(outputs) = gsub(" ", "_", names(outputs))
names(activities) = gsub("/", "_", names(activities))
names(outputs) = gsub("/", "_", names(outputs))

names(activities) = gsub("value_m|value_d", "act", names(activities))
names(outputs) = gsub("value_m|value_d", "out", names(outputs))


#------------------------------------------
# DT 1 - initial cleaned data
#------------------------------------------
dt1a = readRDS(outputFile2a)
dt1b = readRDS(outputFile2b)
names(dt1b) = gsub("_value", "", names(dt1b))

#-------------------------------------------
# DT 2 - merged data, with inputs data redistributed
#-------------------------------------------
dt2 = readRDS(outputFile3)

#-------------------------------------------
# DT 3 - GLM imputation
#-------------------------------------------
dt3 = copy(dt2)

# drop zero-variance variables
numVars = names(dt3)[!names(dt3)%in%c('department','date')]
for(v in numVars) if (all(is.na(dt3[[v]]))) dt3[[v]] = NULL

#EMILY - WE WANT TO ONLY IMPUTE VARIABLES THAT ARE COUNTS. 
# extrapolate where necessary using GLM (better would be to use multiple imputation)
i=1
for(v in numVars) {
  for(h in unique(dt3$department)) { 
    i=i+1
    if (!any(is.na(dt3[department==h][[v]]))) next
    if (!any(!is.na(dt3[department==h][[v]]))) next
    form = as.formula(paste0(v,'~date'))
    lmFit = glm(form, dt3[department==h], family='poisson')
    dt3[department==h, tmp:=exp(predict(lmFit, newdt3=dt3[department==h]))]
    lim = max(dt3[department==h][[v]], na.rm=T)+sd(dt3[department==h][[v]], na.rm=T)
    dt3[department==h & tmp>lim, tmp:=lim]
    dt3[department==h & is.na(get(v)), (v):=tmp]
    pct_complete = floor(i/(length(numVars)*length(unique(dt3$department)))*100)
    cat(paste0('\r', pct_complete, '% Complete'))
    flush.console() 
  }
}
dt3$tmp = NULL

#----------------------------------------------
# DT 4 - cumulative sums 
#----------------------------------------------
dt4 = copy(dt3) 

# # make cumulative variables
cumulVars = names(dt4)
cumulVars = cumulVars[!grepl("total", cumulVars)]
cumulVars = cumulVars[!cumulVars%in%c('department', 'date', 'year', 'min')]
for(v in cumulVars) {
  nv = gsub('value_','',v)
  dt4[, (nv):=cumsum(get(v)), by='department']
}

#----------------------------------------------
# DT 5 - jittering
#----------------------------------------------
dt5 = copy(dt4)

# define model object
source(paste0('./impact_evaluation/gtm/models/gtm_tb_first_half2.R'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
modelVars = gsub("_cumulative", "", modelVars) #Remove "cumulative" so all names will be the same through transformations
modelVars = c('department','date',modelVars)
dt5 = dt5[, unique(modelVars), with=FALSE]

# jitter to avoid perfect collinearity
for(v in names(dt5)[!names(dt5)%in%c('department','date')]) { 
  # if (all(dt5[[v]]>=0)) dt5[, paste0((v), "_jitter"):=get(v)+rexp(nrow(dt5), (sd(dt5[[v]])+2))] # Changed from poisson to exponential distribution to handle low-variance (high # of zeros) in many variables DP & EL 7/29/2019
  if (all(dt5[[v]]>=0)) dt5[, paste0((v), "_jitter"):=get(v)+runif(nrow(dt5), min=0, max=1/10)] # Changed from poisson to exponential distribution to handle low-variance (high # of zeros) in many variables DP & EL 7/29/2019
  if (!all(dt5[[v]]>=0)) dt5[, paste0((v), "_jitter"):=get(v)+rnorm(nrow(dt5), 0, (sd(dt5[[v]])+2)/10)]
}

#----------------------------------------------
# DT 6 - rescaling
#----------------------------------------------
dt6 = copy(dt5)

scaling_factors = data.table(date=1)
numVars = names(dt6)[!names(dt6)%in%c('department','date')]
for(v in numVars) {
  s=1
  while(var(dt6[[v]]/s)>1000) s=s*10
  while(var(dt6[[v]]/s)<100) s=s/10
  scaling_factors[,(v):=s]
}
for(v in names(scaling_factors)) dt6[, (v):=get(v)/scaling_factors[[v]]]



# -----------------------------------------------------------
# MAKE SOME PLOTS AND SEE HOW THINGS ARE DIFFERENT
# -----------------------------------------------------------
output_dir = "J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/"
pdf(paste0(output_dir, 'variable_modifications_with_rescaling.pdf'), height=6, width=12)
vars = modelVars[!modelVars%in%c('department', 'date')]
for (v in vars){
  for (d in 1:22){ #The departments are labeled 1:22. 
    if (v%in%names(activities)){
      p = ggplot(activities[department==d], aes(y=get(v), x=date)) + 
        geom_point(color="darkslategray1") + geom_line(color="darkslategray1") + 
        geom_point(data=dt1b[department==d]) + geom_line(data=dt1b[department==d])
    } else if (v%in%names(outputs)){
      p = ggplot(outputs[department==d], aes(y=get(v), x=date)) + 
        geom_point(color="darkslategray1") + geom_line(color="darkslategray1") + 
        geom_point(data=dt1b[department==d], color="darkslategray4") + geom_line(data=dt1b[department==d], color="darkslategray4")
    } else {
      p = ggplot(dt1a, aes(y=get(v), x=date))+
        geom_point(color="darkslategray1") + geom_line(color="darkslategray1") 
    }
      plot = p +
      geom_point(data=dt2[department==d], color="deepskyblue") + geom_line(data=dt2[department==d], color="deepskyblue") +
      geom_point(data=dt3[department==d], color="dodgerblue4") + geom_line(data=dt3[department==d], color="dodgerblue4") + 
      geom_point(data=dt4[department==d], color="mediumorchid") + geom_line(data=dt4[department==d], color="mediumorchid") + 
      geom_point(data=dt5[department==d], color="mediumpurple") + geom_line(data=dt5[department==d], color="mediumpurple") + 
      geom_point(data=dt6[department==d], color="gray0") + geom_line(data=dt6[department==d], color="gray0") + 
      theme_bw() + 
      labs(title=paste0("Plot of variable modifications to '", v, "' for department ", d), x="Date (2009-2018 only)", y=v, caption="*Includes rescaled line in black")
    print(plot)
  } 
}
dev.off() 

output_dir = "J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/"
pdf(paste0(output_dir, 'variable_modifications.pdf'), height=6, width=12)
vars = modelVars[!modelVars%in%c('department', 'date')]
for (v in vars){
  for (d in 1:22){ #The departments are labeled 1:22. 
    if (v%in%names(activities)){
      p = ggplot(activities[department==d], aes(y=get(v), x=date)) + 
        geom_point(color="darkslategray1") + geom_line(color="darkslategray1") + 
        geom_point(data=dt1b[department==d]) + geom_line(data=dt1b[department==d])
    } else if (v%in%names(outputs)){
      p = ggplot(outputs[department==d], aes(y=get(v), x=date)) + 
        geom_point(color="darkslategray1") + geom_line(color="darkslategray1") + 
        geom_point(data=dt1b[department==d], color="darkslategray4") + geom_line(data=dt1b[department==d], color="darkslategray4")
    } else {
      p = ggplot(dt1a, aes(y=get(v), x=date))+
        geom_point(color="darkslategray1") + geom_line(color="darkslategray1") 
    }
    plot = p +
      geom_point(data=dt2[department==d], color="deepskyblue") + geom_line(data=dt2[department==d], color="deepskyblue") +
      geom_point(data=dt3[department==d], color="dodgerblue4") + geom_line(data=dt3[department==d], color="dodgerblue4") + 
      geom_point(data=dt4[department==d], color="mediumorchid") + geom_line(data=dt4[department==d], color="mediumorchid") + 
      geom_point(data=dt5[department==d], color="mediumpurple") + geom_line(data=dt5[department==d], color="mediumpurple") + 
      # geom_point(data=dt6[department==d], color="gray0") + geom_line(data=dt6[department==d], color="gray0") + 
      theme_bw() + 
      labs(title=paste0("Plot of variable modifications to '", v, "' for department ", d), x="Date (2009-2018 only)", y=v)
    print(plot)
  } 
}
dev.off() 

#-------------------------------------------
# Make a sample dataset to view 
#-------------------------------------------
view_data_transforms = function(variable, dep){
  if (variable%in%names(activities)){ #Just pull raw activities and outputs data from Guillermo. 
    dt0_sub = activities[department==dep, .(var0 = sum(get(variable))), by='date']
  } else if (variable%in%names(outputs)){
    dt0_sub = outputs[department==dep, .(var0 = sum(get(variable))), by='date']
  }
  if (variable%in%names(dt1a)){
    dt1_sub = dt1a[, .(date, var1_all_depts=get(variable))]
  } else {
    dt1_sub = dt1b[department==15, .(date, var1=get(variable))]
  }
  dt2_sub = dt2[department==dep, .(date, var2=get(variable))]
  dt3_sub = dt3[department==dep, .(date, var3=get(variable))]
  dt4_sub = dt4[department==dep, .(date, var4=get(variable))]
  dt5_sub = dt5[department==dep, .(date, var5=get(variable))]
  dt6_sub = dt6[department==dep, .(date, var6=get(variable))]
  
  if ('dt0_sub'%in%ls()){
    dt = merge(dt0_sub, dt1_sub, by=c('date'), all=T)
    dt = merge(dt, dt2_sub, by='date', all=T)
  } else { #For inputs variables, we won't have a dt0. 
    dt = merge(dt1_sub, dt2_sub, by=c('date'))
  }
  dt = merge(dt, dt3_sub, by=c('date'), all=T)
  dt = merge(dt, dt4_sub, by=c('date'), all=T)
  dt = merge(dt, dt5_sub, by=c('date'), all=T)
  dt = merge(dt, dt6_sub, by=c('date'), all=T)
  
  return(dt)
} 


view_data_transforms_all_depts = function(variable){
  if (variable%in%names(activities)){ #Just pull raw activities and outputs data from Guillermo. 
    dt0_sub = activities[, .(var0 = sum(get(variable))), by='date']
  } else if (variable%in%names(outputs)){
    dt0_sub = outputs[, .(var0 = sum(get(variable))), by='date']
  }
  if (variable%in%names(dt1a)){
    dt1_sub = dt1a[, .(var1_all_depts=sum(get(variable))), by='date']
  } else {
    dt1_sub = dt1b[, .(var1=sum(get(variable))), by='date']
  }
  dt2_sub = dt2[, .(var2=sum(get(variable))), by='date']
  dt3_sub = dt3[, .(var3=sum(get(variable))), by='date']
  dt4_sub = dt4[, .(var4=sum(get(variable))), by='date']
  dt5_sub = dt5[, .(var5=sum(get(variable))), by='date']
  dt6_sub = dt6[, .(var6=sum(get(variable))), by='date']
  
  if ('dt0_sub'%in%ls()){
    dt = merge(dt0_sub, dt1_sub, by=c('date'), all=T)
    dt = merge(dt, dt2_sub, by='date', all=T)
  } else { #For inputs variables, we won't have a dt0. 
    dt = merge(dt1_sub, dt2_sub, by=c('date'))
  }
  dt = merge(dt, dt3_sub, by=c('date'), all=T)
  dt = merge(dt, dt4_sub, by=c('date'), all=T)
  dt = merge(dt, dt5_sub, by=c('date'), all=T)
  dt = merge(dt, dt6_sub, by=c('date'), all=T)
  
  return(dt)
} 

#Pull some data tables to look at using these functions. 

#--------------------------------------
#EXAMPLE 1 
#--------------------------------------
dt1a[, .(date, exp_T2_ALL)] #This is quite robust expenditure data. 
dt1b[, .(var = sum(PLHIV_Screened_for_TB_act)), by=c('department')] #For department 4, because there were no PLHIV screened for TB there, can we assume that there was no money spent there?
dt1b[department==4, .(date, department, PLHIV_Screened_for_TB_act)]

#How does this look in the final transformation of exp_t2_all? 
exp_t2_all = view_data_transforms("exp_T2_ALL", 4)


#---------------------------------------
# EXAMPLE 2 
#---------------------------------------
dt5[department==15] #Review ghe_T1_1, exp_T1_6, and exp_T1_5. 


