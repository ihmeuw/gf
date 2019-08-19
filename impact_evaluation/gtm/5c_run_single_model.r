# ------------------------------------------------
# David Phillips
# 
# 3/18/2019
# This runs the second-half SEM dose response model for one health zone
# Intended to be run in parallel by 5d
# The current working directory should be the root of this repo
# Command-Line Arguments:
# modelVersion - (character) name of the model script to loaded (not including the file extension)
# modelStage - (numeric) 1 for first half of results chain, 2 for second half (controls input and output file names, must make sense given modelVersion)
# testRun - (logical) TRUE will run the model with limited MCMC steps, FALSE will run the full thing
# ------------------------------------------------
print(commandArgs())
source('./impact_evaluation/gtm/set_up_r.r')

# for testing purposes
task_id = 16
modelStage = 1
testRun = TRUE
modelVersion = "gtm_tb_first_half7"

# ----------------------------------------------
# Store task ID and other args from command line
if (!'task_id' %in% ls()) task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))

# store non-system command arguments
if (!'args' %in% ls()) args = commandArgs()
print(paste('Command Args:', args))
print(paste('Task ID:', task_id))
if(length(args)==0) stop('No commandArgs found!') 

#Pass arguments to the cluster 

# the first argument should be the model version to use
modelVersion = args[7]
#
# # the second argument should be the "model stage" (1 or 2)
modelStage = as.numeric(args[8])
#
# # the third argument should be whether to run a test run (TRUE) or full run (FALSE)
testRun = as.logical(args[9])


# print for log
print(paste('Model Version:', modelVersion))
print(paste('Model Stage:', modelStage))
print(paste('Test Run:', testRun))
# ----------------------------------------------


# ---------------------------------------------------------------------------------------------------
# Load data
set.seed(1)
if (Sys.info()[1]!='Windows' & modelStage==1) load(outputFile4a)
if (Sys.info()[1]=='Windows' & modelStage==1) load(outputFile4a)
if (Sys.info()[1]!='Windows' & modelStage==2) load(outputFile4b_scratch)
if (Sys.info()[1]=='Windows' & modelStage==2) load(outputFile4b)

# subset to current health zone
d = unique(data$department)[task_id]

subData = data[department==d]

# define model object
source(paste0('./impact_evaluation/gtm/models/', modelVersion, '.r'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
#modelVars = c('department','date',modelVars)
modelVars = c('department', modelVars)
subData = subData[, unique(modelVars), with=FALSE]

#Check unique values in data - do any columns have <5 unique values? 
# check_explan_power = data.table(var=names(subData))
# check_explan_power = check_explan_power[!var%in%c('department', 'date')]
# for (v in check_explan_power$var){
#   print(v)
#   length = length(unique(subData[[v]]))
#   print(length)
#   check_explan_power[var==v, unique_values:=length]
# }
# less_than_5 = unique(check_explan_power[unique_values<=5, .(var)])
# 
# if (length(less_than_5)>0){
#   warning("There are some variables with 5 or less data points in this department.")
#   warning(print(less_than_5))
# }
# 
# #Check for variables that are entirely zero
# print("Checking for variables that are completely zero...")
# vars = names(subData)[!names(subData)%in%c('department', 'date')]
# for (v in vars) if (sum(subData[, get(v)])==0) print(paste0("This variable is zero for all years: ", v))

#jitter to avoid perfect collinearity #Commenting this out for the moment, EL 8/6/2019
# for(v in names(subData)[!names(subData)%in%c('department','date')]) {
#  # if (all(subData[[v]]>=0)) subData[, (v):=get(v)+rexp(nrow(subData), (sd(subData[[v]])+2))] # Changed from poisson to exponential distribution to handle low-variance (high # of zeros) in many variables DP & EL 7/29/2019
#   if (all(subData[[v]]>=0)){
#     print(paste0(v, " is falling into the first jitter category, all(subData[[v]]>=0"))
#     subData[, paste0((v), "_jitter"):=get(v)+rexp(nrow(subData), (sd(subData[[v]])+2))] # Changed back to poisson after model was restructured EL 8/14/19 
#   } 
#   if (!all(subData[[v]]>=0)){
#     print(paste0(v, " is falling into the first jitter category, all(subData[[v]]>=0"))
#     subData[, paste0((v), "_jitter"):=get(v)+rnorm(nrow(subData), 0, (sd(subData[[v]])+2)/10)]
#   }
# }
# 

#Only jitter values that have zeros for the entire time series- this is the only thing that's breaking the model at this point. 
# Replace all-zero-vectors with a zero-variance positive vector (0.1). 
# for(v in names(subData)[!names(subData)%in%c('department','date')]){
#   values = as.double(unique(subData[[v]])) #Get a vector of the unique values of the variable. 
#   zero_compare = rep(0, length(values)) #Get an equal length vector of zeros. 
#   if (all(values==zero_compare)){
#     print(paste0(v, " is completely zero for the entire time series - applying small jitter between 0 and 1/100"))
#     subData[, (v):=runif(nrow(subData), min=0, max=0.01)]
#   }
# }
#   
# print(subData[, .(Number_of_Cases_Screened_for_MDR_act_cumulative, TB_Patients_Tested_for_HIV_act_cumulative)])
# for(v in names(subData)[!names(subData)%in%c('department','date')]) {
#   sd = sd(subData[[v]])
#   if (all(subData[[v]]>=0) & sd!=0){
#     subData[, (v):=get(v)+rexp(nrow(subData), (sd(subData[[v]])/100))] # Changed back to poisson after model was restructured EL 8/14/19
#   } else {
#     subData[, (v):=get(v)+rexp(nrow(subData))]
#   }
# }
# print(subData[, .(Number_of_Cases_Screened_for_MDR_act_cumulative, TB_Patients_Tested_for_HIV_act_cumulative)])

# Original jitter - adding a random exponential 
#Jitter analysis 1 - just plain jitter. (v:=jitter(get(v))
# Jitter analysis 2 - jittering from a uniform distribution, around the standard deviation of v. subData[, (v):=jitter(get(v), amount=sd(get(v)))]
#Jitter analysis 3 - jittering from poisson, with lambda = sd of variable subData[, (v):=get(v) + rpois(length(get(v)), lambda=sd(get(v)))]
# Jitter analysis 4 - jittering from poisson, with lambda = 2sd of variable subData[, (v):=get(v) + rpois(length(get(v)), lambda=2*sd(get(v)))]
# Jitter analyis 5 - poisson distribution, with lambda = number of years of data subData[, (v):=get(v) + rpois(length(get(v)), lambda=length(get(v)))]

# rescale variables to have similar variance
# see Kline Principles and Practice of SEM (2011) page 67
# scaling_factors = data.table()
# numVars = names(subData)[!names(subData)%in%c('department')]
# for(v in numVars) {
#   print(v)
# 	s=1
# 	if (all(subData[[v]]!=0)){ #Changed from 0 to 0.1 to test new jitter scheme EL 8/19/19
#   	if (var(subData[[v]]/s)>1000){
#   	  while(var(subData[[v]]/s)>1000) s=s*10
#   	} else {
#   	  while(var(subData[[v]]/s)<100) s=s/10
#   	}
# 	}
# 	scaling_factors[,(v):=s]
# }
# for(v in names(scaling_factors)) subData[, (v):=get(v)/scaling_factors[[v]]]
# print(subData[, .(Number_of_Cases_Screened_for_MDR_act_cumulative, TB_Patients_Tested_for_HIV_act_cumulative)])
# 

# If running on Windows, optional check for correlation coefficients at this point. 
# Look for correlation coefficients higher than .98. 
# if (Sys.info()[1]=='Windows'){
#   library(GGally)
#   #Generate variable groupings
#   jitter_vars = names(subData)[grep("jitter", names(subData))]
#   pre_jitter_vars = names(subData)[!names(subData)%in%jitter_vars]
#   pre_jitter_vars = pre_jitter_vars[!pre_jitter_vars%in%c('department', 'date')]
#   
#   jitter_fin_vars = jitter_vars[grep("odah|ghe|gf", jitter_vars)]
#   jitter_act_vars = jitter_vars[!jitter_vars%in%jitter_fin_vars]
#   
#   pre_jitter_fin_vars = pre_jitter_vars[grep("odah|ghe|gf", pre_jitter_vars)]
#   pre_jitter_act_vars = pre_jitter_vars[!pre_jitter_vars%in%pre_jitter_fin_vars]
#   
#   long = melt(subData, id.vars=c('department', 'date'))
#   
#   #Make graphs 
#   pre_jitter_plot = ggpairs(subData[, c(pre_jitter_vars), with=F], title = paste0("Pre-jitter correlations of all variables in the model for department ", d))
#   jitter_plot = ggpairs(subData[, c(jitter_vars), with=F], title = paste0("Jitter correlations of all variables in the model for department ", d))
#   
#   j_fin_plot =  ggplot(long[variable%in%jitter_fin_vars], aes(y=value, x=date)) + 
#     geom_line() + 
#     facet_wrap(~variable, scales='free') + 
#     labs(title=paste('Time series of jittered financial variables for department ', d), y='Value', x='Date') + 
#     theme_bw()
#   j_act_plot =  ggplot(long[variable%in%jitter_act_vars], aes(y=value, x=date)) + 
#     geom_line() + 
#     facet_wrap(~variable, scales='free') + 
#     labs(title=paste('Time series of jittered activity/output variables for department ', d), y='Value', x='Date') + 
#     theme_bw()
#   
#   pre_j_fin_plot =  ggplot(long[variable%in%pre_jitter_fin_vars], aes(y=value, x=date)) + 
#     geom_line() + 
#     facet_wrap(~variable, scales='free') + 
#     labs(title=paste('Time series of pre-jittered financial variables for department ', d), y='Value', x='Date') + 
#     theme_bw()
#   pre_j_act_plot =  ggplot(long[variable%in%pre_jitter_act_vars], aes(y=value, x=date)) + 
#     geom_line() + 
#     facet_wrap(~variable, scales='free') + 
#     labs(title=paste('Time series of pre-jittered activity/output variables for department ', d), y='Value', x='Date') + 
#     theme_bw()
#   
#   pdf("J:/Project/Evaluation/GF/impact_evaluation/gtm/visualizations/jitter_analysis.pdf", height=5.5, width=9)
#   
#   print(pre_jitter_plot) 
#   print(jitter_plot)
#   
#   print(pre_j_fin_plot) 
#   print(j_fin_plot) 
#   
#   print(pre_j_act_plot) 
#   print(j_act_plot)
#   
#   dev.off() 
#   
# }
# ----------------------------------------------------------------
# Run model

# fit model
# if (testRun==TRUE) semFit = bsem(model, subData, adapt=50, burnin=10, sample=10, bcontrol=list(thin=3))
# if (testRun==FALSE) semFit = bsem(model, subData, adapt=5000, burnin=10000, sample=1000, bcontrol=list(thin=3))

#Make scaling factors data table so you can run this code. - everything should be scaled to 1. 
scaling_factors = data.table()
numVars = names(subData)[!names(subData)%in%c('department')]
for(v in numVars) {
	scaling_factors[,(v):=1]
}

# run series of unrelated linear models for comparison
urFit = lavaanUR(model, subData)
# ----------------------------------------------------------------
if ('urFit'%in%ls()){
  print("The GLM model ran successfully.")
}

# --------------------------------------------------------------
# Store coefficient table from model

# get standardized solution
# standardizedSummary = data.table(standardizedSolution(semFit, se=TRUE))
# setnames(standardizedSummary, 'se', 'se.std')
# standardizedSummary = standardizedSummary[, -c('ci.lower', 'ci.upper')]
# 
# # get unstandardized parameter values
# summary = data.table(parTable(semFit))
# summary = summary[, c('lhs','op','rhs','est','se'), with=FALSE]
# 
# # unrescale SEM coefficients to reflect actual units of x and y variables
tmp = unique(melt(scaling_factors, value.name='scaling_factor'))
# summary = merge(summary, tmp, by.x='rhs', by.y='variable', all.x=TRUE)
# summary = merge(summary, tmp, by.x='lhs', by.y='variable', all.x=TRUE, suffixes=c('.rhs','.lhs'))
# summary[is.na(scaling_factor.rhs), scaling_factor.rhs:=1]
# summary[is.na(scaling_factor.lhs), scaling_factor.lhs:=1]
# summary[, est_raw:=est] # just to be able to see the rescaled value
# summary[, est:=est/(scaling_factor.rhs/scaling_factor.lhs)]
# summary[, se:=se/(scaling_factor.rhs/scaling_factor.lhs)]

# unrescale UR coefficients to reflect actual units of x and y variables
urFit = merge(urFit, tmp, by.x='rhs', by.y='variable', all.x=TRUE)
urFit = merge(urFit, tmp, by.x='lhs', by.y='variable', all.x=TRUE, suffixes=c('.rhs','.lhs'))
urFit[is.na(scaling_factor.rhs), scaling_factor.rhs:=1]
urFit[is.na(scaling_factor.lhs), scaling_factor.lhs:=1]
urFit[, est_raw:=est] # just to be able to see the rescaled value
urFit[, est:=est/(scaling_factor.rhs/scaling_factor.lhs)]
urFit[, se:=se/(scaling_factor.rhs/scaling_factor.lhs)]

# add standardized coefficients to summary
# summary = merge(summary, standardizedSummary, by=c('lhs','op','rhs'))

# label health zone
urFit[, department:=d]
# summary[, department:=d]
# --------------------------------------------------------------


# ------------------------------------------------------------------
# Save model output and clean up

# reassign the temporary output location if the parent script is set to runInParallel FALSE
if ('runInParallel' %in% ls()) if (runInParallel==FALSE){clustertmpDir2 = tempIeDir}

# make unique file name
if(modelStage==1) outputFile5tmp1 = paste0(clustertmpDir2, 'first_half_semFit_', task_id, '.rds')
if(modelStage==1) outputFile5tmp2 = paste0(clustertmpDir2, 'first_half_summary_', task_id, '.rds')
if(modelStage==1) outputFile5tmp3 = paste0(clustertmpDir2, 'first_half_urFit_', task_id, '.rds')
if(modelStage==2) outputFile5tmp1 = paste0(clustertmpDir2, 'second_half_semFit_', task_id, '.rds')
if(modelStage==2) outputFile5tmp2 = paste0(clustertmpDir2, 'second_half_summary_', task_id, '.rds')
if(modelStage==2) outputFile5tmp3 = paste0(clustertmpDir2, 'second_half_urFit_', task_id, '.rds')

# save
# print(paste('Saving:', outputFile5tmp1))
# saveRDS(semFit, file=outputFile5tmp1)
# 
# print(paste('Saving:', outputFile5tmp2))
# saveRDS(summary, file=outputFile5tmp2)

print(paste('Saving:', outputFile5tmp3))
saveRDS(urFit, file=outputFile5tmp3)
# ------------------------------------------------------------------
