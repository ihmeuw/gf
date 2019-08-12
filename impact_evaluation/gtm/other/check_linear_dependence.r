#------------------------------------------------
# AUTHOR: Emily Linebarger
# PURPOSE: Check for 0 vectors in matrix of model variables, 
# which create linear dependence in the model. 
# DATE: August 5, 2019 
#------------------------------------------------

#---------------------------------
# MODEL FIRST HALF 
#---------------------------------
source('./impact_evaluation/gtm/set_up_r.r')
options(scipen=10)

# ---------------------------
# Load data
set.seed(1)
load(outputFile4a)
data1 = copy(data)

# ----------------------------------------------
# Define model object
source(paste0('./impact_evaluation/gtm/models/gtm_tb_first_half2.R'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
modelVars = c('department','date',modelVars)
data1 = data1[, unique(modelVars), with=FALSE] 

# jitter to avoid perfect collinearity #Commenting this out for the moment, EL 8/6/2019
for(v in names(subData)[!names(subData)%in%c('department','date')]) {
  if (all(subData[[v]]>=0)) subData[, (v):=get(v)+rexp(nrow(subData), (sd(subData[[v]])+2))] # Changed from poisson to exponential distribution to handle low-variance (high # of zeros) in many variables DP & EL 7/29/2019
  if (!all(subData[[v]]>=0)) subData[, (v):=get(v)+rnorm(nrow(subData), 0, (sd(subData[[v]])+2)/10)]
}

#Check for zero-vectors, which cause linear dependence. 
all_zero_vectors = data.table(variable=character(), count=integer(), department=integer())
names = names(data1)[!names(data1)%in%c('department', 'date')]
for (d in unique(data1$department)){
  subData = data1[department==d]
  subData = subData[, lapply(.SD, sum), .SDcols=names]
  # subData = subData[, lapply(.SD, round), .SDcols=names] #Add in a rounding element and see if it makes a difference. 
  zero_vectors = character()
  for (i in 1:ncol(subData)){
    if (subData[[i]]==0){
      zero_vectors = c(zero_vectors, names(subData)[i])
    }
  }
  if (length(zero_vectors)!=0){
    print(paste0("There are zero vectors for department ", d, ":"))
    print(zero_vectors)
    print("...")
    zero_vectors_dt = data.table(variable=zero_vectors, count=1, department=d)
    all_zero_vectors = rbind(all_zero_vectors, zero_vectors_dt)
  }
}

all_zero_vectors = unique(all_zero_vectors) 
print("These are all of the variables that ever appear as a zero-vector, with both jittering and rounding applied:")
print(all_zero_vectors)
print(length(all_zero_vectors))

#---------------------------------
# MODEL SECOND HALF
#---------------------------------
source('./impact_evaluation/gtm/set_up_r.r')
options(scipen=10)

# ---------------------------
# Load data
set.seed(1)
load(outputFile4b)
data2 = copy(data)

# ----------------------------------------------
# Define model object
source(paste0('./impact_evaluation/gtm/models/gtm_tb_sec_half2.R'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
modelVars = unique(c(parsedModel$lhs, parsedModel$rhs))
modelVars = c('department','date',modelVars)
data2 = data2[, unique(modelVars), with=FALSE] 

#Jitter to avoid perfect collinearity
for(v in names(data2)[!names(data2)%in%c('department','date')]) { 
  if (all(dt5[[v]]>=0)) dt5[, paste0((v), "_jitter"):=get(v)+runif(nrow(dt5), min=0, max=1/10)] # Changed from poisson to exponential distribution to handle low-variance (high # of zeros) in many variables DP & EL 7/29/2019
  if (!all(data2[[v]]>=0)) data2[, (v):=get(v)+rnorm(nrow(data2), 0, (sd(data2[[v]])+2)/10)]
}

#Check for zero-vectors, which cause linear dependence. 
all_zero_vectors = character() 
names = names(data2)[!names(data2)%in%c('department', 'date')]
for (d in unique(data2$department)){
  subData = data2[department==d]
  subData = subData[, lapply(.SD, sum), .SDcols=names]
  subData = subData[, lapply(.SD, round), .SDcols=names] #Add in a rounding element and see if it makes a difference. 
  zero_vectors = character()
  for (i in 1:ncol(subData)){
    if (subData[[i]]==0){
      zero_vectors = c(zero_vectors, names(subData)[i])
    }
  }
  if (length(zero_vectors)!=0){
    print(paste0("There are zero vectors for department ", d, ":"))
    print(zero_vectors)
    print("...")
    all_zero_vectors = c(all_zero_vectors, zero_vectors)
  }
}

all_zero_vectors = unique(all_zero_vectors) 
print("These are all of the variables that ever appear as a zero-vector, with both jittering and rounding applied:")
print(all_zero_vectors)


