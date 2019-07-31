# Generate 0 relationships between LHS and RHS model vars 

#--------------------------------------------
# SECOND HALF 
#--------------------------------------------
load(outputFile4b)

source(paste0('./impact_evaluation/gtm/models/gtm_tb_sec_half2.R'))

# reduce the data down to only necessary variables
parsedModel = lavParseModelString(model)
lhsVars = unique(parsedModel$lhs)
lhsVars = lhsVars[!lhsVars%in%c('date')]
rhsVars = unique(parsedModel$rhs)
rhsVars = rhsVars[!rhsVars%in%c('date')]

#Map each LHS variable with every other LHS variable in a relationship like this: 
# budget_M2_3_cumulative ~~ 0*budget_M3_1_cumulative
#If there are n vars, repeat the first var n-1 times, and start by matching it to i-1. 
col1 = character() 
for (i in 1:length(lhsVars)){
  col1 = c(col1, rep(lhsVars[i], length(lhsVars)-i))
}

#Then, set up a second vector with the matching names. 
col2 = character()
for (i in 1:(length(lhsVars)-1)){
  col2 = c(col2, lhsVars[(i+1):(length(lhsVars))])
}

stopifnot(length(col1)==length(col2))
all_lhs = data.table(var1=col1, var2=col2)
all_lhs[, label:=paste0(var1, " ~~ 0*", var2)]

#Map each rhs variable with every other rhs variable in a relationship like this: 
# budget_M2_3_cumulative ~~ 0*budget_M3_1_cumulative
col1 = character() 
for (i in 1:length(rhsVars)){
  col1 = c(col1, rep(rhsVars[i], length(rhsVars)-i))
}

#Then, set up a second vector with the matching names. 
col2 = character()
for (i in 1:(length(rhsVars)-1)){
  col2 = c(col2, rhsVars[(i+1):(length(rhsVars))])
}

stopifnot(length(col1)==length(col2))
all_rhs = data.table(var1=col1, var2=col2)
all_rhs[, label:=paste0(var1, " ~~ 0*", var2)]

#Pull out just the vector of labels to paste into the model object. 
all_labels = c(all_lhs$label, all_rhs$label)
write.csv(all_labels, "C:/Users/elineb/Desktop/zero_covariances_2.csv", row.names=F)


#----------------------------------------
#EXAMPLE TO DEBUG 
vars = c('first', 'second', 'third', 'fourth', 'fifth')
#If there are n vars, repeat the first var n-1 times, and start by matching it to i-1. 
col1 = character() 
for (i in 1:length(vars)){
  col1 = c(col1, rep(vars[i], length(vars)-i))
}

#Then, set up a second vector with the matching names. 
col2 = character()
for (i in 1:(length(vars)-1)){
  col2 = c(col2, vars[(i+1):(length(vars))])
}

stopifnot(length(col1)==length(col2))
dt = data.table(var1=col1, var2=col2)
