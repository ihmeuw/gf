# Generate 0 relationships between LHS and RHS model vars 

#---------------------------------------------
# FIRST HALF 
#--------------------------------------------
source(paste0('./impact_evaluation/gtm/models/gtm_tb_first_half2.R'))

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

all_labels = rbind(all_lhs, all_rhs)

#Remove duplicate sets
vars = data.table(var1= unique(all_labels$var1))
vars[, var2:=var1]
vars[, id1:=seq(1, 28, by=1)]
vars[, id2:=seq(1, 28, by=1)]

all_labels = merge(all_labels, vars[, .(var1, id1)], by='var1')
all_labels = merge(all_labels, vars[, .(var2, id2)], by='var2')

dt1 = all_labels[, .(var1, var2, id1)]
dt2 = all_labels[, .(var1, var2, id2)]
setnames(dt2, c('var1', 'var2', 'id2'), c('var2', 'var1', 'id1'))

check_dups = merge(dt1, dt2, by=c('var1', 'var2', 'id1'))
all_labels = all_labels[order(var1)]
all_labels[, id1:=seq(1, nrow(all_labels), by=1), by='var1']
all_labels = all_labels[order(var2)]
all_labels[, id2:=seq(1, nrow(all_labels), by=1), by='var2']

#Pull out just the vector of labels to paste into the model object. 
all_labels = c(all_lhs$label, all_rhs$label)
all_labels = unique(all_labels) 

#There are certain input variable relationships we want to keep - drop these 0 lables. 
to_keep = c("exp_T1_2_cumulative ~~ 0*ghe_T1_2_cumulative", 
"exp_T1_2_cumulative ~~ 0*other_dah_T1_2_cumulative", 
"ghe_T1_2_cumulative ~~ 0*other_dah_T1_2_cumulative", 

"other_dah_T1_1_cumulative ~~ 0*exp_T1_1_cumulative", 
"other_dah_T1_1_cumulative ~~ 0*ghe_T1_1_cumulative", 
"exp_T1_1_cumulative ~~ 0*ghe_T1_1_cumulative", 

"other_dah_T3_1_cumulative ~~ 0*exp_T3_ALL_cumulative", 
"other_dah_T3_2_cumulative ~~ 0*exp_T3_ALL_cumulative", 
"other_dah_T3_ALL_cumulative ~~ 0*exp_T3_ALL_cumulative", 
"other_dah_TB_ALL_cumulative ~~ 0*exp_TB_ALL_cumulative", 
"other_dah_HIV_TB_ALL_cumulative ~~ 0*exp_HIV_TB_ALL_cumulative")

length(all_labels)
all_labels = all_labels[!all_labels%in%to_keep]
length(all_labels)


write.csv(all_labels, "C:/Users/elineb/Desktop/zero_covariances_1.csv", row.names=F)


#--------------------------------------------
# SECOND HALF 
#--------------------------------------------
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
