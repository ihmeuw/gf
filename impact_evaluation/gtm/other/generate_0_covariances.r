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
all_lhs = data.table(expand.grid(var1=lhsVars, var2=lhsVars))
#Remove rows where a variable is matched to itself, and remove duplicates (there shouldn't be any?) 
all_lhs = all_lhs[var1!=var2]
all_lhs[, label:=paste0(var1, " ~~ 0*", var2)]

#Map each rhs variable with every other rhs variable in a relationship like this: 
# budget_M2_3_cumulative ~~ 0*budget_M3_1_cumulative
all_rhs = data.table(expand.grid(var1=rhsVars, var2=rhsVars))
#Remove rows where a variable is matched to itself, and remove duplicates (there shouldn't be any?) 
all_rhs = all_rhs[var1!=var2]
all_rhs[, label:=paste0(var1, " ~~ 0*", var2)]


#Pull out just the vector of labels to paste into the model object. 
all_labels = as.character(all_lhs$label, all_rhs$label)
write.csv(all_labels, "C:/Users/elineb/Desktop/zero_covariances_2.csv", row.names=F)
