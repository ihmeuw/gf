# What variables are in the data, but not the model objects? 
# Also, make a grid of first-half model names to remove zero-correlation error terms. 

#----------------------
# FIRST HALF 
#----------------------
prepped_data = readRDS(outputFile3)
load(outputFile4a)

model = copy(data)

names(prepped_data)[!names(prepped_data)%in%names(model)] #NONE. 

#Sanity check- you should get all of the cumulative vars here. 
names(model)[!names(model)%in%names(prepped_data)]

# Make fixed covariance terms! Should be in this format: 
# budget_M2_3_cumulative ~~ 0*budget_M3_1_cumulative

input_vars = names(model)[!grepl("_act|_out", names(model))]
input_vars = input_vars[grep("_cumulative", input_vars)]

all_inputs = data.table(expand.grid(lhs = input_vars, rhs = input_vars))
all_inputs = all_inputs[lhs!=rhs]

all_inputs[, output:=paste0(lhs, " ~~ 0*", rhs)]
write.csv(all_inputs, "C:/Users/elineb/Desktop/removing_correlated_errors_1st.csv", row.names=F)

output_vars = names(model)[grep("_out_cumulative", names(model))]

all_outputs = data.table(expand.grid(lhs = output_vars, rhs = output_vars))
all_outputs = all_outputs[lhs!=rhs]
all_outputs[, output:=paste0(lhs, " ~~ 0*", rhs)]
write.csv(all_outputs, "C:/Users/elineb/Desktop/removing_correlated_errors_1st_out.csv", row.names=F)


#----------------------
# SECOND HALF 
#----------------------
prepped_data = readRDS(outputFile3a)
load(outputFile4ab)

model = copy(data)

unmapped_vars = names(prepped_data)[!names(prepped_data)%in%names(model)]  
write.csv(unmapped_vars, "J:/Project/Evaluation/GF/impact_evaluation/gtm/raw_data/unmapped_second_half_vars_7.17.19.csv", row.names=F)

