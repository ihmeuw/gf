# prep table with model coefficients for annex of HSM paper - for Guatemala

# INSTRUCTIONS: The current working directory should be the root of this repo (set manually by user)
# ----------------------------------------------

#-----------------------------------------
# TO-DO: 
# * update to run for senegal

#-----------------------------------------

# ---------------------------------------
# Install packages and set up R  
# ---------------------------------------

# clear memory
rm(list=ls())

# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/gtm/set_up_r.r')

# -- This was taken from step 6a where the model results are saved in this datatable called urFit1 ---

# load model results - GTM model only has one half  
load(outputFile5a)
data1=copy(data)
model1=copy(model)
# means1 = copy(means)
# summaries1 = copy(summaries)
urFits1 = copy(urFits)
# load(outputFile5b)
# data2=copy(data)
# model2=copy(model)
# means2 = copy(means)
# summaries2 = copy(summaries)
#urFits2 = copy(urFits)

# compute averages (approximation of standard error, would be better as Monte Carlo simulation)
paramVars = c('est.std','est','se_ratio.std', 'se_ratio', 'se.std', 'se')
urFits1[, se_ratio.std:=se.std/est.std]
urFits1[, se_ratio:=se/est]
urFit1 = urFits1[, lapply(.SD, mean, na.rm=TRUE), .SDcols=paramVars, by=c('lhs','op','rhs')]
urFit1[se.std>abs(se_ratio.std*est.std), se.std:=abs(se_ratio.std*est.std)]
urFit1[se>abs(se_ratio*est), se:=abs(se_ratio*est)]

################## Section below cleans and reformats datatable into format for HSM paper

#load file with the estimates
temp1 <- urFit1

# load file with cleaned up names
temp1names <- fread("./impact_evaluation/gtm/visualizations/nodetable_first_half.csv")

# remove columns we don't need
temp1names[,c("x", "y"):=NULL]

# add labels for lhs
paper_file <- merge(x=temp1, y=temp1names, by.x="lhs", by.y="variable")

# change column names
setnames(paper_file, "label", "dependent variable (y)")

# merge onto x (independent variables) labels
paper_file <- merge(x=paper_file, y=temp1names, by.x="rhs", by.y="variable")

# change column names
setnames(paper_file, "label", "independent variable (x)")

#  calculate 95% CI Lower and Upper
paper_file$lower <- paper_file$est - (1.96*paper_file$se)
paper_file$upper <- paper_file$est + (1.96*paper_file$se)

# keep only dependent variable, independent variable, est, lower, upper
paper_file[,c("rhs", "lhs", "op", "est.std", "se_ratio.std", "se_ratio", "se.std", "se"):=NULL]

# round numbers to two significant figures
paper_file[, est:=signif(est, 2)]
paper_file[, lower:=signif(lower, 2)]
paper_file[, upper:=signif(upper, 2)]


# round numbers to two decimal places
# paper_file[, est:=round(est, 2)]
# paper_file[, lower:=round(lower, 2)]
# paper_file[, upper:=round(upper, 2)]

# add column with model and number
paper_file$Model <- "Guatemala TB"

# set order of column names
setcolorder(paper_file, c("Model", "dependent variable (y)", "independent variable (x)", "est", "lower", "upper"))

write.csv(paper_file, "G:/My Drive/IHME/PCE/Pubs/guatemala_model_annex.csv")
write.csv(paper_file, "J:/Project/Evaluation/GF/impact_evaluation/gtm/special_output/guatemala_model_annex.csv")
