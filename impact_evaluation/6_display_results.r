# -----------------------------------
# David Phillips
# 
# 2/4/2019
# This visualizes results of the SEM
# -----------------------------------


# -----------------------------------------------
# Load/prep data and functions

source('./impact_evaluation/_common/set_up_r.r')

# load the custom predict_lavaan.r function
source('./impact_evaluation/_common/predict_lavaan.r')

# load home-made sem graphing function FIX THIS FILE PATH
source('./impact_evaluation/visualizations/graphLavaan.r')

# load model results
load(outputFile5b)

# load nodeTable for graphing FIX THIS FILE PATH
nodeTable = fread('C:/local/gf/impact_evaluation/visualizations/vartable.csv')

# ensure there are no extra variables introducted from nodeTable
nodeTable = nodeTable[variable %in% names(data)]
# -----------------------------------------------


# ----------------------------------------------
# Display results

# pre-packed alternatives for sem graphics
# semPaths(semFit, 'std', intercepts=FALSE)
# lavaanPlot(model=semFit, coefs=TRUE)

# my sem graph function
p = semGraph(semFit, nodeTable=nodeTable)
# ----------------------------------------------


# ----------------------------------------------
# Example counterfactual

# set up alternative budget FIX THIS WRONG MATH
newData = copy(data)
newData[, budget_M1_1_cumulative:=budget_M1_1_cumulative+5000] # take some away from ITNs and give it to ACTs
newData[, budget_M1_2_cumulative:=budget_M1_2_cumulative+5000] # take some away from ITNs and give it to ACTs
newData[, budget_M2_1_cumulative:=budget_M2_1_cumulative+5000] # take some away from ITNs and give it to ACTs
newData[, budget_M2_3_cumulative:=budget_M2_3_cumulative+5000] # take some away from ITNs and give it to ACTs
newData[, budget_M2_6_cumulative:=budget_M2_6_cumulative+5000] # take some away from ITNs and give it to ACTs
newData[, budget_M3_1_cumulative:=budget_M3_1_cumulative+5000] # take some away from ITNs and give it to ACTs

newData = newData[1]

# predict counterfactual using lavPredict
# https://github.com/yrosseel/lavaan/issues/44#issuecomment-265239994 for solution to lavPredict
preds_lavPredict = data.table(lavPredict(semFit, optim.method='BFGS', newdata=newData, type='yhat'))
preds_lavPredict$value_ITN_received
data$value_ITN_received

# predict counterfactual using algebra
cv <- fitted(semFit)$cov
i <- which(!grepl('budget',colnames(cv)))
j <- which(grepl('budget',colnames(cv)))
coef <- solve(cv[j,j],cv[j,i])
n = colnames(cv)[j]
preds_algebra <- data.table(as.matrix(newData[,n,with=F])%*%coef)

# predict counterfactual using predict_lavaan
preds_predict_lavaan = data.table(predict_lavaan(semFit, newdata=newData))


# check results
preds_lavPredict$value_ITN_received # returns same as original data
preds_algebra$value_ITN_received # returns something different... is it correct?
preds_predict_lavaan$value_ITN_received # returns correct number because it accounts for order
data$value_ITN_received[1]



cf = merge(data, preds, 'date')
cf = melt(cf, id.vars='date')
cf[, cf:=ifelse(grepl('.y',variable),'Counterfactual Budget', 'Actual Budget')]
cf[, graph_var:=grepl('.x|.y', variable)]
cf[, variable:=gsub('.x|.y','',variable)]

# graph comparison
ggplot(cf[graph_var==TRUE], aes(y=value, x=variable, fill=cf)) + 
	geom_bar(stat='identity', position='dodge') + 
	theme_bw() + 
	theme(axis.text.x=element_text(angle=315, hjust=0))
tmp1 = cf[cf=='Counterfactual Budget',.(counterfactual=mean(value)), by=variable]
tmp2 = cf[cf=='Actual Budget',.(actual=mean(value)), by=variable]
merge(tmp1, tmp2, by='variable')
# ----------------------------------------------


# -----------------------------------
# Save output
pdf(outputFile6, height=6, width=9)
print(p)
dev.off()
# -----------------------------------
