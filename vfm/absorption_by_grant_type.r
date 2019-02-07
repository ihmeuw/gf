# ------------------------------------------------------------
# David Phillips
# 
# 1/8/2019
# Check the difference in Q1/Q2 2018 absorption by grant type
# (program continuation, full review, tailored review)
# ------------------------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# -------------------


# -------------------------------------------------------------------
# Files and directories
dir = 'J:/Project/Evaluation/GF/vfm/'
inFile = paste0(dir, 'outputs/synthesis_absorption_table.csv')
outFile = paste0(dir, 'visualizations/absorption_by_grant_type.pdf')
# -------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# identify program continuation grants
pc = c("COD-M-MOH (Q1-Q2 2018)", "COD-M-SANRU (Q1-Q2 2018)", 
	"MOZ-M-MOH (Q1-Q2 2018)", "MOZ-M-WV (Q1-Q2 2018)", "SDN-T-UNDP", 
	"SDN-H-UNDP (Q1-Q2 2018)", "SDN-M-MOH")
data[, type:='1_Full Review']
data[grant %in% pc | (country=='SEN' & disease!='tb'), type:='Continuation']

# identify tailored review (no extension grants have data)
data[country=='COD' & disease!='malaria', type:='Tailored']

# dummies
data[, full:=ifelse(type=='1_Full Review',1,0)]
data[, continuation:=ifelse(type=='Continuation',1,0)]
data[, tailored:=ifelse(type=='Tailored',1,0)]

# clean extreme values
data[, absorption:=absorption_q1_2018]
data[absorption>500, absorption:=NA]

# overall absorption
data[, expenditure:=(absorption/100)*budget]
agg = data[, .(expenditure=sum(expenditure,na.rm=T), budget=sum(budget,na.rm=TRUE)), by=c('country','disease','grant','continuation')]
agg[, absorption:=expenditure/budget]
# ----------------------------------------------------------------------------


# -------------------
# Analyze

# summarize
data[, .(mean=mean(absorption_q1_2018), sd=sd(absorption_q1_2018)), by='type']
data[, .(mean=mean(absorption, na.rm=T), sd=sd(absorption, na.rm=T)), by='type']
data[, .(mean=mean(absorption, na.rm=T), sd=sd(absorption, na.rm=T)), by=c('continuation','disease')]
means = data[, .(mean=mean(absorption, na.rm=T), sd=sd(absorption, na.rm=T)), by=c('continuation','disease')]
data[, .(mean=mean(absorption, na.rm=T), sd=sd(absorption, na.rm=T)), by='full']

# test 
glmfit = glm(absorption~factor(type), family='poisson', data=data)
exp(cbind(coef(glmfit), confint(glmfit)))
glmfit = glm(absorption~factor(continuation), family='poisson', data=data)
exp(cbind(coef(glmfit), confint(glmfit)))

# graph
p = ggplot(data[absorption<200], aes(x=absorption, group=factor(continuation), fill=factor(continuation))) + 
	geom_vline(data=means, aes(xintercept=mean, color=factor(continuation)), size=1.01, linetype='dashed') + 
	geom_density() + 
	facet_wrap(~disease) +
	labs(title='Absorption among Program Continuation and Other Grants by Disease', 
		y='Density of Observations', x='Absorption (%)', fill='Program\nContinuation',
		color='Program\nContinuation', caption='Absorption measured by grant-modules\nDashed lines are means') + 
	theme_bw()
pdf(outFile, height=5.5, width=8)
p
dev.off()
# -------------------
