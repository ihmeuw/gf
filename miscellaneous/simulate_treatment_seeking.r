# --------------------------------------------------------
# David Phillips
# 
# 5/16/2018
# Simulate under-reporting due to <100% treatment-seeking
# --------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
set.seed(1)
library(data.table)
library(boot)
library(lme4)
library(ggplot2)
# ------------------


# --------------------------------------------------------
# Parameters and settings

# number of geographic areas (<25)
A = 20

# date range
dates = seq(2015, 2017)

# incidence parameters
b0 = 10200
b1 = -5
b2 = rnorm(A-1, 0, 1)
e1 = rnorm(A*length(dates), 0, 1.5)

# treatment-seeking parameters
t0 = logit(.75)
t1 = rnorm(A-1, 0, .05)
e2 = rep(rnorm(A, 0, 1.5), length(dates))
# --------------------------------------------------------


# -----------------------------------------------------------------------
# Create simulated data

# make frame
data = data.table(expand.grid(area=LETTERS[1:A], 
		date=dates))

# make true incidence
data[, incidence:= (model.matrix(~date+area, data) %*% c(b0, b1, b2))+e1]

# make some covariate that's correlated with both incidence and tx-seeking

# make treatment-seeking assuming independence between tx-s and true incidence
data[, tx_seeking:=inv.logit((model.matrix(~area, data) %*% c(t0, t1))+e2)]

# simulate observed counts
data[, notifications:=incidence*tx_seeking]

# make groups of areas based on tx-seeking?
quintiles = quantile(data$tx_seeking, seq(0, 1, by=.2))
quintiles[1]=0
quintiles[length(quintiles)]=1
data[, group:=cut(tx_seeking, quintiles)]
levels(data$group) = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5')
# -----------------------------------------------------------------------


# --------------------------------------------------------
# Run Analysis

# linear
lmFit = lm(notifications~tx_seeking, data)

# random slope on group
lmeFit = lmer(notifications ~ tx_seeking + (tx_seeking | group), data)

# adjust linear
data[, adjusted_lm:=notifications + (coef(lmFit)['tx_seeking']*(1-tx_seeking))]

# random effects
coefs = coef(lmeFit)$group + ranef(lmeFit)$group
data[, adjusted_lme:=notifications + (coefs['tx_seeking']*(1-tx_seeking))]
# --------------------------------------------------------


# --------------------------------------------------------
# Graph
ggplot(data, aes(y=incidence, x=date)) + 
	geom_point(color='green') + 
	geom_point(aes(y=notifications), color='red') + 
	geom_point(aes(y=adjusted), color='blue') + 
	facet_wrap(~area)
	
ggplot(data, aes(y=adjusted, x=incidence)) + 
	geom_point()
	
ggplot(data, aes(x=tx_seeking)) + 
	geom_histogram()
# --------------------------------------------------------
