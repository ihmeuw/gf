# ---------------------------------------------------------
# David Phillips

# 2/23/2018
# Testing a system of equations to estimate a regression 
# where we know the explanatory variable is underestimated
# ---------------------------------------------------------


# ---------------------------------------------------------
# Set up R
rm(list=ls())
set.seed(1)
library(data.table)
library(blavaan)
# ---------------------------------------------------------


# ---------------------------------------------------------
# Define parameters

# sample size
n = 100

# mean fraction of spending financed by the global fund
p = .6

# variance of the fraction of spending financed by the global fund
pv = .05

# residual variance
r = .1

# correlation between x and output
b2_true = .75

# mean fraction by which x is underestimated
b1_true = .8

# variance of the fraction by which x is underestimated
b1v = .01
# ---------------------------------------------------------


# ---------------------------------------------------------
# Create simulated data

# compute beta shape parameters from mean and variance
shape1 = ((1 - p) / pv - 1 / p) * p ^ 2
shape2 = shape1 * (1 / p - 1)

# x = fraction of spending financed by the global fund
# rows = district-years
data = data.table(x = rbeta(n, shape1, shape2))

# add a residual variance term
data[, e:=rnorm(n, 0, r)]

# output = things consumed
data[, output:=b2_true*x + e]

# compute other beta shape parameters from mean and variance
shape1_prime = ((1 - b1_true) / b1v - 1 / b1_true) * b1_true ^ 2
shape2_prime = shape1_prime * (1 / b1_true - 1)

# create observed x, an underestimate of true x
data[, frac:=rbeta(n, shape1_prime, shape2_prime)]
data[, x_prime:=x*frac]
# ---------------------------------------------------------


# ---------------------------------------------------------
# Define model

# frequentist model
modelf = '
	x_hat =~ b1*x_prime
	output ~ b2*x_hat
	b1 < 1
	b1 > 0 
	b2 > 0
'

# bayesian model
modelb = '
	x_hat =~ prior("dbeta(2,2)")*x_prime
	output ~ prior("dgamma(1,7)")*x_hat
'
# ---------------------------------------------------------


# ---------------------------------------------------------
# Fit model

# test simple linear models

# true model (if we knew x)
summary(lm(output~x, data))

# "observed" model (should overestimate beta2)
summary(lm(output~x_prime, data))

# sem
summary(sem(modelf, data))

# bsem
summary(bsem(modelb, data))
# ---------------------------------------------------------