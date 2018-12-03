# ---------------------------------------------------------
# David Phillips

# 12/3/2018
# Testing a system of equations to estimate sequential regressions
# Similar to how the "dose-response" investment intensity model might work
# ---------------------------------------------------------


# -----------------
# Set up R
rm(list=ls())
set.seed(1)
library(lavaan)
library(systemfit)
# -----------------


# -------------------------
# Parameters 

# sample size
n=100

# regression coefficients
beta0 = 1
beta1 = 5
beta2 = 2
beta3 = 6
beta4 = -2
# -------------------------


# --------------------------------------------
# Simulate data

# error terms
e1 = rnorm(n, 0, 100)
e2 = e1 + rnorm(n, 0, 50)

# observed data
x = runif(n, 0, 100)
y = beta0 + x*beta1 + e1
t = runif(n, 0, 10)
z = beta2 + y*beta3 + t*beta4 + e2

# data frame
data = data.frame('z'=z, 'y'=y, 'x'=x, 't'=t)
# --------------------------------------------


# -----------------------------
# Run alternative models

# sem
model = '
	z ~ y + t
	y ~ x
'
summary(sem(model, data))

# sur
eq1 = z ~ y + t
eq2 = y ~ x
eqsystem = list(eq1, eq2)
summary(systemfit(eqsystem))

# ols
summary(lm(y~x, data))
summary(lm(z~y+t, data))
# -----------------------------
