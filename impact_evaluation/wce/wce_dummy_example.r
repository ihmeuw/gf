# ----------------------------------------------
# David Phillips
# 
# 1/29/2019
# Dummy example of weighted cumulative exposure
# ----------------------------------------------


# --------------------
# Set up R
rm(list=ls())
set.seed(1)
library(data.table)
library(ggplot2)
# --------------------


# -------------------------------------------------------------------------------------
# Make fake data
data = data.table(year = seq(from=2010, to=2017), nets = rnorm(mean=1000, sd=100, n=8))
# -------------------------------------------------------------------------------------


# ----------------------------------------------
# Calculate cumulative exposure to nets

# simple cumulative sum for comparison
data[, simple_cumulative:=cumsum(nets)]

# weighting function
weightFunction = function(sigma=NA, t=NA, delta=NA) { 
	if (!is.na(delta)) delta = delta-1 # if delta is specified
	if (is.na(delta)) delta = rev(seq(length(t))-1) # this is so you can pass an actual time variable
	w = exp( -(1/(2*(sigma^2))) * (delta^2))
	if (is.na(t)) w = abs(0.5-rev(w)) # this is so you can use optim to identify the sigma that gets closest to 0.5
	return(w)
}

# select a sigma that makes weightFunction(delta3)=0.5 and weightFunction(delta5)=0.5 
# like in Abrahamowcz 2006, 3 and 5 selected based on 3 and 5-year life-spans of ITNs
sigma3 = optim(par=.75, fn=weightFunction, delta=3)$par
sigma5 = optim(par=.75, fn=weightFunction, delta=5)$par

# create weighted sums with sigma3
for(i in seq(nrow(data))) { 
	# create weights
	wvar = paste0('w',i)
	data[1:i, (wvar):=weightFunction(sigma=sigma3, t=year)]
	
	# create weighted sum
	tmp = cumsum(data[1:i]$nets*data[1:i][[wvar]])
	data[i, weighted_cumulative:=tmp[i]]
}
# ----------------------------------------------


# ----------------------------------------------
# Graph to check

# graph weight curve
ggplot(data, aes(y=w8,x=year)) + 
	geom_line()
	
# graph cumulative sums
ggplot(data, aes(y=nets, x=year)) + 
	geom_line() + 
	geom_line(aes(y=weighted_cumulative), color='red') + 
	geom_line(aes(y=simple_cumulative), color='green') 
# ----------------------------------------------
