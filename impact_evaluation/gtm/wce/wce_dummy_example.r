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

# parameters
syear = 2010
eyear = 2017

# generate date
data = data.table(year = seq(from=2010, to=2017), nets = rnorm(mean=1000, sd=500, n=8))

# backcast
tmp = data[rep(which(data$year==syear),10)]
tmp[, year:=seq(from=syear-nrow(tmp), to=syear-1)]
lmFit = lm(nets~year, data)
backcast = predict(lmFit, newdata=tmp)
data = rbind(tmp, data)
data[year<syear, nets:=backcast]
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
	wvar3 = paste0('w3',i)
	wvar5 = paste0('w5',i)
	data[1:i, (wvar3):=weightFunction(sigma=sigma3, t=year)]
	data[1:i, (wvar5):=weightFunction(sigma=sigma5, t=year)]
	
	# create weighted sum
	tmp3 = cumsum(data[1:i]$nets*data[1:i][[wvar3]])
	data[i, weighted_cumulative3:=tmp3[i]]
	tmp5 = cumsum(data[1:i]$nets*data[1:i][[wvar5]])
	data[i, weighted_cumulative5:=tmp5[i]]
}
# ----------------------------------------------


# ----------------------------------------------
# Graph to check

# set up to graph weights
wvar3 = paste0('w3',nrow(data))
wvar5 = paste0('w5',nrow(data))
graphData1 = data[, c('year', wvar3, wvar5), with=FALSE]
graphData1 = melt(graphData1, id.vars='year')
graphData1[, t:=eyear-year]
graphData1[, variable:=ifelse(variable==wvar3, '3-Year ITN Life', '5-Year ITN Life')]

# graph weight curve
ggplot(graphData1[year>=syear], aes(y=value, x=t, linetype=variable)) + 
	geom_line() + 
	labs(title='Weight Functions', y='Weight', x='Time Since Distribution of Net') + 
	theme_bw()
	
# set up to graph cumulative sums
colors=c('Quantity'='black', 'Weighted Cumulative Sum (3-year ITN life)'='red', 
	'Weighted Cumulative Sum (5-year ITN life)'='purple', 
	'Simple Cumulative Sum'='green')

# graph cumulative sums
ggplot(data[year>=syear], aes(y=nets, x=year)) + 
	geom_line(aes(color='Quantity')) + 
	geom_line(aes(y=weighted_cumulative3, color='Weighted Cumulative Sum (3-year ITN life)')) + 
	geom_line(aes(y=weighted_cumulative5, color='Weighted Cumulative Sum (5-year ITN life)')) + 
	geom_line(aes(y=simple_cumulative, color='Simple Cumulative Sum')) + 
	scale_color_manual(values=colors) + 
	theme_bw()
# ----------------------------------------------
