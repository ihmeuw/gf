# ----------------------------------------------
# David Phillips
# 
# 5/17/2019
# Example code for basic analysis in R
# Data source: http://ghdx.healthdata.org/gbd-results-tool
# ----------------------------------------------


# --------------------
# Set up R

# clear memory
rm(list=ls())

# load packages
library(data.table) # this helps with data analysis
library(ggplot2) # this makes nice graphs
# --------------------


# -------------------------------------
# Files and directories

# input file
inFile = 'D:/R Practice/GBD Estimates.csv'

# output files
graphFile = 'D:/R Practice/Graphs.pdf'
# -------------------------------------


# ----------------------------------------------
# Load, explore and prep the data

# load data
data = fread(inFile)

# look at the data
data

# check how many rows and columns are in the data
dim(data)

# check which countries, diseases and measures are in the data
unique(data$location)
unique(data$cause)
unique(data$measure)

# format variables (note: ':=' is from the data.table package)
data[, val:=as.numeric(val)]
data[, upper:=as.numeric(upper)]
data[, lower:=as.numeric(lower)]

# subset variables to drop age and sex (they aren't necessary in this dataset)
data = data[, c('measure', 'location', 'cause', 'metric', 'year', 'val', 'upper', 'lower'), with=FALSE]

# subset observations to only Senegal (and make a different data.table object)
senegalData = data[location=='Senegal']

# make a new variables (just as an example)
data[, standard_error:=(upper-val)/1.96]
# ----------------------------------------------


# ---------------------------------------------------------------------------------------
# Run analyses

# what is the prevalence of each disease in 2017 in Senegal?
senegalData[measure=='Prevalence' & year==2017 & metric=='Percent']

# what is the average number of new cases per year for each disease in Senegal since 2000?
senegalData[measure=='Incidence' & year>=2000 & metric=='Number', mean(val), by=cause]

# regression of deaths due to malaria over time in Senegal
lm(val~year, data=senegalData[measure=='Deaths' & metric=='Number' & cause=='Malaria']
# ---------------------------------------------------------------------------------------


# ----------------------------------------------
# Graphs

# make a graph of deaths due to malaria over time in Senegal
ggplot(senegalData[measure=='Deaths' & metric=='Number' & cause=='Malaria'], aes(y=val, x=year)) + 
	geom_point() + # this makes a scatter plot
	geom_smooth(method='lm') + # this adds a linear regression line
	labs(title='Deaths Due to Malaria in Senegal', subtitle='Model Estimates from GBD',  
		y='Number of Deaths', x='Year') + # this adds titles 
	theme_bw() # this part just makes it look better
	
# make a graph that includes the upper/lower of the estimates in the data
ggplot(senegalData[measure=='Deaths' & metric=='Number' & cause=='Malaria'], aes(y=val, x=year, ymax=upper, ymin=lower)) + 
	geom_ribbon(fill='grey75') + 
	geom_point() +
	geom_line() +
	theme_bw() 
	
# make a graph with all three diseases and a smoothed regression line
ggplot(senegalData[measure=='Deaths' & metric=='Number'], aes(y=val, x=year, color=cause)) + 
	geom_point() + 
	geom_smooth() + # method='lm' makes a linear regression, the default method is "loess"
	labs(title='Deaths Due to the Three Globla Fund Diseases in Senegal', subtitle='Model Estimates from GBD',  
		y='Number of Deaths', x='Year', color='Cause of Death') + # this adds titles 
	theme_bw() 
	
# save the graph as an object
p = ggplot(senegalData[measure=='Deaths' & metric=='Number'], aes(y=val, x=year, color=cause)) + 
	geom_point() + 
	geom_smooth() + 
	labs(title='Deaths Due to the Three Globla Fund Diseases in Senegal', subtitle='Model Estimates from GBD',  
		y='Number of Deaths', x='Year', color='Cause of Death') + 
	theme_bw() 	
	
# make the same graph comparing countries
p2 = ggplot(data[measure=='Deaths' & metric=='Number'], aes(y=val, x=year, color=cause)) + 
	geom_point() + 
	geom_smooth() + 
	facet_wrap(~location, scales='free') + # this makes different "facets" for each country
	labs(title='Deaths Due to the Three Globla Fund Diseases in Senegal', subtitle='Model Estimates from GBD',  
		y='Number of Deaths', x='Year', color='Cause of Death') + 
	theme_bw() 
# ----------------------------------------------


# --------------------------------
# Save the graph object in a pdf
pdf(graphFile, height=6, width=9) # this opens the pdf
p # this prints the graph
p2
dev.off() # this closes the pdf
# --------------------------------
