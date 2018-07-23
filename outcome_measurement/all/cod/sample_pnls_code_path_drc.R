# Sample code for analyzing PNLS indicators in DHIS2
# Date:
#
# This code is used to run descriptive statistics and visualize DHIS2 data
# -------------------------------------------

#---------------------
# Install R packages 
# You only need to install packages on your computer the first time you use them

install.packages('data.table')
install.packages('httr')
install.packages('ggplot2')
install.packages('dplyr')

# --------------------
# Set up R 
# Load the packages you need for analysis
# Load these packages every time you open RStudio

rm(list=ls())
library(data.table)
library(httr)
library(ggplot2)
library(dplyr)

# --------------------

# --------------------
# import the data from your home computer
# download the data from Basecamp and record the directory
# choose either the csv or rds file

# replace the directory (in the brackets) with the file location on your computer

# import the file as a RDS (replace directory):
tb_hiv <- readRDS('C:/Users/ccarelli/Documents/tb_hiv.rds')

# if you prefer, import the CSV file (replace directory):
# tb_hiv <- read.csv('C:/Users/ccarelli/Documents/tb_hiv.csv', stringsAsFactors = F)

# --------------------
# on the IHME team, we prefer to use a package called 'data tables'
# data tables are more similar to Stata 

# convert the data to a data table
tb_hiv <- data.table(tb_hiv)

#-------------------------------------------
# all DHIS2 data sets are shaped long
# there is a single unique value for each combination of variables
# introductory commands for viewing the data table

# view the data table (opens a new tab)
View(tb_hiv)

# view the first six rows of the data table
head(tb_hiv)

# view the structure of the data table, including variable types
str(tb_hiv)

# view unique data elements
tb_hiv[ , unique(element)]

# to see the names of the variables in the data set
names(tb_hiv)

# view unique data elements, divided by the type of indicator
tb_hiv[ ,unique(element), by=indicator]

# there are 2,910 health facilities reporting in this data set
tb_hiv[ ,length(unique(org_unit))]

#------------------------------------------
# create a new variable to identify Maniema, Tshopo, and Kinshasa
# (i included this variable, but we can create it here)
# if the dps is Maniema, Tshopo, or Kinshasa, make 'mtk' equal to 'yes'
tb_hiv[dps=='Maniema' | dps=='Tshopo' | dps=='Kinshasa', mtk:='Yes']

# if mtk is missing (not equal to yes), make it equal 'No'
tb_hiv[is.na(mtk), mtk:='No']


#-----------------------------------------
# create a graph of a single indicator

# create a data table, called dt1, that includes only one data element
# you can subset by element name or ID
dt1 <- tb_hiv[element_id=='Gv1UQdMw5wL']

# the dt1 data table has a unique row for each facility
# we do not want to graph 3,000 sep
# sum over all facilities by date and "category" and assign the sums to dt1
dt1 <- dt1[ ,.(value=sum(value)), by=.(date, category, element)]

# view the first six rows of dt1
head(dt1)

# what are the different categories and elements in dt1?
dt1[ , unique(element), by=category]

# graph the new data table, with different lines for each category
# geom_point() creates a scatter plot; geom_line() creates a line graph
# see the ggplot2 cheat sheet on Basecamp for more information
ggplot(dt1, aes(x=date, y=value, color=category, group=category)) +
  geom_point() +
  geom_line() +
  theme_bw()

# note: you don't need theme_bw(), it just looks nice!

#-----------------------------------------
# graph the number of stockout days using dt1

# create a data table that only includes the element 'Determine complete' and the category 'jours RS'
stock_outs <- tb_hiv[element_id=='Gv1UQdMw5wL' & category=='Nbr de jours RS']

# view the new data table
head(stock_outs)

# sum over the values, but this time by DPS
# you do not need category or element - they are included to keep the names for graph titles
stock_outs <- stock_outs[ ,.(value=sum(value)), by=.(date, element, mtk, dps)]

# graph the number of stock out days per month by district
ggplot(stock_outs, aes(x=date, y=value, color=dps, group=dps)) +
  geom_point() +
  geom_line() +
  theme_bw()


# create a graph for only Maniema, Tshop, and Kinshasa, with one graph for each
# changes the labels on the graph 
ggplot(stock_outs[mtk=='Yes'], aes(x=date, y=value, color=dps)) +
  geom_point() +
  geom_line() +
  facet_wrap(~dps) +
  theme_bw() +
  labs(title=stock_outs$element, x='Date', y=stock_outs$category)

#----------------------------------------








