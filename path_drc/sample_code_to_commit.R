# ----------------------------------------------
# Caitlin O'Brien- Carelli
# Hi Constant! Welcome to the shared folder!
# Let's make changes in this (very simple) farming data 

# ----------------------------------------------

#-----------------------------------------------
# Load packages
# ----------------------------------------------
rm(list=ls())
library(data.table)
library(openxlsx)
library(ggplot2)
library(stringr)
#---------------------------------------
# Set up directories 
#----------------------------------------

# create a data table with the number of animals on my farm
animals = c('cat', 'dog', 'rabbit', 'horse', 'toad')
counts = rev(c(3:7))
mammal = c(T, T, T, T, F)

# bind them together
farm = data.table(cbind(animals, counts, mammal))
str(farm)

# the counts are characters - convert
farm[ ,counts:=as.numeric(counts)]

# how many mammals?
farm[mammal==T, sum(counts)]

# TEST FOR BETHANY
# COUNT THE NUMBER OF TOADS
# How many toads are there?


#-----------------------------------------

