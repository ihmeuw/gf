############################################################
# Adapted from Aaron O-Z's match_municipalities_v3
# to match the names of health zones in the 2016 DRC notifications
# to the WHO shapefile of health zones
# J. Ross
# June 15, 2017 


rm(list=ls()) 

shape <- read.csv("J:/Project/TB/geospatial/Mortality/COD/DRC_shapefile_units.csv")
zone <- read.csv("J:/Project/TB/geospatial/Mortality/COD/Health_zones_DRC_2016.csv")

###################################
## try some basic fuzzy matching ##
###################################

## make all chars lowercase
shape_low <- tolower(shape$Name)
zone.low <- tolower(zone$Name)

## look at 'edit' distances between strings. Function adist looks for string distances.
dists <- sapply(shape_low,
                FUN=function(x){
                  adist(x, zone.low)
                } )

min.dists <- apply(X=dists, MARGIN=2, FUN=min)

num.min <- sapply(X=1:length(min.dists),
                  FUN=function(x){
                    sum(dists[, x] == min.dists[x])
                  })

table(min.dists)
table(num.min)
table(min.dists, num.min)
#391 that match exactly with just one. Another 69 that are only another 1 off.

sum(min.dists == 0 & num.min > 1)
#Five cases where it looks like something matches perfectly in two regions

#########################
## assign good matches ##
#########################

## put row of matching gaul table entry into:
matched_row <- rep(NA, dim(shape)[1])
# rep function above puts NA into a vector with the length of shape (516)

## if there is 1 minimum, assume it's correct and match
one_matches <- which(num.min==1)
length(one_matches) ## 489
#which.min returns the index (aka position) of the minimmum value.
#So now these positions are hanging out in one.match, but how do they get bound together? See loop below.
for(i in one_matches){
  matched_row[i] <- which.min(dists[, i])
}

#############################################################################################
## manually assign remaining bad matches. 

#Umm, this step not yet implemented for this code!!!!!!!
#############################################################################################

m <- cbind(shape, zone[matched_row, c(1:4)])

write.csv(m, "J:/Project/TB/geospatial/Mortality/COD/DRC_shapefile_matches.csv")



