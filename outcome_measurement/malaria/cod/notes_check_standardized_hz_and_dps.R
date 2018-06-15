
# Originally part of the prep COD Malaria data master script, used when developing the prep_data.R function. 

# ----------------------------------------------
# compare health zones and dps between years to make sure names are consistent
# ---------------------------------------------- 
hz2010 <- unique(fullData[year==2010, health_zone])
hz2011 <- unique(fullData[year==2011, health_zone])
hz2012 <- unique(fullData[year==2012, health_zone])
hz2013 <- unique(fullData[year==2013, health_zone])
hz2014 <- unique(fullData[year==2014, health_zone])
hz2015 <- unique(fullData[year==2015, health_zone])
hz2016 <- unique(fullData[year==2016, health_zone])
hz2017 <- unique(fullData[year==2017, health_zone])

hz_missing <- hz2017[!hz2017 %in% hz2016]
print(hz_missing)
hz_missing_2017 <- hz2016[!hz2016 %in% hz2017]
print(hz_missing_2017)


# ----------------------------------------------  
# ----------------------------------------------  
fullData$dps <- tolower(fullData$dps)

dps2010 <- unique(fullData[year==2010, dps])
dps2011 <- unique(fullData[year==2011, dps])
dps2012 <- unique(fullData[year==2012, dps])
dps2013 <- unique(fullData[year==2013, dps])
dps2014 <- unique(fullData[year==2014, dps])
dps2015 <- unique(fullData[year==2015, dps])
dps2016 <- unique(fullData[year==2016, dps])
dps2017 <- unique(fullData[year==2017, dps])

dps_missing <- dps2017[!dps2017 %in% dps2010]
print(dps_missing)
dps_missing_2017 <- dps2010[!dps2010 %in% dps2017]
print(dps_missing_2017)
