# script 4a

# this code fixes typos that are identified through the process of working with the data
# ideally this should be run before the data is cleaned because often some values are then used to calculate others

# Cleaning up uganda target_pct column in which some values are incorrectly typed into the PUDRs

dt$target_pct[which(dt$loc_name=="uga" & dt$target_pct==1)] <- 100
dt$target_pct[which(dt$loc_name=="uga" & dt$target_pct==0.5)] <- 50
dt$target_pct[which(dt$loc_name=="uga" & dt$target_pct==0.85)] <- 85
dt$target_pct[which(dt$loc_name=="uga" & dt$baseline_year==2014 & dt$baseline_value==19 & dt$indicator_type=="Impact")] <- 6.7
