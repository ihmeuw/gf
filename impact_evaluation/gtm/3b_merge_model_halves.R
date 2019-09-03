# AUTHOR: Emily Linebarger 
# PURPOSE: This code merges together the first and second-half datasets prepped in 3 and 3b for a combined model run. 
# DATE: August 23, 2019 

first_half = readRDS(outputFile3) 
second_half = readRDS(outputFile2c)
second_half = second_half[date>=START_YEAR]
names(second_half) = gsub("_value", "", names(second_half))

#Make sure you aren't merging any variables twice. 
overlap = names(first_half)[names(first_half)%in%names(second_half)]
overlap = overlap[!overlap%in%c('department', 'date')]

#Keep variables from first half model where there's overlap. 
keepVars = names(second_half)[!names(second_half)%in%overlap]
second_half = second_half[, keepVars, with=F]

#Merge datasets together on date and department. 
dt = merge(first_half, second_half, by=c('date', 'department'), all=T)

saveRDS(dt, outputFile3b)
archive(outputFile3b)

print("Step 3b: merge model halves completed.")