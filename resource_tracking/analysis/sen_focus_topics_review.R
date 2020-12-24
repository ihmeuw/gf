# set up
library(data.table)
library(readxl)
library(xlsx)

# read in senegal test file
data <- as.data.table(read.csv('//ihme.washington.edu/ihme/snfs/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/keyword_search/SEN/test_senegal_focus_topic_search_12_22_2020.csv'))

###########################################################
# create a spreadsheet to review potential NFM3 focus topics
###########################################################

# subset rows
dt <- data[grant_period=="2021-2023" & budget_version=="approved"]

# subset columns
dt <- dt[,.(loc_name, grant_period, budget_version, disease, gf_module, gf_intervention, activity_description, topicAreaDesc)]

# change column names
setnames(dt, old=c('topicAreaDesc'),
         new=c('potentialTopicArea'))

# add new columns
dt$final_decision <- NA
dt$notes <- NA

# save as an excel spreadsheet
write.xlsx(dt, file='//ihme.washington.edu/ihme/snfs/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/keyword_search/SEN/Senegal_approvedgm2020_ft_activities.xlsx')

###########################################################
# create a spreadsheet to review potential NFM2 focus topics
###########################################################

# subset rows
dt2 <- data[grant_period!="2021-2023"]

# subset columns
dt2 <- dt2[,.(loc_name, grant_period, budget_version, disease, gf_module, gf_intervention, activity_description, topicAreaDesc)]

# change column names
setnames(dt2, old=c('topicAreaDesc'),
         new=c('potentialTopicArea'))

# add new columns
dt2$final_decision <- NA
dt2$notes <- NA

# save as an excel spreadsheet
write.xlsx(dt2, file='//ihme.washington.edu/ihme/snfs/Project/Evaluation/GF/resource_tracking/modular_framework_mapping/keyword_search/SEN/Senegal_nfm2_ft_activities.xlsx')
