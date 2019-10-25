#---------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Financial graphs to support DRC HIV deep dive slides 
# DATE: September 3, 2019 
#----------------------------------------------------

library(data.table) 

#Set up file paths, and source functions
repo_root = "C:/Users/elineb/Documents/gf/"
setwd(repo_root)
save_loc = "J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/DRC HIV Deep dive slides September 2019/"

source("resource_tracking/visualizations/graphing_functions.r", encoding="UTF-8")

funding_landscape = funding_landscape('cod', 'hiv', 2010, 2016, includeGHE=TRUE, altTitle="Funding landscape in DRC for HIV, 2010-2016", 
                                      altCaption="*GHE only available from 2010-2015")

ggsave(paste0(save_loc, "cod_hiv_funding_landscape.png"), funding_landscape, height=10, width=18)


# Hi Emily, 
# 
# Could you re-do the absorption figure on slide 15 to use only the annual PU/DR to compare S1 and S2 and update the text?
#Same figure, but just using the percentages for S1 and S2 as reported at the end of 2018 in the annual PU/DR. 
#From what I was seeing in Slack today, not all data may be reported by the end of S1, so the complete comparison of semester-on-semester change would need to come from the annual PU/DR. 
# 
# On the same slide, could you also add a bullet point on why we are not including the PNLS PU/DR? Just briefly explaining it
#– for example “PNLS submitted an S1 PUDR in MONTH of 2018 but did not list any reported expenditures. 
#We are awaiting the receipt of the approved annual PU/DR” or something like that. I would add this but I’m not familiar with the data requests.
# 
# I asked this via Slack, but could you send the updated figure  for slide 14 wide? The version you sent including GHE looks good but is just too small. Thanks! Caitlin 
