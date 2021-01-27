# create senegal focus topic dataset that incorporates feedback received from SEN CT

# set up
library(data.table)
library(readxl)
library(googledrive)

# directory filepaths
box = paste0("C:/Users/frc2/Box Sync/Global Fund Files/")
j = "\\\\ihme.washington.edu/ihme/snfs"
dir = paste0(j, '/Project/Evaluation/GF/resource_tracking/')
mapping_dir = paste0(dir, "modular_framework_mapping/")

# files
inFile = paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv')

# read in prepped data
data <- fread(inFile)

# search on google drive for files to access
# drive_find(pattern="ft_activities", type = "xlsx")

# read in spreadsheets with final confirmed files (obtain from Google Drive storage)
nfm2_ft_activities <- drive_get(as_id("1-a-lFhkWD_U88uuBokFwrhgS6yBQhTdQ"))
nfm3_ft_activities <- drive_get(as_id("1gIlazzSnAzG1vrF3r7sz1695cGOiXx4X"))

local_file_nfm2 = paste0(mapping_dir, "keyword_search/SEN/sen_ct_id_ft_nfm2.xlsx") # where to save copy of file locally
local_file_nfm3 = paste0(mapping_dir, "keyword_search/SEN/sen_ct_id_ft_nfm3.xlsx") # where to save copy of file locally

drive_download(file=nfm2_ft_activities,
               path=local_file_nfm2,
               overwrite = TRUE)

drive_download(file=nfm3_ft_activities,
               path=local_file_nfm3,
               overwrite = TRUE)

# subset prepped data to just be for Senegal
dt <- data[loc_name=="Senegal"]

# merge final CT feedback for NFM2 and NFM3
ct_nfm2 <- as.data.table(read_xlsx(paste0(mapping_dir, "keyword_search/SEN/sen_ct_id_ft_nfm2.xlsx")))
ct_nfm3 <- as.data.table(read_xlsx(paste0(mapping_dir, "keyword_search/SEN/sen_ct_id_ft_nfm3.xlsx")))

# bind two files together
ct_nfm2_nfm3 <- rbind(ct_nfm2, ct_nfm3, fill=TRUE)

# subset rows to those identified by the CT
ct_feedback <- ct_nfm2_nfm3[final_decision==TRUE]

# rename columns in CT feedback
setnames(ct_feedback, old = c("potentialTopicArea", "final_decision"), 
         new = c("ctTopicAreaDesc", "isCTtopicArea"))

# subset the rows to exclude notes and budget version
ct_feedback <- ct_feedback[,.(loc_name, grant_period, disease, gf_module, gf_intervention, activity_description, ctTopicAreaDesc, isCTtopicArea)]

# remove duplicates
ct_feedback <- unique(ct_feedback)

# save data set together
dt2 <- merge(dt, ct_feedback, 
             by=c('loc_name', 'grant_period', 'disease', 'gf_module', 'gf_intervention', 'activity_description'),
             all.x = TRUE)

#### to fix --maybe just using activities to track funds is sufficient, no?????
write.csv(dt2, file = paste0(box, "tableau_data/sen_budgetRevisions_with_frBudgets_activityLevel_ct.csv"))
