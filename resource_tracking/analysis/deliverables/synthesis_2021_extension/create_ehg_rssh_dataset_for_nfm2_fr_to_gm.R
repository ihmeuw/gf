# Francisco Rios
# # create EHG RSSH data set for NFM3 FR to GM analyses

# clear workspace
rm(list=ls())

# set up
library(data.table)
library(ggplot2)
library(readxl)

# Files and directories

user = as.character(Sys.info()[7])

box = paste0("C:/Users/",user,"/Box Sync/Global Fund Files/")
inFile1 = paste0(box, '/synthesis/2021_extension/data/EHG Synthesis Budget Variance .xlsx') #EHG NFM3 FR data
inFile2 = paste0(box, '/synthesis/2021_extension/data/RSSH PF comparison v5.xlsx') #EHG NFM3 GM data

# inFile3 = paste0(box, 'synthesis/2021_extension/data/Synthesis RSSH 2S Analysis.xlsx') #EHG NFM3 GM data

out.path = paste0(box, 'synthesis/2021_extension/data/')

# read in EHG NFM3 FR data -------------------------------
all_fr <- as.data.table(read_xlsx(inFile1, sheet = "RSSH"))

# reshape data
ehg_fr_data <- all_fr[loc_name%in%c("Cambodia", "Myanmar", "Mozambique"),.(funding_request20=sum(nfm3_funding_request20, na.rm=T)), 
                      by=c('loc_name')]

# read in EHG NFM3 GM data
gm_mmr <- as.data.table(read_xlsx(inFile2, sheet = "MMR_w partner")) #Myanmar
gm_moz <- as.data.table(read_xlsx(inFile2, sheet = "Moz")) #Mozambique
gm_cam <- as.data.table(read_xlsx(inFile2, sheet = "Cam")) #Cambodia

# rbind the different data sets
gm_1 <- rbind(gm_mmr, gm_moz, gm_cam, fill=TRUE)

# # reshape each respective data set
ehg_gm_data <- gm_1[,.(budget=sum(Budget, na.rm = TRUE)),
                    by=c('Country')]

# # read in EHG NFM3 GM data
# ehg_gm <- as.data.table(read_xlsx(inFile3))
# 
# 
# ehg_gm_data <- ehg_gm[`Grant period`=="2021-23",.(budget=sum(Budget, na.rm = TRUE)), 
#   by=c('Country')]

# change names
setnames(ehg_gm_data, old = c("Country", "budget"), new = c("loc_name", "approved"))

# rename countries for plotting
ehg_gm_data[loc_name=="MMR", loc_name:="Myanmar"]
ehg_gm_data[loc_name=="Moz", loc_name:="Mozambique"]
ehg_gm_data[loc_name=="CAM", loc_name:="Cambodia"]

# merge data sets together
ehg_rssh_data <- merge(ehg_fr_data, ehg_gm_data, by="loc_name")

# save file in synthesis data folder
write.csv(ehg_rssh_data, paste0(out.path, "ehg_rssh_data.csv"))
