# ARV stockouts by facility - data prep
# Caitlin O'Brien-Carelli
# 10/30/2018

# ----------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)

# ----------------------
# home drive 
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# data directory
dir = paste0(j,  '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/arv_data/')

# working directory to aggregate
setwd(dir)

# output directory for the prepped data 
OutDir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/uga/arv_stockouts/')

# ----------------------
# read in the files 

i = 1
files = list.files('./', recursive=TRUE)

for (f in files) {
  
  # import the csv and convert to a data table
  arv_data = data.table(read.csv(paste0(dir, f), skip=1))
  
  # create useful variable names 
  setnames(arv_data, c("Health.Facility", "ART.Accredited", "X", "a..Total.ANC1.visits", 
                       "h..Has.Test.Kits.stockout", "i..Has.ARVs.stockout"),
           c("facility", "art", "district", "anc_visits", "test_kits", "arvs"))
  
  # subset to the relevant variables
  arv_data = arv_data = arv_data[ ,.(facility=as.character(facility), art=as.character(art), 
                                     district=as.character(district), anc_visits, tests=as.character(test_kits),
                                     arv=as.character(arvs))]
  
  # prep facilities
  arv_data[ , ip:=unlist(lapply(strsplit(arv_data$facility, '\\(' ), '[', 2))]
  arv_data[ , ip:=gsub("\\)", "", ip)]
  arv_data[ , facility:=unlist(lapply(strsplit(arv_data$facility, '\\(' ), '[', 1))]
  
  # drop out the duplicates - alter later to include multiple international partners
  arv_data = arv_data[!(duplicated(facility))]
  
  # remove 'district' from district names
  arv_data[ , district:=unlist(lapply(strsplit(arv_data$district, '\\s' ), '[', 1))]
  
  # alter district names to match the shape file
  arv_data[district=="Bunyangabu", district:="Kabarole"]
  arv_data[district=="Butebo", district:="Pallisa"]
  arv_data[district=="Kagadi", district:="Kibaale"]
  arv_data[district=="Kakumiro", district:="Kibaale"]
  arv_data[district=="Kyotera", district:="Rakai"]
  arv_data[district=="Namisindwa", district:="Manafwa" ]
  arv_data[district=="Omoro", district:="Gulu"]
  arv_data[district=="Pakwach", district:="Nebbi"]
  arv_data[district=="Rubanda", district:= "Kabale"]
  arv_data[district=="Rukiga", district:="Kabale"]
  arv_data[district=="Luwero", district:="Luwero"]
  arv_data[district=="Sembabule", district:="Ssembabule"]
  
  # add facility level
  arv_data[ ,facility1:=tolower(facility)]
  arv_data[(grepl(pattern="clinic", facility1)) , level:='Clinic'] 
  arv_data[(grepl(pattern="ii", facility1)) & !(grepl(pattern="\\siii", facility1)), level:='HC II'] 
  arv_data[(grepl(pattern="iii", facility1)), level:='HC III'] 
  arv_data[grepl(pattern="\\siv",facility1), level:='HC IV']
  arv_data[grepl(pattern="taso", facility1), level:='TASO']
  arv_data[grepl(pattern="hospital", facility1) | grepl(pattern="regional ref", facility1) , level:='Hospital']
  arv_data[is.na(level), level:='Other']
  arv_data[ ,facility1:=NULL]
  
  # convert Y/Ns to T/F
  arv_data[art=='Y', art_site:=TRUE]
  arv_data[art=='N', art_site:=FALSE]
  arv_data[tests=='Y', test_kits:=TRUE]
  arv_data[tests=='N', test_kits:=FALSE]
  arv_data[arv=='Y', arvs:=TRUE]
  arv_data[arv=='N', arvs:=FALSE]
  
  # create a week variable
  week = strsplit(f, '\\s')[[1]][2]
  if (substr(week, 3, 3)!=')') week = substr(week, 2, 3)
  if (substr(week, 3, 3)==')') week = substr(week, 2, 2)
  arv_data[ , week:=as.numeric(week)]
  
  # add a year variable
  arv_data[ , year:=as.numeric(strsplit(f, '/')[[1]][1])]

  # generate a date 
  arv_data[ , day:=(week*7)-6]
  arv_data[ , date:=strptime(paste(year, day), format="%Y %j")]
  
  # generate a monthly date
  arv_data[ , month:=strsplit(as.character(date), '-')[[1]][2]]
  arv_data[ , month:=as.Date(paste0(year, '-', month, '-01'), format='%Y-%m-%d')]
  
  # drop week and day
  arv_data[ ,c('day', 'tests', 'arv', 'art', 'week'):=NULL]
  
  if(i==1) full_data = arv_data
  if(i>1) full_data = rbind(full_data, arv_data)
  i = i+1
  
}


#---------------------------
# save the output
saveRDS(full_data, paste0(OutDir, 'arv_stockouts_2017_2018.rds'))

#----------------------------


