# ----------------------------------------------
# Irena Chen
#
# 3/1/2018
# ### Map FPM SDAs to Sicoin $$ by municipality
# ----------------------------------------------
# Set up R
rm(list=ls())
library(readxl)
library(rgeos)
library(lubridate)
library(raster)
library(ggplot2)
library(maptools)
library(tools)
library(data.table)
library(lubridate)
library(grDevices)
library(RColorBrewer)
library(reshape)
library(scales)
library(ggrepel)
library(dplyr)
library(zoo)
library(stringr)
# ----------------------------------------------
##########Set the directory to be the mapping folder in the J Drive ########
# ----------------------------------------------

dir <- 'J:/Project/Evaluation/GF/mapping/gtm/'

# ----------------------------------------------
######## Load the FPM and SICOIN datasets ########
# ----------------------------------------------
fpm_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_fpm_pudr.csv", 
                                  fileEncoding = "latin1"))

sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv",
                                   fileEncoding="latin1"))
sicoin_data$start_date <- as.Date(sicoin_data$start_date,"%Y-%m-%d")
sicoin_data$year <- year(sicoin_data$start_date)

fpm_data$start_date <- as.Date(fpm_malaria$start_date,"%Y-%m-%d")
fpm_data$year <- year(fpm_malaria$start_date)

# ----------------------------------------------
####### Subset the data by disease of choice ########
# ----------------------------------------------

# Using malaria, but can also do TB or HIV if you want
sicoin_malaria <- sicoin_data[disease=="malaria"&municipality!="gtm"] ##drop where we only have national data
fpm_malaria <- fpm_data[disease=="malaria"]

##sum the fpm data by year, module, and intervention
byVars = names(fpm_malaria)[names(fpm_malaria)%in%c('year', 'gf_module', 'gf_intervention', 'disease')]
fpm_malaria = fpm_malaria[, list(budget=sum(na.omit(budget))), by=byVars]

# ----------------------------------------------
######## Roll up the SICOIN data to department level ########
# ----------------------------------------------
dept_muni_list <- data.table(read_excel("J:/Project/Evaluation/GF/mapping/gtm/Codigos censales Guatemala  actualizado.xls", 
                                        sheet = "Poblacion_INE_94"))

dept_codes <- data.table(read_excel("J:/Project/Evaluation/GF/mapping/gtm/Codigos censales Guatemala  actualizado.xls", 
                                        sheet = "department_codes"))

dept_muni_list <- dept_muni_list[,c("department", "municipality"), with=FALSE]
dept_muni_list <- unique(dept_muni_list)
dept_muni_list$department <- tolower(dept_muni_list$department)

##Capitlize every separate word (e.g. San Jose)
dept_muni_list$department <- str_to_title(dept_muni_list$department)

# ----------------------------------------------
##to make the matching easier, get rid of accents/special characters:
# ----------------------------------------------
unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

remove_chars <- c(" ","rssh","hss", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                  , "[[:punct:]]", "[^[:alnum:]]","\"", ",")
dept_muni_list$municipality<- chartr(paste(names(unwanted_array), collapse=''),
                                     paste(unwanted_array, collapse=''),
                                     dept_muni_list$municipality)

dept_muni_list$municipality <-gsub(paste(remove_chars, collapse="|"), "",dept_muni_list$municipality)
dept_muni_list$municipality <- tolower(dept_muni_list$municipality)


sicoin_malaria$municipality <- tolower(sicoin_malaria$loc_name)
sicoin_malaria$municipality <-gsub(paste(remove_chars, collapse="|"), "",sicoin_malaria$municipality)
sicoin_malaria$municipality <- chartr(paste(names(unwanted_array), collapse=''),
                               paste(unwanted_array, collapse=''),
                               sicoin_malaria$municipality)

# ----------------------------------------------
### Attach the department to the SICOIN dataset
# ----------------------------------------------

department_data <- merge(sicoin_malaria, dept_muni_list, all.x=TRUE, by="municipality", allow.cartesian=TRUE)

##Check for any munis that didn't get mapped: 
# dropped_munis <- department_data[is.na(department)]

##sum by department now: 
##sum up budget and disb. by year, disease, and data source 
byVars = names(department_data)[names(department_data)%in%c('department','module','year')]
department_data  = department_data[, list(budget=sum(na.omit(budget)), 
                                          disbursement=sum(na.omit(disbursement))), by=byVars]

##attach the codes to the department - need codes later for shapefiles
department_data <- merge(department_data, dept_codes, by="department", all.x=TRUE, allow.cartesian=TRUE)

##Check for any dropped departments
#dropped_depts <- department_data[is.na(dept_code)]


##get the % of sicoin money for each department by module and year: 
department_data[, department_fraction:=budget/sum(budget), by=c("year", "module")]
##change the variable names so we can merge the FPM and sicoin
setnames(department_data, c("budget", "disbursement"), c("sicoin_budget", "sicoin_disb"))


# ----------------------------------------------
######## This part is where we can merge the FPM and SICOIN data ########
# ----------------------------------------------

##you can join on quarter, but I'm doing year here: 
graphData <- merge(fpm_malaria, department_data, by="year", allow.cartesian=TRUE)

##multiply the % of sicoin money for each department with the $$ for each FPM intervention
graphData[,department_budget:=budget*department_fraction]

##create a concatenation of years and modules/interventions for the graph titles: 
graphData[, mod_year:=paste(year, ":",gf_module)]
graphData[, int_year:=paste(year, gf_module,":",gf_intervention)]
setnames(graphData, "dept_code", "id")

graphData$dept_budget <- cut(graphData$department_budget/100000, 
                             breaks= c(seq(0, 3, by=0.5),4:10, Inf),right = FALSE)
colors <- c( '#1F3AC7',
             '#235BCD',
             '#287CD3',
             '#2D9ED9',
             '#32BFDF',
             '#37E1E6',
             '#fdbf6f',
             '#ff7f00',
             '#cab2d6',
             '#9e82ba', #dark lilac
             '#93b500',##cherry flavoring
             '#d1cd06', ##neutralized chartreuse 
             '#af445b', 
             '#ff447c'
             
)
names(colors) <- levels(graphData$dept_budget)

# ----------------------------------------------
######## Load the department level shapefile ########
# ----------------------------------------------
adminData = shapefile(paste0(dir,'gtm_region.shp'))
coords <- data.table(fortify(adminData, region='ID_1'))
names <- data.table(adminData@data)
coord_and_names = merge(coords, names, by.x = 'id', by.y='ID_1', allow.cartesian=TRUE)
coord_and_names$id <- as.numeric(coord_and_names$id)

# ----------------------------------------------
### optional: Plot the names of the departments on the map: 
# ----------------------------------------------
gtm_region_centroids <- data.frame(long = coordinates(adminData)[, 1],lat = coordinates(adminData)[, 2])
gtm_region_centroids[, 'ID_1'] <- adminData@data[,'ID_1'] 
gtm_region_centroids[, 'NAME_1'] <-adminData@data[,'NAME_1']
gtm_region_centroids$NAME_1[18] <- "Totonicapán"
gtm_region_centroids$NAME_1[22] <- "Sololá"
gtm_region_centroids$NAME_1[21] <- "Suchitepéquez"
gtm_region_centroids$NAME_1[3] <- "Sacatepéquez"
gtm_region_centroids$NAME_1[1] <- "Quiché"
gtm_region_centroids$NAME_1[7] <- "Petén"

# ----------------------------------------------
######## Use ggplot to make the visualizations ########
# ----------------------------------------------
gtm_plots <- list()
i = 1
for(k in unique(graphData$mod_year)){
  shapedata <- copy(coord_and_names)
  subdata <- graphData[mod_year==k]
  shapedata$mod_year <- k ## merge coordinate dataset to budget dataset on a common variable
  # merge on the data (all.x=TRUE so the shapefile data doesn't disappear)
  graphdata  <- merge(shapedata, subdata,by=c('mod_year','id'), all.x=TRUE, allow.cartesian=TRUE)
  plot <- (ggplot() + geom_polygon(data=graphdata, aes(x=long, y=lat, group=group, fill=dept_budget)) + 
             coord_equal() + ##so the two shapefiles have the same proportions 
             geom_path() +
             geom_map(map=coord_and_names, data=coord_and_names,
                      aes(map_id=id,group=group), size=1, color="#4b2e83", alpha=0) + 
             # geom_polygon(data=admin_dataset, aes(x=long, y=lat, group=group), color="red", alpha=0) + 
             theme_void() +  
             scale_fill_manual(name="Budget (USD, 100k)", values=colors,na.value="grey50" ) + 
             ## uncomment if you want the department names: 
             geom_label_repel(data = gtm_region_centroids, aes(label = NAME_1, x = long, y = lat, group = NAME_1), 
                              size = 3, fontface = 'bold', color = 'black',
                              box.padding = 0.35, point.padding = 0.3,
                              segment.color = 'grey50', nudge_x = 0.7, nudge_y = 4.5) + 
             labs(title=k, fill='USD (hundred thousands)'))
  gtm_plots[[i]] <- plot
  i=i+1
}

##export as pdf 
pdf("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/municipality_viz/fpm_sicoin/malaria_gf_modules.pdf", height=9, width=12)
invisible(lapply(gtm_plots, print))
dev.off()


