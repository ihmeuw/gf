# ----------------------------------------------
# Irena Chen
#
# 3/1/2018
# ### Map FPM SDAs to Sicoin $$ by municipality
# ----------------------------------------------
# Set up R
rm(list=ls())
library(rgeos)
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
# ----------------------------------------------

##load the data: 
gtmBudgets <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_fpm_pudr.csv", 
                                  fileEncoding = "latin1"))

sicoin_data <- data.table(read.csv("J:/Project/Evaluation/GF/resource_tracking/gtm/prepped/prepped_sicoin_data.csv"
                                   ,fileEncoding="latin1"))

dept_muni_list <- data.table(read.csv("J:/Project/Evaluation/GF/mapping/gtm/department_municipality_list.csv"
                                   ,fileEncoding="latin1"))

# ----------------------------------------------
##MAP FPM BUDGETS TO SICOIN MUNICIPALITIES: 
gtmBudgets <- gtmBudgets[!(data_source=="pudr"&year>2015)]

gtmBudgets$quarter <- as.yearqtr(gtmBudgets$start_date)

##sum up budget (as "variable") by year, disease, and data source 
byVars = names(gtmBudgets)[names(gtmBudgets)%in%c('quarter', 'disease','year','gf_module', 'gf_intervention')]
gtm_subset = gtmBudgets[, list(budget=sum(na.omit(budget)), expenditure=sum(na.omit(expenditure))
                          , disbursement=sum(na.omit(disbursement))), by=byVars]


# ##order the data to get the % of interventions per module: 
# gtm_subset<- gtm_subset[with(gtm_subset, order(disease,year,quarter, gf_module, gf_intervention, budget)), ]
# gtm_subset[, module_fraction := budget/sum(budget), by=c("disease","quarter")]
# gtm_subset[, int_fraction := budget/sum(budget), by=c("disease","quarter", "gf_module")]


fpm_malaria <- gtm_subset[(disease=="malaria"&year==2012)]
# ----------------------------------------------

##just work with GF data for now: 
sicoin_data <- sicoin_data[source=="gf"]
##change the start dates from factors to dates: 
sicoin_data$start_date <- as.Date(sicoin_data$start_date,"%Y-%m-%d")
gtmBudgets$start_date <- as.Date(gtmBudgets$start_date,"%Y-%m-%d")
sicoin_data$year <- year(sicoin_data$start_date)

##clean the ID numbers from sicoin: 
sicoin_data$id <- as.numeric(lapply(sicoin_data$adm2, function(y) sub('^0+(?=[1-9])', '', y, perl=TRUE)))



##Malaria in this example, but the other diseases work fine: 
sicoin_subset <- sicoin_data[year==2012&disease=="malaria"]

sicoin_subset$quarter <- as.yearqtr(sicoin_subset$start_date)

##sum up budget (as "variable") by year, disease, and data source 
byVars = names(sicoin_subset)[names(sicoin_subset)%in%c('loc_name','module','quarter','year','id')]
sicoin_subset = sicoin_subset[, list(budget=sum(na.omit(budget)), 
                                     disbursement=sum(na.omit(disbursement))), by=byVars]

##you can do this by quarter or year: 
##QTR
sicoin_subset[, qtr_fraction:=budget/sum(budget), by=c("quarter", "module")]


##YEAR:
sicoin_subset[, year_total:=sum(budget), by="year"]
sicoin_subset[, muni_total:=sum(budget), by=c("year", "loc_name")]
sicoin_subset[, year_fraction:=budget/sum(budget), by=c("year", "module")]


##just work with 2013 data for now:  
setnames(sicoin_subset, c("budget", "disbursement"), c("sicoin_budget", "sicoin_disb"))

graphData <- merge(sicoin_subset, fpm_malaria, by=c("year", "quarter"), allow.cartesian=TRUE)

##how many modules are in this year: 
##municipality budget divided by the number of modules this year: 
graphData[,muni_budget:=sum(budget), by=c("year", "loc_name")]


graphData[,muni_budget_qtr:=budget*qtr_fraction]
graphData[,muni_budget_year:=muni_budget*year_fraction]


graphData[, mod_year:=paste(year, ":",gf_module)]
graphData[, int_year:=paste(year, gf_module,":",gf_intervention)]

# ----------------------------------------------

dir <- 'J:/Project/Evaluation/GF/mapping/gtm/'

# load the shapefile
shapeData = shapefile(paste0(dir, 'GTM_munis_only.shp'))

## load the admin1 shape with the projection: 
adminData = shapefile(paste0(dir, 'gtm_region.shp'))

# use the fortify function to convert from spatialpolygonsdataframe to data.frame
# use IDs instead of names
coordinates = data.table(fortify(shapeData, region='Codigo'))
admin_coords <- data.table(fortify(adminData, region='ID_1'))
coordinates$id <- as.numeric(coordinates$id)

# merge on municipality names
names = data.table(shapeData@data)
admin_names <- data.table(adminData@data)
coord_and_names = merge(coordinates, names, by.x='id', by.y='Codigo', allow.cartesian=TRUE)
admin_dataset = merge(admin_coords, admin_names, by.x = 'id', by.y='ID_1', allow.cartesian=TRUE)


##modules:  
colScale <-  scale_fill_gradient2(low='#0606aa', mid='#87eda5', high='#ffa3b2',
                                  na.value = "grey70",space = "Lab", midpoint = 2.5, ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  breaks=c(0,1, 2,3, 4), limits=c(0,5))

##interventions: 
colScale <-  scale_fill_gradient2(low='#0606aa', mid='#87eda5', high='#ffa3b2',
                                  na.value = "grey70",space = "Lab"  ## play around with this to get the gradient 
                                  # that you want, depending on data values 
                                  #midpoint = 0.5, breaks=c(0,0.25, 0.5, 0.75), limits=c(0,1)
)

graphData$muni_budget <- cut(graphData$muni_budget_year/10000, 
                             breaks= c(0,0.1, seq(0.2, 1, by = .1), 2:5, Inf),right = FALSE)

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

names(colors) <- levels(graphData$muni_budget)

# ----------------------------------------------
### if you want:  Get names and id numbers corresponding to administrative areas to plot as labels: 
# gtm_region_centroids <- data.frame(long = coordinates(adminData)[, 1],lat = coordinates(adminData)[, 2])
gtm_region_centroids[, 'ID_1'] <- adminData@data[,'ID_1'] 
gtm_region_centroids[, 'NAME_1'] <-adminData@data[,'NAME_1']
gtm_region_centroids$NAME_1[18] <- "Totonicapán"
gtm_region_centroids$NAME_1[22] <- "Sololá"
gtm_region_centroids$NAME_1[21] <- "Suchitepéquez"
gtm_region_centroids$NAME_1[3] <- "Sacatepéquez"
gtm_region_centroids$NAME_1[1] <- "Quiché"
gtm_region_centroids$NAME_1[7] <- "Petén"

# ----------------------------------------------
gtm_plots <- list()
i = 1
for(k in unique(graphData$mod_year)){
  shapedata <- copy(coord_and_names)
  subdata <- graphData[mod_year==k]
  shapedata$mod_year <- k ## merge coordinate dataset to budget dataset on a common variable
  # merge on the data (all.x=TRUE so the shapefile data doesn't disappear)
  graphdata  <- merge(shapedata, subdata,by=c('mod_year','id'), all.x=TRUE, allow.cartesian=TRUE)
  plot <- (ggplot() + geom_polygon(data=graphdata, aes(x=long, y=lat, group=group, fill=muni_budget)) + 
             coord_equal() + ##so the two shapefiles have the same proportions 
             geom_path() +
             geom_map(map=admin_dataset, data=admin_dataset,
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



pdf("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/NAME HERE.pdf", height=9, width=12)
invisible(lapply(gtm_plots, print))
dev.off()

# ----------------------------------------------
##Roll up to department level: 

malData <- sicoin_data[disease=="malaria"]

admin_names$NAME_1[18] <- "Totonicapán"
admin_names$NAME_1[22] <- "Sololá"
admin_names$NAME_1[21] <- "Suchitepéquez"
admin_names$NAME_1[3] <- "Sacatepéquez"
admin_names$NAME_1[1] <- "Quiché"
admin_names$NAME_1[7] <- "Petén"


##create vector of unwanted characters:
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


malData$municipality <- tolower(malData$loc_name)
malData$municipality <-gsub(paste(remove_chars, collapse="|"), "",malData$municipality)
malData$municipality <- chartr(paste(names(unwanted_array), collapse=''),
                                     paste(unwanted_array, collapse=''),
                              malData$municipality)


department_data <- merge(malData, dept_muni_list, all.x=TRUE, by="municipality", allow.cartesian=TRUE)

##Check for any munis that didn't get mapped: 

dropped_munis <- department_data[is.na(department)]

##sum by department now: 

##sum up budget (as "variable") by year, disease, and data source 
byVars = names(department_data)[names(department_data)%in%c('department','module','year')]
department_data  = department_data[, list(budget=sum(na.omit(budget)), 
                                     disbursement=sum(na.omit(disbursement))), by=byVars]



department_data[, department_fraction:=budget/sum(budget), by=c("year", "module")]







