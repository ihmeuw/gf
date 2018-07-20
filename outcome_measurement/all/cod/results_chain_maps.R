

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(stringr) # to extract meta data from file names
# --------------------

# --------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#--------------------
# Initial cleaning after download
# Import base services data set and convert to a data table

terg1 <- mal_cases[element_id=='nRm30I4w9En' | element_id=='wleambjupW9']
terg1[element_id=='wleambjupW9', category:='Pregnant woman']
terg <- terg1[ ,.(value=sum(value)), by=.(date, category)]


graph_colors <- c('#bd0026', '#3182bd', '#74c476')

# alternate version - pregnant women only
terg1 <- mal_cases[element_id=='wleambjupW9']


#-------------------------

terg_map <- terg1[ ,.(value=(sum(value))), by=.(province)]
setnames(terg_map, c('province', 'value'), c('dps', 'value'))

#upload the shape file


# load the shapefile
shapeData <- shapefile("J:/Project/Evaluation/GF/outcome_measurement/cod/dhis/gadm36_COD_shp/gadm36_COD_1.shp")

# check that shapeData is a spatialpolygonsdataframe
class(shapeData)

# plot the shape file in the base package
plot(shapeData)

# ----------------------------------------------

# ----------------------------------------------
# merge the data with the shape file

# identify the variable that contains district names and codes
shapeData@data %>% as_tibble()
unique(shapeData@data$NAME_1, shapeData@data$GID_1)
length(unique(shapeData@data$NAME_1)) # 112 districts

shape_table <- data.table(shapeData@data)
shape_table[ ,.(unique(NAME_1), unique(GID_1))]

terg_map[dps=='Bas-Uele', GID_1:='COD.1_1']
terg_map[dps=='Equateur' , GID_1:='COD.2_1']
terg_map[dps=='Haut-Katanga', GID_1:='COD.3_1']
terg_map[dps=='Haut-Lomami', GID_1:='COD.4_1']
terg_map[dps=='Haute-Uele', GID_1:='COD.5_1']
terg_map[dps=='Ituri', GID_1:='COD.6_1']

terg_map[dps=='Kasai', GID_1:='COD.9_1'] 
terg_map[dps=='Kasai Oriental', GID_1:='COD.8_1'] 
terg_map[dps=='Kasai Central', GID_1:='COD.7_1'] 
terg_map[dps=='Kinshasa', GID_1:='COD.10_1']
terg_map[dps=='Kongo Central', GID_1:='COD.11_1'] 

terg_map[dps=='Kwango', GID_1:='COD.12_1'] 
terg_map[dps=='Kwilu', GID_1:='COD.13_1'] 

terg_map[dps=='Lomami', GID_1:='COD.14_1'] 
terg_map[dps=='Lualaba', GID_1:='COD.15_1']
terg_map[dps=='Mai-Ndombe', GID_1:='COD.16_1']
terg_map[dps=='Mongala', GID_1:='COD.18_1'] 
terg_map[dps=='Maniema', GID_1:='COD.17_1'] 
terg_map[dps=='Nord-Kivu', GID_1:='COD.19_1'] 
terg_map[dps=='Nord-Ubangi', GID_1:='COD.20_1']

terg_map[dps=='Sankuru', GID_1:='COD.21_1'] 
terg_map[dps=='Sud-Ubangi', GID_1:='COD.22_1'] 
terg_map[dps=='Sud-Kivu', GID_1:='COD.23_1'] 
terg_map[dps=='Tanganyika', GID_1:='COD.24_1']
terg_map[dps=='Tshopo', GID_1:='COD.25_1'] 
terg_map[dps=='Tshuapa', GID_1:='COD.26_1'] 


ids <- terg_map$GID_1
shape_check <- unique(shapeData@data$GID_1)
ids %in% shape_check

terg_map <- terg_map[dps!='Unknown']
terg_map[ ,dps:=NULL]
setnames(terg_map, 'GID_1', 'id')

coordinates <- data.table(fortify(shapeData, region='GID_1')) 
coordinates <- merge(coordinates, terg_map, by="id", all.x=TRUE)





#-------------------------------------------------------------------
ratio_colors <- brewer.pal(8, 'Spectral')
gents <- brewer.pal(6, 'Purples')
preg <- brewer.pal(6, 'Greens')


pdf(paste0(dir, 'terg_slides.pdf'), height=6, width=12)

ggplot(terg, aes(x=date, y=value, color=category, group=category)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title='Confirmed simple malaria cases treated, by age and pregnancy status',
       subtitle='January 2017 - present', 
       caption='Source: Système National d\'Information Sanitaire (DHIS2)',
       y='Count', x='Date', color='Category') +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values=graph_colors) +
  theme(plot.title=element_text(size=22), plot.caption=element_text(size=14),
        plot.subtitle = element_text(size=18), legend.title = element_text(size=16), 
        legend.text = element_text(size=14), axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14), axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) 

# suppression ratio for all years 
ggplot(coordinates, aes(x=long, y=lat, group=group, fill=as.integer(value))) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=gents, labels=scales::comma) + 
  theme_void() + 
  labs(title="Confirmed simple malaria cases treated",
       subtitle=" January 2017 - present",
       caption='Source: Système National d\'Information Sanitaire (DHIS2)', 
       fill="Number of cases treated") +
  theme(plot.title=element_text(vjust=-4, size=22), 
        plot.subtitle=element_text(vjust=-4, size=18), 
        plot.caption=element_text(vjust=6, size=14),
        legend.title = element_text(size=16), 
        legend.text = element_text(size=14)) + 
  coord_fixed()



# suppression ratio for all years 
ggplot(coordinates, aes(x=long, y=lat, group=group, fill=as.integer(value))) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=preg, labels=scales::comma) + 
  theme_void() + 
  labs(title="Confirmed simple malaria cases treated among pregnant women",
       fill="Number of cases treated") +
  theme(plot.title=element_text(vjust=-4, size=22), 
        legend.title = element_text(size=16), 
        legend.text = element_text(size=14)) + 
  coord_fixed()

dev.off()



