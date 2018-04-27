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

setnames(department_data, c("budget", "disbursement"), c("sicoin_budget", "sicoin_disb"))

graphData <- merge(fpm_malaria, department_data, by="year", allow.cartesian=TRUE)

graphData[,department_budget:=budget*department_fraction]

dep_names_and_coords <- admin_dataset[, c("id", "NAME_1"), with=FALSE]
dep_names_and_coords <- unique(dep_names_and_coords)
setnames(dep_names_and_coords, "NAME_1", "department")

graphData$department <- as.character(graphData$department)

graphData <- merge(graphData, dep_names_and_coords, by="department", allow.cartesian=TRUE)

graphData[, mod_year:=paste(year, ":",gf_module)]
graphData[, int_year:=paste(year, gf_module,":",gf_intervention)]



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


gtm_plots <- list()
i = 1
for(k in unique(graphData$mod_year)){
  shapedata <- copy(admin_dataset)
  subdata <- graphData[mod_year==k]
  shapedata$mod_year <- k ## merge coordinate dataset to budget dataset on a common variable
  # merge on the data (all.x=TRUE so the shapefile data doesn't disappear)
  graphdata  <- merge(shapedata, subdata,by=c('mod_year','id'), all.x=TRUE, allow.cartesian=TRUE)
  plot <- (ggplot() + geom_polygon(data=graphdata, aes(x=long, y=lat, group=group, fill=dept_budget)) + 
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

pdf("J:/Project/Evaluation/GF/resource_tracking/gtm/visualizations/municipality_viz/fpm_sicoin/malaria_gf_modules.pdf", height=9, width=12)
invisible(lapply(gtm_plots, print))
dev.off()


