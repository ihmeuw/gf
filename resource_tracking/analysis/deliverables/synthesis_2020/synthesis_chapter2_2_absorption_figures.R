#########################
# set up
#########################

library(data.table)
library(readxl)
library(RColorBrewer)
library(ggplot2)

# input file
box = paste0("C:/Users/frc2/Box Sync/Global Fund Files/")
inFile = paste0(box, 'synthesis/data/merged_consortia_absorption_data.csv')
out.path = paste0(box, 'synthesis/figures/')

#######################################################
# read in IHME and EGH data
#######################################################
data <- as.data.table(read.csv(inFile)) # cumulative IHME absorption data

# calculate all country absorption for end of 2019
dat1 <- data[,.(cum_budget=sum(cum_budget, na.rm=TRUE), cum_expend=sum(cum_expend, na.rm=TRUE)), by=c('loc_name', 'year')]
dat1$type <- "All modules"

# calculate all RSSH abosrption for end of 2019
dat2 <- data[rssh==TRUE]
dat2 <- dat2[,.(cum_budget=sum(cum_budget, na.rm=TRUE), cum_expend=sum(cum_expend, na.rm=TRUE)), by=c('loc_name', 'year')]
dat2$type <- "RSSH"

# calculate all equity absorption for end of 2019
dat3 <- data[equity==TRUE]
dat3 <- dat3[,.(cum_budget=sum(cum_budget, na.rm=TRUE), cum_expend=sum(cum_expend, na.rm = TRUE)), by=c('loc_name', 'year')]
dat3$type <- "HRG/Equity"

# bind data frames together
all_data <- rbind(dat1, dat2, dat3, fill=TRUE)
all_data[,absorption:=cum_expend/cum_budget]

# subset to the correct years
ehg_data <- all_data[loc_name%in%c('Mozambique', 'Cambodia', 'Myanmar', 'Sudan') & year=="2019"]
ihme_data <- all_data[loc_name%in%c('DRC', 'Senegal', 'Uganda', 'Guatemala') & year=="2020"]

plot_data <- rbind(ehg_data, ihme_data, fill=TRUE)

plot_data$type <- factor(plot_data$type, 
                        levels = c("HRG/Equity", "RSSH", "All modules"))

# make synthesis graphic
p1 <- ggplot(plot_data, aes(y=absorption, x=type, fill=type)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0(""),
       y='Absorption',
       x='',
       caption = "Cumulative data through 2019. \n DRC, Senegal, Uganda, and Guatemala data through first semester of 2020.",
       fill = "") +
  scale_y_continuous(labels = function(x)x*100)+
  facet_grid(loc_name ~ ., switch = "y")+
  theme_minimal(base_size=14)+
  theme(axis.text.y=element_blank())+
  scale_fill_brewer(palette = "Set2")+
  guides(fill = guide_legend(reverse=TRUE))

p1
ggsave("cumulative_absorption_synthesis_figure.png", path = out.path, plot = p1, width = 10, height = 9, units = "in")

setcolorder(plot_data, neworder = c('loc_name', 'type', 'year', 'cum_budget', 'cum_expend', 'absorption'))
plot_data <- plot_data[order(loc_name, year, -type)]
plot_data$absorption <- plot_data$absorption*100

# save table to include as an annex at the end of the report

# create overall summary values
