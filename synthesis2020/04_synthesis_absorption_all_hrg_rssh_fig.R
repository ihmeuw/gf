#########################
# Creating absorption figures for PCE 2020 Synthesis Report and Slides
# Author: Matt Schneider
# Last updated and ran - 1/28/21
#########################

library(ggplot2)
library(ggpubr)
library(data.table)
library(readxl)
library(RColorBrewer)

user = as.character(Sys.info()[7])

# input file
box = paste0("C:/Users/",user,"/Box Sync/Global Fund Files/")
inFile = paste0(box, 'synthesis/data/merged_consortia_absorption_data.csv')
out.path = paste0(box, 'synthesis/figures/')
UGAinFile = paste0(box, 'synthesis/data/draft_synthesis_absorption_quant.xlsx') #Uganda's reporting for semester 1-2 were off for certain grants and this file makes the correct adjustments
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
dat3$type <- "HRG-Equity"

# bind data frames together
all_data <- rbind(dat1, dat2, dat3, fill=TRUE)
all_data <- all_data[loc_name!="Uganda"]

##slightly adjusted data for Uganda
uga_data <- as.data.table(read_xlsx(path = UGAinFile, sheet = "Absorption by cntry year"))
uga_data <- uga_data[loc_name=="Uganda"]

##making year variables
uga_data[semester=="Semester 1-2",year:=2018]
uga_data[semester=="Semester 3-4",year:=2019]
uga_data[semester=="Semester 5",year:=2020]

uga_data[,cum_budget:=cumsum(budget)]
uga_data[,cum_expend:=cumsum(expenditure)]
uga_data[,type:="All modules"]

uga_equity_data <- as.data.table(read_xlsx(path = UGAinFile, sheet = "HRG-E Absorption by cntry year"))
uga_equity_data <- uga_equity_data[loc_name=="Uganda"]
##making year variables
uga_equity_data[semester=="Semester 1-2",year:=2018]
uga_equity_data[semester=="Semester 3-4",year:=2019]
uga_equity_data[semester=="Semester 5",year:=2020]

uga_equity_data[,cum_budget:=cumsum(budget)]
uga_equity_data[,cum_expend:=cumsum(expenditure)]
uga_equity_data[,type:="HRG-Equity"]

uga_rssh_data <- as.data.table(read_xlsx(path = UGAinFile, sheet = "RSSH Absorption by cntry year"))
uga_rssh_data <- uga_rssh_data[loc_name=="Uganda"]
##making year variables
uga_rssh_data[semester=="Semester 1-2",year:=2018]
uga_rssh_data[semester=="Semester 3-4",year:=2019]
uga_rssh_data[semester=="Semester 5",year:=2020]

uga_rssh_data[,cum_budget:=cumsum(budget)]
uga_rssh_data[,cum_expend:=cumsum(expenditure)]
uga_rssh_data[,type:="RSSH"]

##adding correcated Uganda data back in
all_data <- rbind(all_data, uga_data[,c("loc_name","year","cum_budget","cum_expend","type")],
                  uga_equity_data[,c("loc_name","year","cum_budget","cum_expend","type")],
                  uga_rssh_data[,c("loc_name","year","cum_budget","cum_expend","type")])
all_data[,absorption:=cum_expend/cum_budget]

# subset to the correct years
ehg_data <- all_data[loc_name%in%c('Mozambique', 'Cambodia', 'Myanmar', 'Sudan') & year=="2019"]
ihme_data <- all_data[loc_name%in%c('DRC', 'Senegal', 'Uganda', 'Guatemala') & year=="2020"]

plot_data <- rbind(ehg_data, ihme_data, fill=TRUE)

all_data$type <- factor(all_data$type, 
                         levels = c("All modules","RSSH","HRG-Equity"))

p1 <- ggplot(all_data[absorption!=0], aes(y=absorption, x=as.factor(year), fill=type)) +
  geom_bar(stat = 'identity') +
  coord_flip()+
  labs(title=paste0(""),
       y='Absorption',
       x='',
       caption = "Cumulative data at end of each year of grant implementation. 2020 absorption only for first semester.",
       fill = "") +
  scale_y_continuous(labels = function(x)x*100)+
  facet_grid(loc_name ~ type, switch = "y", scales = "free_y")+
  geom_text(aes(label=round((absorption*100))),nudge_y = 0.05) +
  theme_minimal(base_size=14)+
  scale_fill_brewer(palette = "Set2", direction = -1)+
  theme(legend.position = "none") +
  guides(fill = guide_legend(reverse=TRUE))

all_data[,year:=as.character(year)]

p2a <- ggplot(all_data[absorption!=0 & (loc_name=="Cambodia" | loc_name=="DRC" | loc_name=="Guatemala" | loc_name=="Mozambique")],
              aes(y=absorption, x=year, color=type, group=type)) +
  geom_line(size=1.25) +
  facet_grid(loc_name ~ ., scales = "free_y") +
  labs(title=paste0(""),
       y='Cumulative Absorption',
       x='',
       fill = "") +
  scale_y_continuous(labels = function(x)x*100, limits = c(.05,.9))+
  theme_minimal(base_size=18)+
  theme(legend.position = "none") +
  scale_color_brewer("",palette = "Set2", direction = -1)
  
p2b <- ggplot(all_data[absorption!=0 & (loc_name!="Cambodia" & loc_name!="DRC" & loc_name!="Guatemala" & loc_name!="Mozambique")],
              aes(y=absorption, x=year, color=type, group=type)) +
  geom_line(size=1.25) +
  facet_grid(loc_name ~ ., scales = "free_y") +
  labs(title=paste0(""),
       y='',
       x='',
       caption = "2020 absorption only for first semester.",
       fill = "") +
  scale_y_continuous(labels = function(x)x*100, limits = c(.05,.9))+
  theme_minimal(base_size=18)+
  scale_color_brewer("",palette = "Set2", direction = -1)

p2 <- ggarrange(p2a,p2b)

all_data$type <- factor(all_data$type, 
                        levels = c("HRG-Equity","RSSH","All modules"))

p3 <- ggplot(all_data[absorption!=0], aes(y=absorption, x=as.factor(year), fill = type)) +
  geom_bar(position = "dodge", stat = 'identity') +
  coord_flip()+
  labs(title=paste0(""),
       y='Absorption',
       x='',
       caption = "Cumulative data at end of each year of grant implementation. 2020 absorption only for first semester.",
       fill = "") +
  scale_y_continuous(labels = function(x)x*100)+
  facet_grid(loc_name ~ ., switch = "y", scales = "free_y")+
  geom_text(aes(label=round((absorption*100))),size = 3, hjust = -0.2,
            position = position_dodge(width = 1)) +
  theme_minimal(base_size=14)+
  scale_fill_brewer(palette = "Set2", direction = -1)+
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse=TRUE))

avg <- all_data[,.(cum_budget=sum(cum_budget,na.rm = TRUE), cum_expend=sum(cum_expend,na.rm = TRUE)), by = c("type","year")]
avg[,absorption:=cum_expend/cum_budget]
avg[,loc_name:="All Countries"]

all_data_avg <- rbind(all_data,avg)
all_data_avg$loc_name <- factor(all_data_avg$loc_name, 
                                levels = c("All Countries","Cambodia","DRC",
                                           "Guatemala","Mozambique","Myanmar",
                                           "Senegal","Sudan","Uganda"))

setorder(all_data_avg,loc_name,-type,year)
setcolorder(all_data_avg, neworder = c("loc_name", "type", "year",
                                  "cum_expend","cum_budget","absorption"))

# output files
outDir = paste0(box, 'synthesis/figures/cross_consortia_nfm2_nfm3_comparisons/')
outFilep <- paste0(outDir, '/cc_comparisons_absorption_years.png')
outFilep2 <- paste0(outDir, '/cc_comparisons_absorption_years_lines.png')
outFilepa <- paste0(outDir, '/cc_comparisons_absorption_years_a.png')
outFilepb <- paste0(outDir, '/cc_comparisons_absorption_years_b.png')


outabsTable = paste0(outDir,'/table_absorption_by_year.csv')
write.csv(all_data_avg, outabsTable)

png(outFilep, height = 10, width = 14, units = "in", res = 300)
p1
dev.off()


png(outFilep2, height = 8, width = 14, units = "in", res = 300)
p2
dev.off()


png(outFilepa, height = 6, width = 5, units = "in", res = 300)
p2a
dev.off()

png(outFilepb, height = 6, width = 5, units = "in", res = 300)
p2b
dev.off()