# Synthesis figure: scatterplot of revisions and absorption

rm(list=ls())

# set up
library(data.table)
library(ggplot2)
library(RColorBrewer)

# establish directories
# input file
box = paste0("C:/Users/frc2/Box Sync/Global Fund Files/synthesis/")
absorption_data = paste0(box, 'data/merged_consortia_absorption_data.csv')
revision_data = paste0(box, 'data/cc_revisions_data_forplots.csv')
out.path = paste0(box, "figures/")
#######################################################
# reshape absorption data for plotting
#######################################################
data <- as.data.table(read.csv(absorption_data)) # cumulative IHME absorption data

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
all_data[,absorption:=cum_expend/cum_budget]

##### keep only 2019 data for the first set of graphs

abs_2019 <- all_data[year=="2019"]

abs_2019 <- abs_2019[,.(loc_name, cum_budget, cum_expend, type, absorption)]

#################################################
# reshape budget revision data for plotting
#################################################
dt <- as.data.table(read.csv(revision_data))
dt <- dt[,.(loc_name, type, nfm2_approved, nfm2_most_recent_revision, difference, percent_change)]

################################
##### Merge data together
###############################
plot_data <- merge(abs_2019, dt, by=c('loc_name', 'type'), all=TRUE)

plot_data$type <- factor(plot_data$type, 
                        levels = c("HRG-Equity", "RSSH", "All modules"))

#####
#### plot
####
p1 <- ggplot(plot_data, aes(y=percent_change, x=absorption, color=type, shape=type))+
  geom_point(show.legend = FALSE)+
  labs(x="Cumulative absorption (end of 2019)",
       y="Percent change (during NFM2)",
       title="",
       caption="")+
  guides(color=guide_legend(title="Type of funds", reverse = TRUE))+
  theme_minimal(base_size = 16)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_brewer(palette="Set2")
p1  
  
ggsave("scatterplot_percent_change_revisions_2019absorption.png", path = out.path, plot=p1, width = 9.5, height=9, units = "in")

p2 <- ggplot(plot_data, aes(y=difference, x=absorption, color=type, shape=type))+
  geom_point(show.legend = FALSE)+
  labs(x="Cumulative absorption (end of 2019)",
       y="Absolute change (during NFM2) in Millions",
       title="",
       caption="")+
  guides(color=guide_legend(title="Type of funds", reverse = TRUE))+
  theme_minimal(base_size = 16)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_brewer(palette="Set2")+
  scale_y_continuous(labels = function(x)x/10^6)
p2  

ggsave("scatterplot_absolute_change_revisions_2019absorption.png", path = out.path, plot=p2, width = 9.5, height=9, units = "in")


#########
#reshape data to include the first year
abs_2018 <- all_data[year=="2018"]

abs_2018 <- abs_2018[,.(loc_name, cum_budget, cum_expend, type, absorption)]


################################
##### Merge data together using 2018 valuess
###############################
plot_data2 <- merge(abs_2018, dt, by=c('loc_name', 'type'), all=TRUE)

plot_data2$type <- factor(plot_data2$type, 
                         levels = c("HRG-Equity", "RSSH", "All modules"))

p3 <- ggplot(plot_data2, aes(y=percent_change, x=absorption, color=type, shape=type))+
  geom_point(show.legend = FALSE)+
  labs(x="Cumulative absorption (end of 2018)",
       y="Percent change (during NFM2)",
       title="",
       caption="")+
  guides(color=guide_legend(title="Type of funds", reverse = TRUE))+
  theme_minimal(base_size = 16)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_brewer(palette="Set2")
p3  

ggsave("scatterplot_percent_change_revisions_2018absorption.png", path = out.path, plot=p3, width = 9.5, height=9, units = "in")



p4 <- ggplot(plot_data2, aes(y=difference, x=absorption, color=type, shape=type))+
  geom_point(show.legend = FALSE)+
  labs(x="Cumulative absorption (end of 2018)",
       y="Absolute change (during NFM2) in Millions",
       title="",
       caption="")+
  guides(color=guide_legend(title="Type of funds", reverse = TRUE))+
  theme_minimal(base_size = 16)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_brewer(palette="Set2")+
  scale_y_continuous(labels = function(x)x/10^6)
p4  

ggsave("scatterplot_absolute_change_revisions_2018absorption.png", path = out.path, plot=p4, width = 9.5, height=9, units = "in")

#######################################
###### Save new data- plot_data3
####################################

#reshape data to include the first year and second year
abs_dif <- all_data[loc_name!="Guatemala"]
abs_dif <- abs_dif[year!=2020]
abs_diff_gtm <- all_data[loc_name=="Guatemala"]
abs_diff_gtm[year==2019, year:=2018]
abs_diff_gtm[year==2020, year:=2019]

# rbind the two dataframes together
abs_diff <- rbind(abs_dif,abs_diff_gtm, fill=TRUE)

# reshape data wide
abs_diff_wide <- dcast(abs_diff, loc_name + type ~ year, value.var = c("cum_budget", "cum_expend", "absorption"))

# calculate new variable: difference between 2018 and 2019 absorption
abs_diff_wide$absorption_diff <- abs_diff_wide$absorption_2019-abs_diff_wide$absorption_2018
abs_diff_wide$absorption_pctchange <- abs_diff_wide$absorption_diff/abs_diff_wide$absorption_2018
  
# subset columns
abs_diff_wide <- abs_diff_wide[,.(loc_name, type, absorption_diff, absorption_pctchange)]

# merge absorption data with revisions data
plot_data3 <- merge(abs_diff_wide, dt, by=c('loc_name', 'type'), all=TRUE)

plot_data3$type <- factor(plot_data3$type, 
                         levels = c("HRG-Equity", "RSSH", "All modules"))

p5 <- ggplot(plot_data3, aes(y=difference, x=absorption_pctchange, color=type, shape=type))+
  geom_point(show.legend = FALSE)+
  labs(x="Percent change in absorption between Year 1 and Year 2",
       y="Absolute change (during NFM2) in Millions",
       title="",
       caption="")+
  guides(color=guide_legend(title="Type of funds", reverse = TRUE))+
  theme_minimal(base_size = 16)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_brewer(palette="Set2")+
  scale_y_continuous(labels = function(x)x/10^6)
p5 

ggsave("scatterplot_absolute_change_revisions_2018-2019_absorption_pctchange.png", path = out.path, plot=p5, width = 9.5, height=9, units = "in")

p6 <- ggplot(plot_data3, aes(y=difference, x=absorption_diff, color=type, shape=type))+
  geom_point(show.legend = FALSE)+
  labs(x="Difference in absorption between Year 1 and Year 2",
       y="Absolute change (during NFM2) in Millions",
       title="",
       caption="")+
  guides(color=guide_legend(title="Type of funds", reverse = TRUE))+
  theme_minimal(base_size = 16)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_brewer(palette="Set2")+
  scale_y_continuous(labels = function(x)x/10^6)
p6

ggsave("scatterplot_absolute_change_revisions_2018-2019_absorption_abs_difference.png", path = out.path, plot=p6, width = 9.5, height=9, units = "in")


plot_data3$loc_name <- factor(plot_data3$loc_name)

p7 <- ggplot(plot_data3, aes(y=percent_change, x=absorption_diff, color=type, shape=loc_name))+
  geom_point(size=5)+
  scale_shape_manual(values = 1:nlevels(plot_data3$loc_name))+
  labs(x="Difference in absorption pct between Year 1 and Year 2",
       y="Percent change (during NFM2)",
       title="",
       caption="")+
  guides(color=guide_legend(title="Type of funds", reverse = TRUE),
         shape=guide_legend(title="Country"))+
  theme_minimal(base_size = 16)+
  geom_smooth(aes(group=type), method=lm, se=FALSE, fullrange=TRUE)+
  scale_color_brewer(palette="Set2")
p7

ggsave("scatterplot_percent_change_revisions_2018-2019_absorption_abs_difference.png", path = out.path, plot=p7, width = 9.5, height=9, units = "in")


