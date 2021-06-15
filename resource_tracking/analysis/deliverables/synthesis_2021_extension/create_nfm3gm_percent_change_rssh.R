# Francisco Rios
# make figures that will be included in the synthesis report
# to show percent change in RSSH funds between funding request and grantmaking

# clear workspace
rm(list=ls())

# set up
library(data.table)
library(ggplot2)
library(readxl)

# Files and directories

user = as.character(Sys.info()[7])

box = paste0("C:/Users/",user,"/Box Sync/Global Fund Files/")
inFile = paste0(box, 'tableau_data/budgetRevisions_with_frBudgets_activityLevel.csv') #IHME data so far only
ehgFile = paste0(box, 'synthesis/2021_extension/data/ehg_rssh_data.csv')
out.path = paste0(box, 'synthesis/2021_extension/figures/')

all_data <- as.data.table(read.csv(file = inFile))
ehg_data <- as.data.table(read.csv(file = ehgFile))

rssh_data <- all_data[rssh==TRUE & grant_period=="2021-2023" & budget_version%in%c("approved", "funding_request20"), .(budget=sum(budget, na.rm=T)), 
                      by=c('loc_name', 'budget_version')]

# cast data wide
ihme_data <- dcast(rssh_data, loc_name ~ budget_version, value.var = "budget")

# add in ehg countries
plot_data <- rbind(ihme_data, ehg_data, fill=TRUE)

# delete extra column
plot_data[,X:=NULL]

# calculate percent change in plot_data
plot_data$difference <- plot_data$approved-plot_data$funding_request20
plot_data$percent_change <- (plot_data$difference/plot_data$funding_request20)*100

# add label that will be added to the figures
plot_data$label <- format(round(plot_data$difference, digits = -3), nsmall = 0, big.mark=",")

# manually correct wrong values for Cambodia
plot_data[loc_name=="Cambodia", percent_change:=-6.7]
plot_data[loc_name=="Cambodia", label:='-482,000']
plot_data[loc_name=="Mozambique", label:='7,700,000']


#  synthesis figure for FR to GM shifts in RSSH
p1 <- ggplot(plot_data, aes(y=percent_change, x=loc_name)) +
  geom_bar(stat = 'identity', color="#FC8D62", fill="#FC8D62") +
  coord_flip()+
  labs(title=paste0("RSSH Change NFM3 FR-GM"),
       y='Percent change',
       x='',
       fill = '')+
  theme_minimal(base_size=20)+
  geom_text(aes(label = label), position = position_stack(vjust = 0.5))

p1
ggsave("RSSH_NFM3_percent_change_frgm.png", plot = p1, path = out.path, width = 7.5, height = 5, units = "in")
