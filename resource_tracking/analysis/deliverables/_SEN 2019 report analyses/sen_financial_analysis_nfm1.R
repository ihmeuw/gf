# Title: 
# file to create a graph of historical absorption in NFM1

# set up
library(data.table)
library(ggplot2)

# load data
data <- fread("C:\\Users\\frc2\\Box Sync\\Global Fund Files\\tableau_data\\absorption_sen.csv")

# subset to appropriate grants
data <- data[grant_period %in% c("2015-2017")]

# re-shape for graphing
data <- data[,lapply(.SD,sum, na.rm=TRUE),by=c("grant", "grant_period", "start_date", "end_date"), .SDcols=c("budget", "expenditure")]

# re-calculate absorption
data[,absorption:=(expenditure/budget)*100]

# add quarter variable
data$quarter <- NA
data$quarter[which(data$start_date=="2015-01-01" & data$end_date=="2015-03-31")] <- 1
data$quarter[which(data$start_date=="2015-04-01" & data$end_date=="2015-06-30")] <- 2
data$quarter[which(data$start_date=="2015-07-01" & data$end_date=="2015-09-30")] <- 3
data$quarter[which(data$start_date=="2015-10-01" & data$end_date=="2015-12-31")] <- 4

data$quarter[which(data$start_date=="2016-01-01" & data$end_date=="2016-03-31")] <- 5
data$quarter[which(data$start_date=="2016-04-01" & data$end_date=="2016-06-30")] <- 6
data$quarter[which(data$start_date=="2016-07-01" & data$end_date=="2016-09-30")] <- 7
data$quarter[which(data$start_date=="2016-10-01" & data$end_date=="2016-12-31")] <- 8

data$quarter[which(data$start_date=="2017-01-01" & data$end_date=="2017-03-31")] <- 9
data$quarter[which(data$start_date=="2017-04-01" & data$end_date=="2017-06-30")] <- 10
data$quarter[which(data$start_date=="2017-07-01" & data$end_date=="2017-09-30")] <- 11
data$quarter[which(data$start_date=="2017-10-01" & data$end_date=="2017-12-31")] <- 12



# for SEN-H-CNLS there are some data missing but start_dates seem to correspond with the quarter
data$quarter[which(data$grant=="SEN-H-CNLS" & data$start_date=="2016-01-01")] <- 5
data$quarter[which(data$grant=="SEN-H-CNLS" & data$start_date=="2016-04-01")] <- 6
data$quarter[which(data$grant=="SEN-H-CNLS" & data$start_date=="2016-07-01")] <- 7
data$quarter[which(data$grant=="SEN-H-CNLS" & data$start_date=="2016-10-01")] <- 8

data$quarter[which(data$grant=="SEN-H-CNLS" & data$start_date=="2017-01-01")] <- 9
data$quarter[which(data$grant=="SEN-H-CNLS" & data$start_date=="2017-04-01")] <- 10
data$quarter[which(data$grant=="SEN-H-CNLS" & data$start_date=="2017-07-01")] <- 11
data$quarter[which(data$grant=="SEN-H-CNLS" & data$start_date=="2017-10-01")] <- 12

# for SEN-M-IntraH there are some data missing but start_dates seem to correspond with the quarter
data$quarter[which(data$grant=="SEN-M-IntraH" & data$start_date=="2016-01-01")] <- 5
data$quarter[which(data$grant=="SEN-M-IntraH" & data$start_date=="2016-04-01")] <- 6
data$quarter[which(data$grant=="SEN-M-IntraH" & data$start_date=="2016-07-01")] <- 7
data$quarter[which(data$grant=="SEN-M-IntraH" & data$start_date=="2016-10-01")] <- 8

data$quarter[which(data$grant=="SEN-M-IntraH" & data$start_date=="2017-01-01")] <- 9
data$quarter[which(data$grant=="SEN-M-IntraH" & data$start_date=="2017-04-01")] <- 10
data$quarter[which(data$grant=="SEN-M-IntraH" & data$start_date=="2017-07-01")] <- 11
data$quarter[which(data$grant=="SEN-M-IntraH" & data$start_date=="2017-10-01")] <- 12

# for SEN-M-PNLP there are some data missing but start_dates seem to correspond with the quarter

data$quarter[which(data$grant=="SEN-M-PNLP" & data$start_date=="2017-01-01")] <- 9
data$quarter[which(data$grant=="SEN-M-PNLP" & data$start_date=="2017-04-01")] <- 10
data$quarter[which(data$grant=="SEN-M-PNLP" & data$start_date=="2017-07-01")] <- 11
data$quarter[which(data$grant=="SEN-M-PNLP" & data$start_date=="2017-10-01")] <- 12

# add in year variable
data$year <- NA
data$year[which(data$quarter>=1 & data$quarter<=4)] <- 2015
data$year[which(data$quarter>=5 & data$quarter<=8)] <- 2016
data$year[which(data$quarter>=9 & data$quarter<=12)] <- 2017


# create figure that will be included in Senegal report
p1 = ggplot(data, aes(x=year, y=absorption, color=grant, group=grant)) + 
  geom_point() + 
  geom_line() + 
  theme_bw(base_size=16)+
  scale_x_continuous(breaks = seq(from=2015, to=2017, by=1))+
  labs(title="Absorption by grant for 2015-17 end of year period", x="Grant year", y="Absorption (%)", color="Grant")

pdf("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_SEN 2019 annual report/report_graph_nfm1.pdf", height=8, width=11)
p1
dev.off()

ggsave("J:/Project/Evaluation/GF/resource_tracking/visualizations/deliverables/_SEN 2019 annual report/report_graph_nfm1.png", plot=p1, height = 8, width = 11)
