# Make visualizations of data

# set up
library(data.table)
library(ggplot2)
library(lubridate)
library(gridExtra)

# read data
DT <- readRDS("merged.RDS")

# convert format into date r understands (the first of each month)
DT$date <- paste("1", DT$V1, sep="-")

# add year variable to the data
DT$year <- dmy(DT$date)
DT$year <- year(DT$year)
DT$year <- as.factor(DT$year)

# Saint-Louis
dtsl <- DT[V2=="SAINT-LOUIS"]

# creating plots

# plot in region Saint-Louis

p1 = ggplot(data=dtsl, aes(x=N, y=N_DHIS2, color = year)) + 
    geom_point() + geom_abline(intercept = 0) + 
    labs(title = "Saint-Louis",
       caption = "Line indicates perfect agreement",
       x="Diagnosed TB (MACEPA)",
       y="Confirmed TB (DHIS2)")

# plot in region Matam

# subset Matam
dtm <- DT[V2=="MATAM"]

p2 = ggplot(data=dtm, aes(x=N, y=N_DHIS2, color = year)) + 
    geom_point() + geom_abline(intercept = 0) + 
    labs(title = "Matam",
         subtitle = "with outlier present",
       caption = "Line indicates perfect agreement",
       x="Diagnosed TB (MACEPA)",
       y="Confirmed TB (DHIS2)")

# plot in region Louga

# subset Louga
dtl <- DT[V2=="LOUGA"]

p3 = ggplot(data=dtl, aes(x=N, y=N_DHIS2, color = year)) + 
  geom_point() + geom_abline(intercept = 0) + 
  labs(title = "Louga",
       caption = "Line indicates perfect agreement",
       x="Diagnosed TB (MACEPA)",
       y="Confirmed TB (DHIS2)")

# remove outlier from MATAM and plot again
dtm2 <- dtm[-c(16)]

p4 = ggplot(data=dtm2, aes(x=N, y=N_DHIS2, color = year)) + 
  geom_point() + geom_abline(intercept = 0) + 
  labs(title = "Matam",
       subtitle = "with 1 outlier removed",
       caption = "Line indicates perfect agreement",
       x="Diagnosed TB (MACEPA)",
       y="Confirmed TB (DHIS2)")

# save output
pdf("DHIS2 MACEPA Comparisons.pdf", height=6, width =6)
p1
p2
p3
p4
dev.off()
