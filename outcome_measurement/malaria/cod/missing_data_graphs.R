

# make some health zone graphs to show missing data

dt[,date := as.Date(date)]
dates <- data.table(start=c("2010-11-01", "2011-11-01", "2012-11-01", "2013-11-01", "2014-11-01", "2015-11-01", "2016-11-01", "2017-11-01"), 
                    end=c("2010-12-01", "2011-12-01", "2012-12-01", "2013-12-01","2014-12-01", "2015-12-01", "2016-12-01", "2017-12-01"), 
                    group=c(1, 2, 3, 4, 5, 6, 7, 8))

dates[,start := as.Date(start)]
dates[,end := as.Date(end)]


g <- ggplot(dt[health_zone=="kasa vub" & indicator=="newCasesMalariaSevere" & (subpopulation != "pregnantWomen")], aes(date, value, color = subpopulation, ymin=0)) +
  geom_line() + theme_bw() + ggtitle("Kasa Vub: Cases of Severe Malaria")
   #+ geom_rect(data=dates, inherit.aes=FALSE, aes(xmin=dates$start, xmax=dates$end, ymin=min(dt$value), ymax=max(dt$value), group=group), color="transparent", fill="orange", alpha=0.3)

print(g)

