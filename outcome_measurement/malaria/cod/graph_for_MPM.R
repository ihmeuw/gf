# graph for Caitlin MPM presentation:

dtSevereMalaria <- dt_long[indicator=="newCasesMalariaSevere", .(aggValue = sum(value, na.rm=TRUE)), by= c("date", "subpopulation")]


m <- ggplot(data= dtSevereMalaria, aes(date, aggValue, color = subpopulation, ymin=0)) + 
  geom_point() + geom_line() + theme_bw() + ggtitle(paste0("Time Series Analysis for Severe Malaria in DRC" )) + 
  labs(x= "Date (Month/Year)", y="Number of Cases", color= "Subpopulation") +
  scale_color_manual(labels = c("5 and Older", "Pregnant Women", "Under 5" ), values = c("steelblue4", "palegreen4", "steelblue1"))

print(m)