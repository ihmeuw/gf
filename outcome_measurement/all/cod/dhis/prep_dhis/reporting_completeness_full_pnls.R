#--------------------------
# reporting completeness before aggregation 

new = dt[ ,.(facilities=length(unique(org_unit))), by=date]

ggplot(new, aes(x=date, y=facilities)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(y='Facilities reporting', x='Date') +
  theme(axis.title=element_text(size=22), axis.text=element_text(size=18)) 

# describe reporting completeness
dt[ ,length(unique(org_unit))]
dt[ ,range(date)]

dt[ ,.(length(unique(org_unit))/3891), by=level]