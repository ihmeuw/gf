

# run descriptives_uvl.R

#-----------------------------------
pdf(paste0(dir, '/outputs/idrc_slides.pdf'), height=6, width=9)

two <- c('#bd0026', '#3182bd')

# sample validity
graph1 <- uvl[ ,.(samples_received=sum(samples_received), samples_tested=sum(samples_tested), valid_results=sum(valid_results)), by=.(date)]
graph1 <- melt(graph1, id.vars='date')

graph1$variable <- factor(graph1$variable, levels=c('samples_received', 'samples_tested', 'valid_results'), 
                          labels=c('Samples received', 'Samples Tested', 'Valid test results'))

ggplot(graph1, aes(x=date, y=value, col=factor(variable), group=variable)) + 
  geom_point(size=1.5) + 
  geom_line(alpha=0.8) + 
  theme_bw() +
  xlab("Date") + ylab("Count") + 
  labs(title = "Viral load testing", caption="Source: Uganda VL Dashboard", color="") +
  scale_color_manual(values=tri_sex) +
  theme(title = element_text(size=18),
        legend.title = element_text(size=18), 
        legend.text = element_text(size=16))


graph2 <- uvl[ ,.(ratio=100*sum(valid_results)/sum(samples_received)), by=.(date)]

ggplot(graph2, aes(x=date, y=ratio)) + 
  geom_point(size=1.5) + 
  geom_line(alpha=0.8) + 
  theme_bw() +
  xlab("Date") + ylab("%") + 
  labs(title = "Proportion of samples received that resulted in valid test results", caption="Source: Uganda VL Dashboard", color="") +
  scale_color_manual(values=tri_sex) +
  theme(title = element_text(size=18))

#--------------------------------------
# percentage of samples received that resulted in valid test results

ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=ratio_valid)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors, name="% of samples") + 
  theme_void() +
  labs(title="Percentage of samples received that resulted in valid test results", caption="Source: Uganda Viral Load Dashboard") +
  theme(plot.title=element_text(size=18), plot.subtitle=element_text(vjust=-4, size=14), plot.caption=element_text(size=14))   


#---------------------------
# percent virally suppressed
graph3 <- uvl[ ,.(suppressed=sum(suppressed), valid_results=sum(valid_results)), by=.(date, sex)]
graph3 <- melt(graph3, id.vars=c('date', 'sex'))

graph3$variable <- factor(graph3$variable, levels=c('valid_results', 'suppressed'), 
                          labels=c('Valid test results', 'Virally suppressed'))

ggplot(graph3[sex!='Unknown'], aes(x=date, y=value, col=factor(variable), group=variable)) + 
  geom_point(size=1.5) + 
  geom_line(alpha=0.8) + 
  facet_wrap(~sex, scale='free_y') +
  theme_bw() +
  xlab("Date") + ylab("Count") + 
  labs(title = "Valid viral load test results and viral suppression", caption="Source: Uganda VL Dashboard", colour="Sex") +
  scale_color_manual(values=two) +
  theme(title = element_text(size=18),
    legend.title = element_text(size=18), 
         legend.text = element_text(size=16))









dev.off()

graph1[ , ratio:=(100*(suppressed/valid_results))]


ratio_table <- uvl[ ,.(ratio=100*(sum(suppressed)/sum(valid_results))), by=.(date, level, sex)]
ratio_table[is.nan(ratio), ratio:=0]

ggplot(ratio_table, aes(x=date, y=ratio, col=factor(level), group=level)) + 
  geom_point(size=1.5) + 
  geom_line(alpha=0.8) + 
  facet_wrap(~sex) +
  theme_bw() +
  xlab("Date") + ylab("Percent (%)") + 
  labs(title = "Percentage of patients submitting samples who are virally suppressed (August 2014 - June 2018)", 
       caption="Source: Uganda VL Dashboard", colour="Platform") +
  scale_color_manual(values=ladies)

  


