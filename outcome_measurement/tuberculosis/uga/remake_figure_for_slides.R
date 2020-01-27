library(data.table)
library(ggplot2)

# Audrey Batzel
# remake a figure for UGA slides (so it isn't blurry)
# 1/24/20


dt = data.table(year = c(2015:2019), all_tb_cases = c(66.2, 65.5, 70.4, 71.5, 74), mdr_tb_cases = c(51.4, 50, 40.8, 53.9, 48.5))
dt = melt.data.table(dt, id.vars = 'year')

g = ggplot(dt, aes(x = year, y = value, color = variable)) + 
  geom_point(size = 5) + 
  geom_line(size = 2) +
  theme_bw() +
  ylim(0, 100) +
  theme(text = element_text(size=22), legend.position = 'bottom', plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Treatment success rate for Tuberculosis (2015 - 2019)',
       color = '', x = '', y = '') +
  scale_color_manual(values = c('yellowgreen', 'skyblue3'), 
                     labels = c(all_tb_cases = 'NTLP - % Treatment success rate, all DS-TB cases', 
                               mdr_tb_cases = 'NTLP - % Treatment success rate, RR/MDR-TB cases')) +
  geom_hline(yintercept = 85, size = 1.25) + 
  geom_text(aes(label = value), vjust = -1, color = 'black', size = 5)
                         

pdf('J:/Project/Evaluation/GF/outcome_measurement/uga/visualizations/TB treatment success figure for AR and slides.pdf', 
    height = 9, width = 14)
print(g)
dev.off()
