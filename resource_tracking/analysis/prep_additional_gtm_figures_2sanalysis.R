additional.plot.1 <- ggplot(graph_module_nfm3[loc_name == country], aes(x = version, y = budget, fill = coding_2s)) + 
  geom_bar(stat = 'identity', position=position_dodge()) +
  facet_grid(rows = 'plot_module', scales = 'free_y') +
  labs(fill = '2S Coding', x = 'Budget Version', y = 'Budget (USD)', title = '2S funding by module, comparing NFM3 funding request to NFM3 award') +
  geom_text(aes(x = version, y = budget, label = paste0('US$',round(budget, 0)), group = coding_2s), 
            hjust = -0.05, vjust = 0.3, position = position_dodge(width = 1), inherit.aes = TRUE)  +
  scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 14) +
  theme(legend.position="bottom")

# save figures
pdf(paste0(box, '2s_data/figures/2S_figure_', country, 'additional.pdf'), height = 12, width = 15)
additional.plot.1
dev.off()

ggplot(graph_module_nfm3[loc_name == country], aes(x = version, y = budget, fill = coding_2s)) + 
  geom_bar(stat = 'identity', position='stack') +
  facet_grid(rows = 'plot_module', scales = 'free_y') +
  labs(fill = '2S Coding', x = 'Budget Version', y = 'Budget (USD)', title = '2S funding by module, comparing NFM3 funding request to NFM3 award') +
  geom_text(aes(x = version, y = budget, label = paste0('US$',round(budget, 0)), group = coding_2s), 
            position = position_stack(vjust=.5), inherit.aes = TRUE)  +
  scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 14) +
  theme(legend.position="bottom")

additional.plot.2 <- ggplot(graph_grant_module[loc_name == 'GTM' & grant=="GTM_H_INCAP"], aes(x = cycle, y = budget/1000000, fill = coding_2s)) + 
  geom_bar(stat = 'identity', position='stack') +
  labs(fill = '2S Coding', x = 'Grant Cycle', y = 'Budget (millions USD)', title = '2S funding by module, comparing NFM2 award to NFM3 award for INCAP grant') +
  facet_grid(rows = 'plot_module', scales = 'free_y') +
  # geom_text(aes(x = cycle, y = budget/1000000, label = paste0('US$',round(budget/1000000, 1), 'million'), group = coding_2s),
  # position = position_stack(vjust = .5), inherit.aes = TRUE)  +
  scale_fill_manual(name = '2S Coding', values = colors) + coord_flip() + theme_bw(base_size = 14) +
  theme(legend.position="bottom")

pdf(paste0(box, '2s_data/figures/2S_figure_', country, '_additional_2.pdf'), height = 12, width = 15)
additional.plot.2
dev.off()

table_brief <- graph_grant_module[loc_name == 'GTM' & grant=="GTM_H_INCAP",.(cycle, plot_module, coding_2s, budget)]

table_for_brief <- dcast(table_brief, plot_module + coding_2s ~ cycle, value.var = 'budget')
setcolorder(table_for_brief, neworder = c('plot_module', 'coding_2s', 'NFM2', 'NFM3'))
write.csv(table_for_brief, file = paste0(box, '2s_data/figures/gtm_nfm2_nfm3_comp_table_for_brief.csv'))
