# rename some of the labels for spanish graph
View(plot_equity_3)

# change the translation for country names
plot_equity_3$loc_name_sp <- plot_equity_3$loc_name
plot_equity_3[loc_name=="Cambodia", loc_name_sp:="Camboya"]
plot_equity_3[loc_name=="DRC", loc_name_sp:="RDC"]

# translate the labels for country names
plot_equity_3[simple_version=="NFM3 FR", simple_vers_sp:="NFM3 SF"]
plot_equity_3[simple_version=="NFM2 Revision*", simple_vers_sp:="NFM2 Revisión*"]
plot_equity_3[simple_version=="NFM2 Award", simple_vers_sp:="NFM2 Aprobada"]
plot_equity_3[simple_version=="NFM2 FR", simple_vers_sp:="NFM2 SF"]
plot_equity_3[simple_version=="NFM3 Award", simple_vers_sp:="NFM3 Aprobada"]

# translate the labels
plot_equity_3[label=="Human rights related investments", label_sp:="Inversiones relacionadas con los derechos humanos"]
plot_equity_3[label=="KVP related investments", label_sp:="Inversiones relacionadas con PC"]
plot_equity_3[label=="Other equity related investments", label_sp:="Otras intervenciones relacionadas con la equidad"]

# factor the variables
plot_equity_3$simple_vers_sp <- factor(plot_equity_3$simple_vers_sp,
                                       levels=c("NFM3 Aprobada",
                                                "NFM3 SF",
                                                "NFM2 Revisión*",
                                                "NFM2 Aprobada",
                                                "NFM2 SF"))

plot_equity_3$loc_name_sp <- factor(plot_equity_3$loc_name_sp,
                                    levels = c("Camboya", "RDC", "Guatemala", "Mozambique", "Myanmar", "Senegal", "Uganda"))

plot_equity_3[,label_sp:=factor(label_sp, 
                             levels = c("Otras intervenciones relacionadas con la equidad","Inversiones relacionadas con los derechos humanos","Inversiones relacionadas con PC"))]

e_spanish <- ggplot(plot_equity_3, aes(y=budget, x=simple_vers_sp, fill=label_sp)) + 
  geom_bar(stat = 'identity') +
  coord_flip()+
  theme(legend.position = "none") +
  labs(title=paste0("HRG-Equity related investments"),
       y='Presupuesto (Millones)',
       x='Versiones de Presupuesto',
       #caption = "*Revision is the most recent official budget revision. Percentages are HRG-Equity/total grant investments",
       fill = "HRG-Equity Categories") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot", 
        text = element_text(size = 19),
        legend.position = "bottom",legend.title = element_blank()) +
  scale_y_continuous(labels = function(x)x/1000000)+
  scale_fill_brewer(palette = "Dark2")+
  facet_grid(loc_name_sp ~ ., switch = "y",scales = "free_y")+
  geom_text(data=plot_equity_3[label=="Human rights related investments"],
            aes(label=paste0(round(hrg_prop_total*100),'%')),
            y=119200000, size = 6)+
  guides(fill = guide_legend(reverse=TRUE))

outFilee_spanish = paste0(outDir, '/cc_fr_comparisons_equity_modules2_fg14_spanish.png')

png(outFilee_spanish, height = 11, width = 17, units = "in", res = 300)
e_spanish
dev.off()
