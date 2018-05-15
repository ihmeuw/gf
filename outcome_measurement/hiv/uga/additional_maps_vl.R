# maps

# ---------------
# suppression ratio maps

# suppression ratio for all years 
ggplot(coordinates, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  geom_polygon() + 
  geom_path(size=0.01, color="#636363") + 
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() + 
  labs(title="Viral suppression ratios by district, Uganda", subtitle=" August 2014 - February 2018",
       caption="Source: Uganda Viral Load Dashboard", fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-4), plot.subtitle=element_text(vjust=-4), 
        plot.caption=element_text(vjust=6)) + coord_fixed()

# annual suppression ratios
ggplot(coordinates_year, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ratio_colors) + 
  theme_void() +
  labs(title="Viral suppression ratios by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# ---------------
# suppression ratio by sex

# suppression ratios among females
ggplot(coordinates_female, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=ladies) + 
  theme_void() +
  labs(title="Viral suppression ratios among females by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

# suppression ratios among males
ggplot(coordinates_male, aes(x=long, y=lat, group=group, fill=suppression_ratio)) + 
  coord_fixed() +
  geom_polygon() + 
  geom_path(size=0.01) + 
  facet_wrap(~year) +
  scale_fill_gradientn(colors=gents) + 
  theme_void() +
  labs(title="Viral suppression ratios among males by district, Uganda", caption="Source: Uganda Viral Load Dashboard", 
       fill="% virally suppressed") +
  theme(plot.title=element_text(vjust=-1), plot.caption=element_text(vjust=6)) 

