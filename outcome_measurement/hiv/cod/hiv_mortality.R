# ----------------------------------------------
# Caitlin O'Brien-Carelli
# Mortality in DRc Figure

# -----------------------
rm(list=ls())
library(lubridate)
library(data.table)
library(ggplot2)
library(RColorBrewer)

# -------------------------

dt = fread("C:/Users/ccarelli/Documents/hiv_deaths_drc.csv")

tri_colors = c('#a50026', '#fdae61', '#abd9e9')

pdf("C:/Users/ccarelli/Documents/hiv_deaths_drc.pdf", width=12, height=6)

ggplot(dt, aes(x=year, y=val, color=sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~measure, scales='free_y') +
  theme_bw() +
  labs(x='Year', y='Count', color='Sex') +
  scale_color_manual(values=tri_colors) +
  scale_y_continuous(labels = scales::comma) +
  theme(text=element_text(size=20))

dev.off()