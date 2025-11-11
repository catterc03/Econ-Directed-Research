# Name: Christopher Catterick
# Version 1.0
# Desc: Program to plot data
#

library(tidyverse)
library(ggplot2)
library(dplyr)
library(panelView)


ggplot(dwage, aes(REF_DATE, VALUE)) +
  geom_point(color = "red") +
  facet_wrap(~ NAICS, scales = "free_y" ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#This doesnt work idk why
ggplot(dwage, aes(REF_DATE, VALUE)) +
  geom_line(color = "red") +
  facet_grid(GEO ~ NAICS) +
  theme_minimal()

