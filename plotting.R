#Christopher Catterick (7224520)
#ECON 3P60 - Directed Research
# Version 1.0
# Desc: Program to plot data
#

library(tidyverse)
library(ggplot2)
library(dplyr)
library(panelView)
library(canadianmaps)


ggplot(dwage, aes(REF_DATE, VALUE)) +
  labs(x = "Year", y = "Wage Filings") +
  geom_point(color = "red") +
  facet_wrap(~ NAICS, scales = "free" ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dCmaCount_filtered, aes(REF_DATE, VALUE)) +
  labs(x = "Year", y = "Number of Businesses") +
  geom_point(aes(color = factor(GEO), shape = factor(Industry))) +
  facet_wrap(~ Industry, scales = "free") +
  theme_minimal() 

#ggplot for dCanCount by industry
ggplot(dCanCount, aes(REF_DATE, VALUE)) +
  labs(x = "Year", y = "Number of Businesses") +
  geom_line() + 
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-01"))), color = "red") +
  facet_wrap(~ Industry, scales = "free") +
  theme_minimal() 

ggplot(dwage, aes(REF_DATE, VALUE)) +
  labs(x = "Year", y = "Number of Businesses") +
  geom_point(aes(color = factor(GEO), shape = factor(NAICS))) +
  theme_minimal() 

dCSI <- st_as_sf(dCSI, sf_column_name = "Project.Geometry")

ggplot() +
  geom_sf(data = CSD, fill = "gray90", color = "black") +
  geom_sf(data = dCSI, fill = "red", color = "black", size = 0.5) +
  theme_minimal() +
  labs(title = "Map of Canada Census Sub-Divisions")

CSD_provinces <- CSD %>%
  filter(PRUID %in% c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59))

ggplot() +
  geom_sf(data = CSD_provinces, fill = "gray90", color = "black") +
  geom_sf(data = dCSI, fill = "red", color = "black", size = 0.5) +
  theme_minimal() +
  labs(title = "Map of Canadian Innovation Super Cluster Projects")


