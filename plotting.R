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


ggplot() +
  geom_sf(data = FSA, fill = "gray90", color = "black") +
  geom_sf(data = dCSI$FSA.Geometry, fill = "red", color = "black", size = 0.5) +
  geom_sf(data = dCSI$HQ.Geometry, fill = "grey")
  theme_minimal() +
  theme(axis.text = element_blank())
  labs(title = "Map of Global Innovation Cluster Projects")


CSD_provinces <- CSD %>%
  filter(PRUID %in% c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59))

ggplot() +
  geom_sf(data = CSD_provinces, fill = "gray90", color = "black") +
  geom_sf(data = dCSI, fill = "red", color = "black", size = 0.5) +
  theme_minimal() +
  labs(title = "Map of Canadian Innovation Super Cluster Projects")


ggplot(treated, aes(dist, Funding, color = threshold))+
  geom_smooth(data = treated %>% filter(threshold == 0), method = "lm") +
  geom_smooth(data = treated %>% filter(threshold == 1), method = "lm" ) +
  geom_point() #this is a good plot 

dCSI <- dCSI %>%
  mutate(Date.of.Announcement = mdy(Date.of.Announcement))

dCSI_year <- dCSI %>%
  mutate(year = year(Date.of.Announcement)) %>%
  count(year)

ggplot(dCSI_year, aes(x = year, y = n)) +
  geom_line(color = "blue", size = 1.2) +  
  geom_point(color = "black", size = 3) +    
  scale_x_continuous(breaks = dCSI_year$year) + 
  labs(
    x = "Year of Commencement",
    y = "Number of Projects",
    title = "Projects Commenced Over Time"
  ) +
  theme_minimal(base_size = 14) +              
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),       
    panel.grid.major = element_line(color = "grey") 
  )

ggplot(dCSI, aes(y = Date.of.Announcement, x = Funding)) +
  geom_smooth(se = FALSE, methods = "loess") +
  theme_minimal(base_size = 14) +
  labs(
    x= "Funding: in Millions",
    y= "Year",
    title = "Funding by Year"
  )

ggplot(dwage, aes(x = REF_DATE, y = VALUE, color = NAICS, group = NAICS)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Number of Wage Filings",
    y = "Date",
    color = "Industry",
    title = "Wage Filings by Industry Over Time"
  )

ggplot(dCmaCount, aes(x = REF_DATE, y = VALUE, color = Industry, group = Industry)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.3, size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Number of Wage Filings",
    y = "Date",
    color = "Industry",
    title = "Wage Filings by Industry Over Time"
  )


ggplot(dCSI, aes(x = Date.of.Announcement, y = Funding)) +
  geom_point(alpha = 0.5, color = "black") + 
  geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Year",
    y = "Funding (Millions)",
    title = "Funding by Year"
  )



