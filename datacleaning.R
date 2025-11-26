# Name: Christopher Catterick
# Version 3.0
# Desc: Base program to intake and clean data for ECON 3P60 research
#
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fixest)
library(lubridate)
library(canadianmaps)
library(sf)

dwage <- wage_by_industry
dCmaCount <- `3310072201_databaseLoadingData.(2)`
dCanCount <- `3310072201_databaseLoadingData.(1)`
dCSI <- CSI.Data

#Remove unused variables from base dataset and renamed NAICS codes
dwage <- dwage %>%
  mutate(
    Statistics = NULL,
    UOM = NULL,
    UOM_ID = NULL,
    SCALAR_FACTOR = NULL,
    SCALAR_ID = NULL,
    VECTOR = NULL,
    COORDINATE = NULL,
    SYMBOL = NULL,
    TERMINATED = NULL,
    DECIMALS = NULL,
    STATUS = NULL,
  )
dwage <- dwage %>%
rename(NAICS = Main.industry.based.on.the.North.American.Industry.Classification.System..NAICS.)

#Remove unused variables from base dataset  
dCmaCount <- dCmaCount %>%
  mutate(
    Statistics = NULL,
    UOM = NULL,
    UOM_ID = NULL,
    SCALAR_FACTOR = NULL,
    SCALAR_ID = NULL,
    VECTOR = NULL,
    COORDINATE = NULL,
    SYMBOL = NULL,
    TERMINATED = NULL,
    DECIMALS = NULL,
    STATUS = NULL,
    Employment.size = NULL,
    Business.dynamics.measure = NULL
  )


#Remove unused variables from base dataset
dCanCount <- dCanCount %>%
  mutate(
    Statistics = NULL,
    UOM = NULL,
    UOM_ID = NULL,
    SCALAR_FACTOR = NULL,
    SCALAR_ID = NULL,
    VECTOR = NULL,
    COORDINATE = NULL,
    SYMBOL = NULL,
    TERMINATED = NULL,
    DECIMALS = NULL,
    STATUS = NULL,
    Employment.size = NULL,
    Business.dynamics.measure = NULL
  )

dCanCount$REF_DATE <- as.Date(paste0(dCanCount$REF_DATE, "-01")) #Update date format
dCanCount_filtered <- dCanCount %>%
  filter(Industry != "Business sector industries [T004]")


#Modify this code to fit whichever dataset you want to output for, its just easier
#than rewriting over and over
dCmaCount %>%
  group_by(Industry) %>%            
  summarise(
    count = n(),                    
    mean = mean(VALUE, na.rm = TRUE),
    sd = sd(VALUE, na.rm = TRUE),
    min = min(VALUE, na.rm = TRUE),
    max = max(VALUE, na.rm = TRUE),
    median = median(VALUE, na.rm = TRUE),
    .groups = "drop"                 
  )

dCmaCount$REF_DATE <- as.Date(paste0(dCmaCount$REF_DATE, "-01"))#Update date format
dCmaCount_filtered <- dCmaCount %>%
  filter(Industry != "Business sector industries [T004]")

##ShapeFile stuff

csdData <- st_read("Data/CSD.shp")

csdData <- csdData %>%
  mutate(
    CMAUID = NULL,
    CMAPUID = NULL,
    DGUIDP = NULL,
    CMANAME = NULL,
    CMATYPE = NULL,
    LANDAREA = NULL,
    PRUID = NULL,
  )

dwage <- dwage %>%
  left_join(csdData, by ="DGUID")
