# Name: Christopher Catterick
# Version 1.0
# Desc: Base program to intake and clean data for ECON 3P60 research
#
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fixest)
library(lubridate)

dwage <- wage_by_industry
dCmaCount <- `3310072201_databaseLoadingData.(2)`
dCanCount <- `3310072201_databaseLoadingData.(1)`

dwage <- dwage %>%
  mutate(
    DGUID = NULL,
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
  
dCmaCount <- dCmaCount %>%
  mutate(
    DGUID = NULL,
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
dCanCount <- dCanCount %>%
  mutate(
    DGUID = NULL,
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

desc(dwage)


