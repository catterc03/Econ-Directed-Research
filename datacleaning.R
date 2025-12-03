#Christopher Catterick (7224520)
#ECON 3P60 - Directed Research
# Version 4.0
# Desc: Base program to intake and clean data for ECON 3P60 research
#
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fixest)
library(lubridate)
library(canadianmaps)
library(sf)
library(fuzzyjoin)
library(rddtools)
library(magrittr)

dwage <- wage_by_industry
dCmaCount <- `3310072201_databaseLoadingData.(2)`
dCanCount <- `3310072201_databaseLoadingData.(1)`
dCSI <- CSIData
shapeContext <- shapes21context

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

#Modify this code to fit whichever dataset you want to output for, its just easier
#than rewriting over and over
# dCmaCount %>%
#   group_by(Industry) %>%            
#   summarise(
#     count = n(),                    
#     mean = mean(VALUE, na.rm = TRUE),
#     sd = sd(VALUE, na.rm = TRUE),
#     min = min(VALUE, na.rm = TRUE),
#     max = max(VALUE, na.rm = TRUE),
#     median = median(VALUE, na.rm = TRUE),
#     .groups = "drop"                 
#   )

dCmaCount$REF_DATE <- as.Date(paste0(dCmaCount$REF_DATE, "-01"))#Update date format

#add cluster HQ locales 
dCSI <- dCSI %>%
  mutate(clusterHQ = case_when(
    Cluster == "Digital Technology Cluster" ~ "Vancouver, BC",
    Cluster == "Protein Industries Cluster" ~ "Regina, SA",
    Cluster == "Advanced Manufacturing Cluster" ~ "Hamilton, ON",
    Cluster == "Ocean Cluster" ~ "St. Johns, NL",
    Cluster == "Scale AI Cluster" ~ "Montreal, QC"
  )) %>%
  rename("Funding" = "Global.Innovation.Clusters.Funding..M.")
#Rename Cities for conformity with shapefiles
dCSI$clusterHQ[dCSI$clusterHQ == "St. Johns, NL"] <- "St. John's"
dCSI$clusterHQ[dCSI$clusterHQ == "Montreal, QC"] <- "Montréal, QC"
dCSI$Project.Location[dCSI$Project.Location == "Montreal, QC"] <- "Montréal, QC"
dCSI$Project.Location[dCSI$Project.Location == "Quebec City, QC"] <- "Québec City, QC"



##ShapeFile stuff

csdData21 <- st_read("Data/Shapefiles21/CSD.shp") #read file

csdData21 <- csdData21 %>% #clean shapefile for irrelevant variables
  mutate(
    CMAUID = NULL,
    CMAPUID = NULL,
    DGUIDP = NULL,
    CMANAME = NULL,
    CMATYPE = NULL,
    LANDAREA = NULL,
    PRUID = NULL,
  )

#left join shape data into dwage
dwage <- dwage %>%
  left_join(csdData21, by ="DGUID")

#Left Join shapefiles given project location 
dCSI <- regex_left_join( dCSI, csdData21,
  by = c("Project.Location" = "CSDNAME")
)

#Left join shapefiles given HQ location
dCSI <- regex_left_join(dCSI, csdData21,
  by = c("clusterHQ" = "CSDNAME")
)


#clean columns and remove duplicates caused by regex join
dCSI <- dCSI %>%
  mutate(
    CSDUID.x = NULL,
    CSDTYPE.x = NULL,
    DGUID.x = NULL,
    CSDNAME.x = NULL,
    CSDUID.y = NULL,
    CSDTYPE.y = NULL,
    DGUID.y = NULL,
    CSDNAME.y = NULL,
  ) %>%
  rename("Project.Geometry" = "geometry.x",
         "HQ.Geometry" = "geometry.y")
dCSI <- dCSI %>%
  distinct(Project.Title.and.Description, .keep_all = TRUE)


#Compute distance variable between project locale and HQ
dCSI <- dCSI %>%
  mutate(dist = as.numeric(
    st_distance(Project.Geometry, HQ.Geometry, by_element = TRUE)
  ))

#write.csv(dCSI, "ProjData_geom.csv") - Write CSV for aggregate dataset. BIG FILE
