#Christopher Catterick (7224520)
#ECON 3P60 - Directed Research
# Version 4.0
# Desc: Base program to intake and clean data for ECON 3P60 research
#

library(tidyverse)
library(dplyr)
library(ggplot2)
library(fixest)
library(canadianmaps)
library(sf)
library(fuzzyjoin)
library(rddtools)
library(janitor)


dwage <- wage_by_industry
dCmaCount <- `3310072201_databaseLoadingData.(2)`
dCanCount <- `3310072201_databaseLoadingData.(1)`
dCSI <- CSI.Data
shapeContext <- shapes21context
FSAShape <- st_read("Data/FSAShapefiles/lfsa000b21a_e.shp")
csdData21 <- st_read("Data/Shapefiles21/CSD.shp")
csdData16 <- st_read("Data/Shapefiles16/CSD16.shp")

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

#Enter CSD Shape data
dwage <- regex_left_join(dwage, csdData21,
                         by = c("GEO" = "CSDNAME")
)

dwage <- dwage %>%
  group_by(GEO, REF_DATE, NAICS) %>%
  slice(1) %>%  
  ungroup()

#Commented as this lacks data needed to properly outline
# dwage <- dwage %>%
#   mutate(Cluster = case_when(
#     Industry == "Agriculture, forestry, fishing and hunting [11]" ~ "Ocean Cluster",
#     Industry == "Manufacturing [31-33]" ~ "Advanced Manufacturing Cluster",
#     Industry == "Food manufacturing [311]" ~ "Protein Industries Cluster", #Need Data
#     Industry == "Utilities [22]" ~ "Digital Technologies Cluster",
#   )) %>%
#   mutate(clusterHQ = case_when(
#     Cluster == "Digital Technology Cluster" ~ "Vancouver, BC",
#     Cluster == "Protein Industries Cluster" ~ "Regina, SA",
#     Cluster == "Advanced Manufacturing Cluster" ~ "Hamilton, ON",
#     Cluster == "Ocean Cluster" ~ "St. Johns, NL",
#     Cluster == "Scale AI Cluster" ~ "Montreal, QC"
#   ))

# dwage <- regex_left_join(dwage, csdData21, by = c("clusterHQ" = "CSDNAME")) #need data

# dCmaCount <- dCmaCount %>%
#   drop_na()



#This section cleans and compiles shapedata for the dCmaCount dataset to ensure prep for regression
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
#Enter CSD shape data
dCmaCount <- regex_left_join(dCmaCount, csdData21,
                         by = c("GEO" = "CSDNAME")
)
dCmaCount <- dCmaCount %>%
  group_by(GEO, REF_DATE, Industry) %>%
  slice(1) %>%  
  ungroup()

dCmaCount <- dCmaCount %>%
  mutate(Cluster = case_when(
    Industry == "Agriculture, forestry, fishing and hunting [11]" ~ "Ocean Cluster",
    Industry == "Manufacturing [31-33]" ~ "Advanced Manufacturing Cluster",
    Industry == "Food manufacturing [311]" ~ "Protein Industries Cluster",
    Industry == "Utilities [22]" ~ "Digital Technologies Cluster",
  )) %>%
  mutate(clusterHQ = case_when(
    Cluster == "Digital Technology Cluster" ~ "Vancouver, BC",
    Cluster == "Protein Industries Cluster" ~ "Regina, SA",
    Cluster == "Advanced Manufacturing Cluster" ~ "Hamilton, ON",
    Cluster == "Ocean Cluster" ~ "St. Johns, NL",
    Cluster == "Scale AI Cluster" ~ "Montreal, QC"
  ))

dCmaCount <- regex_left_join(dCmaCount, csdData21, by = c("clusterHQ" = "CSDNAME"))

dCmaCount <- dCmaCount %>%
  drop_na()

#Reclean
dCmaCount <- dCmaCount %>%
  mutate(
  DGUID.x = NULL,
  DGUID.y = NULL,
  CSDUID.x = NULL,
  CSDUID.y = NULL,
  CSDNAME.x = NULL,
  CSDNAME.y = NULL,
  CSDTYPE.y = NULL,
  CSDTYPE.x = NULL,
) %>%
  rename(
    Geometry  = geometry.x,
    HQ.Geometry = geometry.y
  )

cmaDist = st_distance(dCmaCount$Geometry, dCmaCount$HQ.Geometry,  by_element = TRUE)

dCmaCount <- dCmaCount %>%
  mutate(
    dist = cmaDist
  )

##ShapeFile stuff

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

# The following section cleans and creates necessary variables for the dCSI datasets
# Which includes program data for the GIC program

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

#Left Join shapefiles given project location 
dCSI <- regex_left_join( dCSI, csdData21,
  by = c("Project.Location" = "CSDNAME")
)

#Left join shapefiles given HQ location
dCSI <- regex_left_join(dCSI, csdData21,
  by = c("clusterHQ" = "CSDNAME")
)

dCSI <- dCSI %>%
  mutate(FSA = toupper(substr(gsub(" ", "", Postal.Code), 1, 3))) #For consistent merging FSA Shapefiles

#Left join FSA Shapefiles by postal code
dCSI <- regex_left_join( dCSI, FSAShape,
  by = c("FSA" = "CFSAUID")
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
    CFSAUID = NULL,
    DGUID = NULL,
    PRUID = NULL,
    PRNAME = NULL,
    LANDAREA = NULL,
  ) %>%
  rename("Project.Geometry" = "geometry.x",
         "HQ.Geometry" = "geometry.y",
         "FSA.Geometry" = "geometry")

#Clean duplicates
dCSI <- dCSI %>%
  distinct(Project.Title.and.Description, .keep_all = TRUE)

#Calculate distances and assign signs to ensure variance on both sides of the threshold
FSACentroid <- st_centroid(dCSI$FSA.Geometry)
DistanceToBorder <- st_distance(FSACentroid, st_boundary(dCSI$HQ.Geometry),by_element = TRUE)
insideCheck <- st_within(FSACentroid, dCSI$HQ.Geometry, sparse = FALSE)[,1]

#Assign negative values to distances within the border
dCSI <- dCSI %>%
  mutate(distance = ifelse(insideCheck, -DistanceToBorder, DistanceToBorder)
    )

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

