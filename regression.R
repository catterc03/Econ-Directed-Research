#Christopher Catterick (7224520)
#ECON 3P60 - Directed Research
#Regression Script V.1
#DESC: Script to complete regressions and output results

library(rdrobust)

#defining treatment based on whether a project geometry is within the HQ geom or not
#Uses pre calculated distance variable (see datacleaning.R) and assumes 0 to be within
treated <- dCSI %>%
  mutate(threshold = ifelse(dist <= 0, 1,0)) 
#removing any NA binary assignment
treated <- treated %>%
  drop_na()

#NEED TO DEFINE TREATMENT IN A FUZZY CONTEXT
rd <- rdrobust(
 y = treated$Funding,
 x = treated$dist,
 # c = RD CUTOFF - NEEDS TO BE EDGE OF CSD IN QUESTION ?
 # fuzzy = treated$threshold
 
)

summary(rd)

