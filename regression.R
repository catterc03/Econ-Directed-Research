#Christopher Catterick (7224520)
#ECON 3P60 - Directed Research
#Regression Script V.1
#DESC: Script to complete regressions and output results

library(rdrobust)
library(janitor)

#defining treatment based on whether a project geometry is within the HQ geom or not
#Uses pre calculated distance variable (see datacleaning.R) and assumes 0 to be within

treated <- dCSI %>%
  mutate(threshold = ifelse(distance <= 0, 1, 0)) 
#removing any NA binary assignment
treated <- treated %>%
  drop_na()

#NEED TO DEFINE TREATMENT IN A FUZZY CONTEXT
rdProgram <- rdrobust(
 y = treated$Funding,
 x = treated$distance,
 fuzzy = treated$threshold,
)

summary(rdProgram)

# ggplot(treated, aes(distance, Funding, color = threshold))+
#   geom_smooth(data = treated %>% filter(threshold == 0), method = "lm") +
#   geom_smooth(data = treated %>% filter(threshold == 1), method = "lm" ) +
#   geom_point() #this is a good plot


