#Christopher Catterick (7224520)
#ECON 3P60 - Directed Research
#Regression Script V.1
#DESC: Script to complete regressions and output results

library(rdrobust)

lm_dist <- dCSI %>%
  mutate(threshold = ifelse(dist >= 1378000, 1, 0))

initlm <- lm(dCSI$Funding ~ threshold + I(dist - 1378000), data = lm_dist) #regress funding on distance

#Non-functioning
wagelm <- lm(dwage$VALUE ~ threshold + I(dist - 1378000), data = lm_dist)
busicountcanlm <- lm(dCanCount$VALUE ~ threshold + I(dist - 1378000), data = lm_dist)
busicountcmalm <- lm(dCmaCount$VALUE ~ threshold + I(dist - 1378000), data = lm_dist)

summary(initlm)

#with rdrobust

rdrobinit <- rdrobust(
 y = dCSI$Funding,
 x = dCSI$dist,
 c = 1378000,
 fuzzy = lm_dist$threshold
)

summary(rdrobinit)

rdplot(y = dCSI$Funding, x = dCSI$dist, c = 1378000,
       x.label = "Distance", y.label = "Funding",
       title = "RDD: Funding vs Distance")