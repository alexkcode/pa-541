###############################################
#
# Advanced Data Analysis 1 - Spring 2021
# Week 4- Multiple Regression - Analysis and Estimation
#
###############################################

#Not much to today's script...more about interpretation and understanding

###SET YOUR WD

#load your library
library("tidyverse")

#load the data
load("prestige.RData")

prest = as_tibble(prest)

prest

# This data frame contains the following columns:
#   
# education -Average education of occupational incumbents, years, in 1971.
# income - Average income of incumbents, dollars, in 1971.
# women - Percentage of incumbents who are women.
# prestige -  Pineo-Porter prestige score for occupation, from a social survey conducted in the mid-1960s.
# census - Canadian Census occupational code.
# type - Type of occupation. A factor with levels (note: out of order): bc, Blue Collar; prof, Professional, Managerial, and Technical; wc, White Collar.

###########################################################
lm1 = lm(prestige ~ education, data=prest)
summary(lm1)

lm2 = lm(prestige ~ education + women, data=prest)
summary(lm2)

lm3 = lm(prestige ~ education + income, data=prest)
summary(lm3)

lm4 = lm(prestige ~ education + income + women, data=prest)
summary(lm4)

ed_lm <- lm(education ~ income + women, data=prest)
summary(ed_lm)
resid_lm <- lm(prestige ~ resid(ed_lm), data=prest)
summary(resid_lm)

#------Include an irrelevant variable
#to show a completely irrelevant variable we can draw one
#from a random distribution.  we simply need the number of
#random draws to match the number of observations in our dataset
#we can then simply assign the new value to our dataset.

set.seed(1827)
prest$rand.var = rnorm(102)

lm4 = lm(prestige ~ education + income + rand.var, data=prest)
summary(lm4)
#compare to original results
summary(lm3)

#--------Excluding a relevant variable

lm.true = lm(prestige ~ education + income, data=prest)
summary(lm.true)

lm.omitted = lm(prestige ~ education, data=prest)
summary(lm.omitted)

cor.test(prest$education, prest$income)

