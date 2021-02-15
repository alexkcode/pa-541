#set working directory
setwd("~/Desktop/UIC/PhD/Course Material/Spring 2021/TA - PA 541/Optional Lab 2")

#load libraries
library(haven)
library(tidyverse)

#we will use dataset "cell". Description of variables is as follows:

# Year (Year)
# State (State name)
# state_numeric (numeric representation of state)
# numberofdeaths (Number of traffic deaths in that state)
# cell_subscription (Number of cell phone subscriptions in thousands)
# population (Population within a state)
# total_miles_driven (Total miles driven within a state for that year in millions of miles) 
# urban_percent (percentage of the population that lives in an urban area)
# cell_ban (bans that prohibit drivers from using handheld devices for any purpose)
# text_ban (bans texting while driving)


#import dataset "cell"
cell = read_csv("cell.csv")
cell

#run simple regression - predict number of deaths using cell_subscription as the predictor.
mod1=lm(numberofdeaths ~ cell_subscription, data=cell)
summary(mod1)

#Interpretation
#intercept: Number of traffic deaths in a state with zero cell subscriptions is 124 Indicates other variables must be affecting number of deaths.
#cell_subscription: When cell_subscription increase by a thousand, the number of deaths increase by 0.09.
#Note: it is important to interpret the coefficients and also whether they are significant or not.

#Omitted variable bias
# let's try adding another variable - total_miles_driven
mod2=lm(numberofdeaths ~ cell_subscription + total_miles_driven, data=cell)
summary(mod2)

#What are the two conditions for OVB? Was there OVB before having miles driven in the equation?

cor.test(cell$cell_subscription, cell$total_miles_driven)

#What can you say about the direction of the OVB?
#reference slide - week 4 lecture slide number 40.

#the coefficient for cell_subscriptions has positive bias. 

#calculate residuals and predicted values
#residuals = observed - predicted values
#predicted values = predicted by the model

cell$resid = resid(mod1)
cell$pred = predict(mod1)

#data wrangling

#which states have a text_ban?

cell %>%
  filter(text_ban == 1) %>%
  select(state, text_ban)

#what is the total number of deaths in the states that have a text ban?

cell %>%
  group_by(text_ban) %>%
  summarize(sum.deaths = sum(numberofdeaths))

cell %>%
  filter(text_ban == 1) %>%
  summarize(sum.death = sum(numberofdeaths))

#Among the states that have urban percentage less than 50, which state 
#has the highest population??

cell %>%
  filter(urban_percent < 50) %>%
  select(state, population, urban_percent) %>%
  arrange(desc(population))

