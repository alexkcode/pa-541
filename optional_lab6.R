#load libraries
library(tidyverse)
library(ggplot2)
library(lmtest)

#import dataset
titanic = read_csv("TitanicData.csv")
fatal = read_csv("Fatalities.csv")

#titanic dataset
#Survived (dummy variable to indicate if the passenger survived (=1) or not (=0))
#Pclass (passenger class (1st = 1, 2nd = 2, 3rd = 3))
#Name (full name of passenger)
#Sex (passenger sex)
#Age (passenger age in years)
#Siblings/Spouses Aboard (number of siblings/Spouses aboard)
#Parents/Children Aboard (number of parents/children aboard)
#Fare (amount paid for ticket)

#simple regression
mod1 = lm(Fare ~ Sex + Age, data = titanic)
summary(mod1)


#Interpretation:
#Sex: Coefficient = -20.09
#This tells us that a male pays $20.09 less for a ticket as compared to female. With a p-value of 6.38e-09, this relationship is significant at the significance level of 0.0001. 
#Age: Coefficient = 0.46
#As age increases by a year, the fare increases by $0.46. With a p-value of 8.44e-05, this relationship is significant at the significance level of 0.0001

#histogram of variable Fare
ggplot(titanic, aes(x = Fare)) + geom_histogram()

#convert into log
titanic = titanic %>%
  filter(Fare>0)
titanic$log.Fare = log(titanic$Fare)
ggplot(titanic, aes(x=log.Fare)) + geom_histogram()

#log-lin model #see week12 slide 4 to know how to interpret such models.
mod2 = lm(log.Fare ~ Age, data = titanic)
summary(mod2)

#Interpretation
#Age: Coefficient = 0.0099
#As age increases by a year, the fare increases by (0.0099*100) 0.99%. With a p-value of 8.59e-06, this relationship is statistically significant at the two-tailed alpha = 0.0001. 


#logistic Regession
mod3 = glm(Survived ~ Age + Sex + factor(Pclass), family = binomial(link = "logit"), data = titanic)
summary(mod3)

#Interpretation
#Age: Coefficient = -0.035
#As age increases by one year, it decreases the log odds of surviving by 0.035. With a p-value of 1.14e-06, this relationship is statistically significant at the two-tailed alpha = 0.0001. 
#Sex: Coefficient = -2.56
#The log odds of surviving for a male are 2.56 less than the log odds of surviving for a female. In this case, the log odds of male surviving are (3.71-2.56=1.15). With a p-value of <2e-16, this relationship is statistically significant at the two-tailed alpha = 0.0001. 
#Pclass2 = -1.22
#The log odds of surviving for a female in class2 are 1.22 less than the log odds of surviving for a female in class1. With a p-value of 4.82e-16, this relationship is statistically significant at the two-tailed alpha = 0.0001. 
#Pclass3 = -2.54
#The log odds of surviving for a female in class3 are 2.54 less than the log odds of surviving for a female in class1. With a p-value of <2e-16, this relationship is statistically significant at the two-tailed alpha = 0.0001. 

#in odds-ratio
exp(mod3$coef) #see week12 slide 54 on how we interpret exponential values (odds).

#Interpretation
#Age: As age increases by 1,the odds of survival are 0.97 times less.
#Sexmale: As compared to female, the odds of survival of a male are 0.07 times less. 
#Similar for pclass coefficients compared with pclass1.


#calculate the predicted probability of survival for a female in first class who is 35 years old

#example
x1 = 3.71 + -(0.035*35) + 0 + 0 + 0
x1
exp(x1)/(1+exp(x1)) #see week12 slide 34 for more details on how to convert logits into probabilities.


#pooled cross-sectional data
mod4 = lm(fatal ~ spirits + miles + factor(year), data =fatal)
summary(mod4)

#what is the base year here?

#pooled cross-sectional data with interaction
#is the effect of miles same across all years?
mod5 = lm(fatal ~ spirits + miles + factor(year)*miles, data =fatal)
summary(mod5)
