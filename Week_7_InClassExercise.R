#####################################################################
#IN-Class Exercise
#####################################################################

#read in data
library(tidyverse)
library(haven)
wages = read_spss("wage1_q.sav")
wages


#1 - interactions and plotting interactions

# Using the 'wages' dataset, run a model predicting wage, by educ, exper, 
# white, male, and tenure

# Interpret the results.


c3 = lm (wage ~ white + Male + educ + exper + tenure, data=wages)
summary(c3)
#no significant effect of race, but predicted values of whites are 11.5 cents higher
#Males earn $1.81 more than females
#each year of education adds 57cents to your wage
#each year of experience adds .025cents to your wage
#each year of tenure adds .14cents to your wage


# What is the expected wage of a white, female, with 8 years of educ, and 0 years of exper and tenure?

#note, these are easily calculated by using the regression equation and subbing in the values
#a quicker way using R is to create a new "observations" that meets the requirements specified
#above.
pred1 = as.matrix(c(1,1,0,8,0,0)) #here we have 1 for the intercept, 1 for white, 0 for male, 8 for educ
#0 for exper, and 0 for tenure.  To get the expected wages we multiply those values by their respective
#coefficients and sum them up.  This is exactly what crossprod() does.
wage_obs1 = crossprod(pred1, coef(c3))
wage_obs1

#something to note, that may help provide some more intuitive understanding of regression output.
#let's obtain a new predicted value, but this time let's increase educ by 1 year.
pred2 = as.matrix(c(1,1,0,9,0,0))
wage_obs2 = crossprod(pred2, coef(c3))
wage_obs2
#if we take wage2 - wage1, we get the coefficient on education
wage_obs2 - wage_obs1 # this is exactly what we mean when we say the effect of an additional year
#on education holding all else constant.



# Create an interaction effect between tenure and white.
# Interpret the results.
# Attempt to visualize the interaction effect

c4 = lm (wage ~ white + Male + educ + exper + tenure + tenure*white, data=wages)
summary(c4)
#the effect of the interaction is borderline sig at the 10% level.  The value suggests
#that for whites, the effect of tenure is greater than for non-whites.  In other words,
#the wages for whites have a larger increase for each additional year of tenure.

#create interaction plot with ggplot

#here is a plot of the raw data
gp <- ggplot(data=wages, aes(x=tenure, y=wage, colour=factor(white))) 
gp + geom_point() + stat_smooth(method="lm")

#here is a plot of our predicted values based on our model.  
#Need to start with a new dataset
#here I am setting the variables other than those in the interaction to a
#specific value, as those values do not affect the association of white and tenure
#on wages
newdata2 = expand.grid(white= c(0,1),
                       tenure = c(0:20), Male = 1, educ=12, exper=5 )
head(newdata2)


newdata2$preds = predict(c4, new=newdata2, type="response")

ggplot(newdata2, aes(x=tenure, y=preds, group=white)) + 
  geom_point(aes(colour=as.factor(white)))
