#############################################
#
# Week 5 Multiple Regression Inference
#         Spring 2021
#
##############################################

#Set your Working Directory

#Load your libraries
library(tidyverse)
library(haven)
library(car)



#---------------Test the equality of regression coefficients

data(Duncan)

mod.duncan <- lm(prestige ~ income + education, data=Duncan)
summary(mod.duncan)

#Three different approaches

#1. use a canned function such as linearHypothesis
linearHypothesis(mod.duncan, "income = education")
#the f test is not significant and so we fail to reject the
#null and conclude that they are equivalent...

#2. run a restricted model and compare with the full.
#This was not covered in class......but follows the same logic
#as the test of exclusion restrictions.  We once again need
#an unrestricted model, which imposes no restrictions on
#what values our coefficients can take.  We also need a 
#restricted model in which we force certain coefficients
#to match our null hypothesis.  Here our null is that the
#coefficients on income and education are the same.  Thus
#a one unit change in either one has the same effect. So we
#can develop a restricted model that forces the effect to be the same.
#we do this by creating a new variable that is the sum of education and
#income. Now when either increases by 1 unit, the impact on prestige
#is the same.

#Note the I function stands for 'as is'.  It overrides the
#interpretation of a model symbol as a formula operator when the intention
#is to use it as an arithmetic operator.  Suppose you want to fit 1/x
# as an explanatory variable in a regression, you might try
# y ~ 1/x, but something strange happens, as the "/" is used
#to indicate nesting in model, if you want it it mean division within
# a regression call, you need to use , y ~ I(1/x)
#for our model we are going to use 'I' to add the variables together.
mod1 = lm(prestige ~ I(income+education), data=Duncan)
mod2 = lm(prestige ~ income + education, data=Duncan)

anova(mod1, mod2) #gives us the same result as the linear hypothesis test


#3. Wooldridge approach where you create a combined term as
#we did above, but also add in 'income' as well and 
#test the significance on income's coefficient

Duncan$inced = Duncan$income + Duncan$education

mod3 = lm(prestige ~ income + inced, data=Duncan)
summary(mod3) #note we get the same p-value on the variable
#in fact if you square the t-value (.261) you get the F-value from
#either of the approaches above.
#also note, the value of the coefficient on income, is exactly 
#the difference between income and education in the original model
# .59873-.54583 = .0529


#------------------Testing Exclusion Restrictions

hl = read_dta(file="homeless1.dta")
head(hl)

cor(hl, use="complete.obs")

mod1 = lm (hmlss ~ grossr + ssipop + unemploy + 
          mhosp , data=hl)
summary(mod1)
#no significant effect for unemployment or ssipop; let's
#test to see if they are jointly significant

mod2 = lm(hmlss ~ grossr + mhosp, data=hl)
summary(mod2)

#to calculate the F-test using the SSRs
anova(mod1)
anova(mod2)

#we can pull out the values from our anova table as follows
ssr.ur = anova(mod1)[5,2] #or you could just enter these numbers by hand
ssr.r = anova(mod2) [3,2]

f.value = ((ssr.r - ssr.ur)/2) / (ssr.ur/ (270-5))
f.value
#remember, the degrees of freedom in the numerator, q, is just
#the number of exclusions.  the degrees of freedom in the denominator
#is the df of the unrestricted model.  Thus we had 270 observations
#and 4 predictors plus the intercept.  Here you need to be careful, while
#the dataset has 273 observations, due to missing data, three of the
#observations were removed when calcualting the model for a total n of 270
#and thus are df in the unrestriced model is 265.

#now we need to compare that F-value to the f critical value
qf(.95, 2, 265) # so we fail to reject the null

#just as side note, if you are using R to look of the critical value
#for the T-dist, you only get a one sided test, so you need to
#remember to divide your alpha in half.  Thus if you want a 5%
#two tailed test you would need to enter:
qt(.975, df=265)
qt(.975, df = c(1:10,20,50,100,1000)) #here you can see how the
#t converges to a normal when the df increases

#---Another Approach for testing exclusion restrictions
#the easy way to do this test is as follows:
anova(mod2, mod1)#note, we get the exact same F-value and same result
#we do not reject the null and conclude that both coefficients on
#ssipop and unemploy are equal to zero

#----A Third Way
#another approach to testing exclusion restricts uses the same 
#linearHypothesis function we used above, now we simply set 
#the two variables of interest to zero, as is our null
#hypothesis in these cases

linearHypothesis(mod1, c("ssipop=0", "unemploy=0"))


#############################################
#IN CLASS EXERCISE
#############################################

## The Data

#Read in the 'cov2.csv' data.  This dataset contains information for 190 countries on a 
#number of variables related to COVID-19 cases, deaths, and vaccinations. 
#The data come from https://ourworldindata.org/coronavirus. The data also contains 
#information on the population, percentage of the population over 65, gdp per capita, 
#as well as information on a number of relevant health factors.  The data are current as 
#February 1st, 2021.  See the associated codebook for a full description of each variable 
#along with its associated source.

cov = read_csv("cov2.csv")

cov

#Initial Steps:

#Calculate a new variable that is total cases per 10,000 people:

cov$cases_10k <- cov$total_cases / (cov$population / 10000)


#Because we are going to test exclusion restrictions based on model fit, we
#we need to ensure that our models are nested and that they have the same 
#number of observations. We will discuss missing data in more detail later, but
#for now, let's just drop observations with missing data on the following
#variables:

cov2 = cov %>% drop_na(handwashing_facilities, extreme_poverty, gdp_per_capita,
                       female_smokers, male_smokers)
#note we lose a ton of observations, mostly due to lack of data on
#handwashing facilities...for this exercise we are not going to worry
#about this.

#Question 1 - run a model predicting total cases per 10,000 people by 
#handwasing facilities, extreme poverty, and gdp per capita. You will
#find that none of these variables are statistically significant. Now,
#run a model with just an intercept term. Test if
#all three variables can be excluded from the model or whether they
#are jointly significant.  Discuss your findings. 

mod1 <- lm(cases_10k ~ handwashing_facilities + extreme_poverty + gdp_per_capita, data = cov2)

summary(mod1)

mod2 <- lm(cases_10k ~ 1, data = cov2)

summary(mod2)

#Question 2 - Compare your findings above to the overall model F-test that you observe
#in the summary output for the original model predicting total cases per 10,000 people
#by handwasing facilities, extreme poverty, and gdp per capita. What do you make of 
#these results?

anova(mod1, mod2)

cov2$agg <- cov2$handwashing_facilities + cov2$extreme_poverty + cov2$gdp_per_capita

mod3 <- lm(cases_10k ~ agg, data = cov2)

summary(mod3)

#Question 3 - run a model predicting total deaths per 100,000 by the percentage of
#female smokers and the percentage of male smokers. Then test whether the coefficient 
#on female smokers is the same as male smokers. 


