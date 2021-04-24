###########################################################
#
#PA 541 ADVANCED DATA ANALYSIS
#MICHAEL D. SICILIANO
#Week 13 - Panel Data Part 1
#
#####################################################

library("tidyverse")
library("sandwich")
library("lmtest")
library("haven")


#-------------------------------------------------------------------------------
#Maximum Likelihood Estimation
#------------------------------------------------------------------------------
#Begin with a really simple example (but not really that simple)
#Note all of the info in this section is just for your information...you
#will not be tested on it.

y=c(1,0,0,1,1)
#So here we assume Y is distributed bernoulli with some constant probability
#of seing a one across observations.  We might also assume the observations
#are independent.  The model
#Yi ~ Fbern(yi|zi)
#z_i=z #note the probability is the same for all observations i
#Yi and Yj are independent

#so the probability that Y=1 is just z and the probability y = 0 is 1-z

#the joint distribution for out data conditional on z is
#Pr(y|z) = Pr(Y1=1, Y2=0, Y3=0, Y4=1, Y5=1|z); which can be rewritten as
#Pr(y|z) = Pr(Y1=1|z)Pr(Y2=0|z)...Pr(Y5=1|z)
#Pr(y|z) = z*(1-z)*(1-z)*z*z; note that this just the prob for each observation in Y
#Pr(y|z) = z^3 * (1-z)^2; since we have 3 one's and 2 zeros

#the likelihood function L(z|Y) = z^3 * (1-z)^2
#the reason for the this that the likelihood and probability are proportional.
#in the probability statement we assume z is given.  In statistics and for 
#our likelihood theory, Y is given and we try to find the most likely z that
#produced our Y.

#Take the log of both sides of the likelihood function (note taking the log
#here is relatively unimportant in this simple example, but becomes essential for
#computational reasons later on).  Taking the natural log we get:
#ln(z^3) + ln (1-z)^2  #which can be reduced to
#3ln(z) + 2ln(1-z)

#Let's actually make a plot of the likelihood at different values of z
z1=seq(.01 ,.99, by=.001) #lets plug these values into our loglikelihood equation


y1=3*log(z1) + 2*log(1-z1)
plot(y1 ~z1, xlab="Value of z", ylab = "Log Likelihood")

#the resulting solution is .6; which matches the value we would have obtained
#using calculus (by setting the derivative of the loglikelihood to zero, because
#that is the point at which the function is maximized).

#What happens with this data when we run a model

y.mod = glm(y ~ 1, family = binomial)
summary(y.mod)

#Transform the intercept back to the scale of the response
#which is in probability
exp(y.mod$coefficients[1]) / (1+ exp(y.mod$coefficients[1]))
#The result is .6 (as we would expect). This is the MLE for z. 


#
#----------------------
#MLE with our Ski Data From Last week
#---------------------

ski=read_spss("ski.sav")
ski


#run a logistic regression

ski1=glm(Fall ~ Difficulty + Season, family=binomial(link="logit"), data=ski)
summary(ski1)

#Note that the residual deviance is 26.470. This is a "black box" output that
#I want to show you where it came from.  MLE is not a mystery when we see
#how it operates with a simple dataset.
#Open the excel sheet in courseweb to see exactly how this value was computed.



###--------------------------------Pooled Cross Sectional

#Fertility Example - wooldridge p. 450 3e
# Obs:  1129
# 
# 1. year                     72 to 84, even
# 2. educ                     years of schooling
# 3. meduc                    mother's education
#   4. feduc                    father's education
#   5. age                      in years
# 6. kids                     # children ever born
# 7. black                    = 1 if black
# 8. east                     = 1 if lived in east at 16
# 9. northcen                 = 1 if lived in nc at 16
# 10. west                     = 1 if lived in west at 16
# 11. farm                     = 1 if on farm at 16
# 12. othrural                 = 1 if other rural at 16
# 13. town                     = 1 if lived in town at 16
# 14. smcity                   = 1 if in small city at 16
# 15. y74                      = 1 if year = 74
# 16. y76                      
# 17. y78                      
# 18. y80                      
# 19. y82                      
# 20. y84                      
# 21. agesq                    age^2
# 22. y74educ                  y74*educ
# 23. y76educ                  
# 24. y78educ                  
# 25. y80educ                  
# 26. y82educ                  
# 27. y84educ                  


fertil=read_csv("FERTIL1.csv", col_names = FALSE)
fertil
colnames(fertil)=c("year", "educ", "meduc", "feduc", "age", "kids", "black", "east",     
                   "northcen", "west", "farm", "othrural", "town", "smcity", "y74", "y76",      
                   "y78", "y80", "y82", "y84", "agesq", "y74educ", "y76educ", "y78educ",  
                   "y80edu", "y82educ", "y84educ")

#Table of the mean number of kids per year
#need to make year a factor variable
fertil$year=factor(fertil$year)

aggdat = fertil %>%
  select(kids, educ, east, northcen, west, age, year)

aggdat %>%
  group_by(year) %>%
  summarize_all(mean, na.rm = T)


pcs1=lm(kids ~ educ + age + agesq + black +east + northcen + west + farm + othrural + town +
          smcity + y74 + y76 + y78 + y80 + y82 + y84, data=fertil)

summary(pcs1)

# The model assumes that the effect of the explanatory variable, particularly education, 
# has remained constant.  We will investigate this.
# There may be heteroskedasticity in the error term underlying the estimated equation.  

#Testing Heteroskedasticity using the Breusch-Pagan test
#There may be heteroskedasiticy in the error term underlying the estimated equation.  This
#can be dealt with using linear regression methods (see Wooldridge ch 8).  There is one 
# interesting difference here: now the error variance may change overtime even if it does not
# change with the values of education, age, black, and so on.  The heteroskedasticity-robust 
# standard errors and test statistics are nevertheless valid. 

library(lmtest)#need package to do bptest

bptest(pcs1) #based on the test it appears that heteroskedasticity is a problem and
#we should probably use a robust standard error

#Robust Standard Errors in R
pcs1$newse = vcovHC(pcs1) #create a new var-cov matrix which allows
#us to produce robust standard errors
coeftest(pcs1,pcs1$newse) #update the table with the robust SEs

stata.se = vcovHC(pcs1, type="HC1") #again, if you are looking to replicate stata
#results, stata using HC1
coeftest(pcs1, vcov=stata.se)



##---Let's interact education with years
pcs2=lm(kids ~ educ + age + agesq + black +east + northcen + west + farm + othrural + town +
          smcity + y74 + y76 + y78 + y80 + y82 + y84 + educ:y74 + educ:y76 + educ:y78 + 
          educ:y80 + educ:y82 + educ:y84, data=fertil)

summary(pcs2)

# Three things to note:
# Educ itself is no longer significant.so education in 1972 did not have a significant on the number of kids.
# The year effects are now positive, where in the model without the interaction they were negative.
# General trend of the effect of education on children is increasingly negative overtime.more education means less children.




##-----------------POLICY ANALYSIS WITH POOLED CROSS SECTIONAL DATA
price=read_dta("KIELMC.dta")
price

p81=filter(price, year==1981)
p78=filter(price, year==1978)

#Naive approach
lm81=lm(rprice ~ nearinc, data=p81)
summary(lm81)

lm78=lm(rprice ~ nearinc, data=p78)
summary(lm78)

#----------DIFFERENCE IN DIFFERENCE APPROACH

lmfull=lm(rprice ~ y81 + nearinc + nearinc:y81, data=price)
summary(lmfull)
#we don't find a significant effect, it is close to .10 but not significant

#If we add a set of appropriate predictors that should be related to predicting housing price
#we do find a significant effect
lmfull2=lm(rprice ~ y81 + nearinc + nearinc:y81 + age + agesq + intst + land + area +
             rooms + baths, data=price)
summary(lmfull2)


