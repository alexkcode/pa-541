#------------------------------------------------------------------------------
#PA 541 - M.D. Siciliano
#Week 12 - Logistic Regression
#In Class Exercise
#Solutions
#
#------------------------------------------------------------------------------
ski2 = load("ski.RData")
ski=read_spss("ski.sav")
ski
#str(ski)

#run a logistic regression

ski1=glm(Fall ~ Difficulty + Season, family=binomial(link="logit"), data=ski)
summary(ski1)
#to get the odds ratio of the coefficients, take the exponential
exp(ski1$coef)

# 1. Calculate the increase in odds for falling on a slope in winter of 
# difficulty 1 versus difficulty 2.

#This is just the exp of the coefficient on difficulty
exp(1.5688) #so the odds increase 4.8 times

# 2. Calculate the increase in odds for falling on a slope in winter of 
# difficulty 1 versus difficulty 3.

#Easiest way to do this is to look at the effect of an increase of 2 in
#difficulty.  Recall this is a multiplicative effect
exp(1.5688*2)# so this equals 23.  So your odds are 23 times greater for falling
#on a slope of difficulty 3 than a slope of difficulty 1 holding everything else 
#equal.  Note that winter or not doesn't matter. Also note, you get the same value if
#you take exp(1.5688) * exp(1.5688)

#compare odds in winter; reminder, the first one in matrix is for the intercept
s1=as.matrix(c(1,1,1))#create a data set for winter=1 and difficulty = 1
s2=as.matrix(c(1,3,1))#create  data set for winter=1 and difficulty = 3
odds_s1=exp(crossprod(s1, coef(ski1)))
odds_s2=exp(crossprod(s2, coef(ski1)))
odds_s2/odds_s1

#compare odds in not winter
s1b=as.matrix(c(1,1,0))#create a data set for winter=0 and difficulty = 1
s2b=as.matrix(c(1,3,0))#create  data set for winter=0 and difficulty = 3
odds_s1b=exp(crossprod(s1b, coef(ski1)))
odds_s2b=exp(crossprod(s2b, coef(ski1)))
odds_s2b/odds_s1b

#So we see that season does not matter.  Because there are no interactions
#the effect of slope difficulty is the same regardless of season.
#Now of course the probability of falling will be different for each scenario, but the
#change in the odds of falling as difficulty increases does not depend on the season.

#Some things to try -
# 1. In the above code change the 3 in s2 to a 2.  What should our results be?
s1=as.matrix(c(1,1,1))#create a data set for winter=1 and difficulty = 1
s2=as.matrix(c(1,2,1))#create  data set for winter=1 and difficulty = 2
odds_s1=exp(crossprod(s1, coef(ski1)))
odds_s2=exp(crossprod(s2, coef(ski1)))
odds_s2/odds_s1 #just our exponentiated coefficient for difficulty


# 3. Calculate the predicted probability for falling in winter on a 
# slope of difficulty 2 versus difficulty 5.
#Recall if Exp(z)  is odds(event).  The P(event) = Exp(z)/(1 + Exp(z)).

#As with the example above, we can calculate the logit (which is what we would
#do with a standard OLS, but now we need to take the exp and then calculate
#the probability from that) Note that unlike the example above, the season does
#matter now for the predictive probability.  

#so the odds for winter, difficulty of 2 and 5 are
s3=as.matrix(c(1,2,1))#create a data set for winter=1 and difficulty = 2
s4=as.matrix(c(1,5,1))#create  data set for winter=1 and difficulty = 5
odds_s3=exp(crossprod(s3, coef(ski1)))
odds_s4=exp(crossprod(s4, coef(ski1)))

odds_s3
odds_s4

#probability for s3
odds_s3/(1+odds_s3)

#probability for s4
odds_s4/(1+odds_s4)