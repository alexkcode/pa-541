###########################################################
#
#PA 541 ADVANCED DATA ANALYSIS
#MICHAEL D. SICILIANO
#Week 12 - Logistic Regression
#
#####################################################


library(haven)
library(ggplot2)

#-------------------------------------------------------------------------
#Example of a linear probability model and its associated problems when
#modeling binary data
#-------------------------------------------------------------------------

loan=read_spss("Loan_App_Wooldridge.sav")
head(loan)
#Variables used in model
# reject = 1 if loan was rejected
# pubrec = 1 if person had previously filed for bankruptcy
# black = 1 if person is black
# hispan = 1 if person is Hispanic
# loanprc = is the amount of the loan divided by the price of the house

loan1=lm(reject ~ pubrec + black + hispan + loanprc, data=loan)
summary(loan1)

predict(loan1)#look at the predicted probabilities resulting from the model

#let's plot the predicted outcomes and see why using a linear model for
#binary outcomes can be problematic

#diagram with ggplot
ggplot(data=loan, aes(x=predict(loan1))) + geom_histogram() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))

#as you can see, some of the predicted probabilities are less than 1



#--------------------------------------------------------------------------
#Simple, small dataset logistic regression example
#--------------------------------------------------------------------------
ski=read_spss("ski.sav")
head(ski)
str(ski)

#run a logistic regression

ski1=glm(Fall ~ Difficulty + Season, family=binomial(link="logit"), data=ski)
summary(ski1)
#to get the odds ratio of the coefficients, take the exponential
exp(ski1$coef)

#Let's create a classification table (similar to what you might see in SPSS)
# predicted probabilities
Yhat <- fitted(ski1)
#or we could say predict(ski1, type="response")


#Question: is there a difference between fitted() and predict() - from stack exchange
# Yes, there is. If there is a link function relating the linear predictor to 
# the expected value of the response (such as log for Poisson regression or 
# logit for logistic regression), 
# predict returns the fitted values before the inverse of the link function 
# is applied (to return the data to the same scale as the response variable), 
# and fitted shows it after it is applied.
# This does mean that for models created by linear regression (lm), there is 
# no difference between fitted and predict.
# you can use predict(m,type="response") to get predictions on the 
# original (response) scale
head(predict(ski1))
head(fitted(ski1))
head(predict(ski1, type="response") )


#Let's plot the fitted values
ggplot(data=ski, aes(y=Yhat, x=Difficulty)) + geom_point() + geom_smooth() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))
#note if we plot the logits we get a straight line
Yhat2 = predict(ski1) #now Yhat2 is the predicted logit values
ggplot(data=ski, aes(y=Yhat2, x=Difficulty)) + geom_point() + geom_smooth() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))

#Given the non-linear relationship, if we want to talk about the effect of an
#independent variable (here difficulty) on the probability of the dependent variable
#(here falling), we need to look at different values of x.


#Let's go from difficulty of 1 to difficulty of 2
logit1 = ski1$coefficients[1] + ski1$coefficients[2]*1 + ski1$coefficients[3]*mean(ski$Season)
prob.1 = exp(logit1)/(1+exp(logit1))

logit2 = ski1$coefficients[1] + ski1$coefficients[2]*2 + ski1$coefficients[3]*mean(ski$Season)
prob.2 = exp(logit2)/(1+exp(logit2))

prob.2 - prob.1

#Let's go from difficulty of 2 to difficulty of 3
logit3 = ski1$coefficients[1] + ski1$coefficients[2]*3 + ski1$coefficients[3]*mean(ski$Season)
prob.3 = exp(logit3)/(1+exp(logit3))

prob.3 - prob.2

#Let's go from the mean of difficulty to the mean plus 1
logitm = ski1$coefficients[1] + ski1$coefficients[2]*mean(ski$Difficulty) + ski1$coefficients[3]*mean(ski$Season)
prob.m = exp(logitm)/(1+exp(logitm))

logitm1 = ski1$coefficients[1] + ski1$coefficients[2]*mean(ski$Difficulty + 1) + ski1$coefficients[3]*mean(ski$Season)
prob.m1 = exp(logitm1)/(1+exp(logitm1))

prob.m1 - prob.m


logitm = ski1$coefficients[1] + ski1$coefficients[2]*1 + ski1$coefficients[3]*0
prob.m = exp(logitm)/(1+exp(logitm))

logitm1 = ski1$coefficients[1] + ski1$coefficients[2]*3 + ski1$coefficients[3]*0
prob.m1 = exp(logitm1)/(1+exp(logitm1))

prob.m1 - prob.m

ski1$coefficients[2]*2

#The ggeffects package automates this approach

library(ggeffects)

ggpredict(ski1) #ggpredict calculates the effect holding other variables at their mean.
#in this case, this would be holding season at its mean value

#you can also look at only a single variable:
ggpredict(ski1, "Difficulty")

#---------------------------------------------------------------------------------
#Logistic Regression - Actual Data Set
#--------------------------------------------------------------------------------

#return to bank loan example from above....
loan=read_spss("Loan_App_Wooldridge.sav")
head(loan)
#Variables used in model
# reject = 1 if loan was rejected
# pubrec = 1 if person had previously filed for bankruptcy
# black = 1 if person is black
# hispan = 1 if person is Hispanic
# loanprc = is the amount of the loan divided by the price of the house

#note you do not need to specify the logit link as this is the default when
#you choose the binomial family. I often do just for clarity though.
loan2=glm(reject ~ pubrec + black + hispan + loanprc, family=binomial(link = "logit"), data=loan)
summary(loan2)
#to get the odds ratio's for the estimates we can say
exp(coef(loan2))


#One of the thing we can do to help with interpretation is to use these
#coefficients with some made up data to see how the odds of rejection 
#changes with a 1 unit change on a variable...say loan amount
#for example, let's say the person has previously filed for bankruptcy,
#is black, and the loan amount is for twice as much as the house.  then we
#can create a vector for these x variables
X1=as.matrix(c(1,1,1,0,2))#note the first 1 is for the constant term, next is for 
#public record, the next 1 is for black, 0 is for hispanic, and 2 is for the 
#size of loan
X2=as.matrix(c(1,1,1,0,3))# repeat but increase the size of the loan
crossprod(X1,coef(loan2))#this is the value of the logit...it is the sum of each
#x variable times the coefficient...just as we would do to predict the value of
#an outcome for any regression model
crossprod(X2, coef(loan2))
oddsx1=exp(crossprod(X1,coef(loan2)))#let's calculate the odds for x set 1
oddsx2=exp(crossprod(X2,coef(loan2)))#let's calculate the odds for x set 2
oddsx2/oddsx1# we get the value of the exponentiated coefficient on public record

#----Let's produce some predicted probabilities

#See the excel chart associated with this lecture....

# Probability interpretations. While logistic coefficients are usually interpreted 
# as odds, not probabilities, it is possible to use probabilities. Exp(z) 
# is odds(event).  The P(event) = Exp(z)/(1 + Exp(z)).  Recall z = the constant 
# plus the sum of crossproducts of the b coefficients times the values of their 
# respective X (independent) variables. For dichotomous independents assuming the 
# values (0,1), the crossproduct term is null when X = 0 and is b when X=1. For 
# continuous independents, different probabilities will be computed depending on 
# the value of X. That is, P(event) varies depending on the covariates.

#For our x set 1 above the probability of rejection is
oddsx1/(1+oddsx1)

#For our x set 2 it is 
oddsx2/(1+oddsx2)

#Another nice way to display the results and help with interpretation is to
#plot the probability of rejection at various levels of loan amount, holding
#the other variables in the equation fixed

#Create your sample data [I will show you two ways]

#just build a dataframe based on the values you want
sampdat=data.frame(pubrec=rep(1,100), black=rep(1,100), hispan=rep(0,100),
                   loanprc=seq(from=0, to=4, by=.04))

#perhaps easier, use the expand grid function
sampdat = expand.grid(pubrec = 1, black = 1, hispan = 0, 
                      loanprc = seq(from = 0, to = 4, by = .04))

head(sampdat)

#Obtain the predicted values from the sample data based off our model
predsamp=(predict(loan2, new=sampdat, type="response"))

#ggplot
ggplot(data=sampdat, aes(x=loanprc, y=predsamp)) + geom_point(colour="red") +
  xlab("Ratio of Loan Amount To House Price") + 
  ylab("Probability of Loan Rejection") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))


#----------Let's repeat for non-black, non-hispanic loan applications
sampdat2 = expand.grid(pubrec = 1, black = 0, hispan = 0, 
                       loanprc = seq(from = 0, to = 4, by = .04))

predsamp2=(predict(loan2, new=sampdat2, type="response"))

#ggplot
ggplot(data=sampdat2, aes(x=loanprc, y=predsamp2)) + geom_point(colour="green") +
  xlab("Ratio of Loan Amount To House Price") + 
  ylab("Probability of Loan Rejection") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))

##--------------Plot the two lines in the same graph
sampdat3 = expand.grid(pubrec = 1, black = c(0,1), hispan = 0, 
                       loanprc = seq(from = 0, to = 4, by = .04))

#above we let two variables vary, black and loanprc.
sampdat3$predsamp3=(predict(loan2, new=sampdat3, type="response"))

ggplot(data=sampdat3, aes(x=loanprc, y=predsamp3, colour = factor(black) ) ) + geom_point() +
  xlab("Ratio of Loan Amount To House Price") + 
  ylab("Probability of Loan Rejection") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))


####Just to clarify a point earlier...what if we looked at logits instead
#of probablity.

sampdat3$predsamp3.b=(predict(loan2, new=sampdat3))

#ggplot
ggplot(data=sampdat3, aes(x=loanprc, y=predsamp3.b, color= factor(black) )) + 
  geom_point() +
  xlab("Ratio of Loan Amount To House Price") + 
  ylab("Logit of Loan Rejection") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))
#we see two parallel lines...the size of the gap is the coefficient on black
#that coeffienct is close to 1.25; you can see that by looking at a loan amount
#of 1 and looking at the distance between the lines at that point.  Here you can
#see the gap is close to 1.25

# the other way is to use the ggeffects package I mentioned above

#The ggeffects package automates this approach

ggpredict(loan2) #ggpredict calculates the effect holding other variables at their mean.
#in this case, this would be holding season at its mean value

#you can also look at only a single variable:
dat = ggpredict(loan2, terms = c("loanprc [0:5 by = .1]", "black[0,1]", "hispan[0]", "pubrec [1]") )
dat

ggplot(data=dat, aes(x=x, y=predicted, color= group )) + 
  geom_point() + 
  xlab("Ratio of Loan Amount To House Price") + 
  ylab("Probability of Loan Rejection") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))


#-----------------Testing for Multicollinearity (not discussed in lecture)

#we will use the vif() function in the car package for this
library(car)

loan2=glm(reject ~ pubrec + black + hispan + loanprc, family=binomial(link="logit"), data=loan)
summary(loan2)

vif(loan2) #everything looks good.  Recall that there are not strict rules
#at which we should worry.  But some have argued that viF's of 10 or greater
#are potentially a problem that needs to be dealt with.


