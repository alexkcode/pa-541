#############################################################
#
#PA 541 ADVANCED DATA ANALYSIS
#MICHAEL D. SICILIANO
#WEEK 8: Nonlinearity and Testing Regression Assumptions
#
##############################################################


library(lmtest) #has the coeftest () function and bptest() function
library(sandwich) #HC standard errors
library(car) #This is what we used for linearHypothesis; also has the vif() test
library(tidyverse)


#-------------------------Testing for Heteroskedastictiy
load("wage2.RData")
wage2 = as_tibble(data)

wage2

#let's graphically look at our potentially heteroskedastic relationhsip
ggplot(data = wage2, aes(x=educ, y=wage)) + geom_point() 

m1 = lm(wage ~ educ, data=wage2)
summary(m1)
#Let's calculate the BP test 'by hand'
wage2$resid = resid(m1)
wage2$residsq = wage2$resid^2 #test requires the square of the residuals

m2 = lm(residsq ~ educ, data = wage2) #then we attempt to predict the square of the residuals
#by the IVs in our model.
summary(m2)

#There is a BP test function in the 'lmtest' package
#you simply call the function and enter in the original regression model
#so for our example above, we would enter
bptest(m1)
#note the p-value and F-stat are slightly different from the value we
#obtained when we calculated this by hand.  This is because this test
#by default uses Koenker's studentized version of the test statistic.

#for our purposes, either test will be sufficient (in the homework or
#on exams)



#-----Let's correct our standard errors for heteroskedasticity

#we will use the vcovHC function from the sandwich package to calcualte our
#heteroscedasticity-consistent standard errors

coeftest(m1, vcov = vcovHC(m1, type="HC0")) #gives us the heteroskedastic robust standard errors
#The 'sandwich' package allows us to estimate several
#different versions of heteroskedasticity-consistent standard errors.  The "HC0" is
#the white's standard errors, though there are other froms that use slightly different
#formulas for correction.  
#For instance stata uses "HC1", so if you want to reproduce stata results,
#this is the estimation procedure you would want.

#to replicate results you would see in Stata
coeftest(m1, vcov = vcovHC(m1, type="HC1"))


#---------------------Testing for Multicollinearity

lm2 = lm(wage ~ educ + exper, data=wage2)
summary(lm2)

#to test for multicollinearity in this model we first need to run a regression
#predicting education by experience

mult1 = lm(educ ~ exper, data = wage2)
summary(mult1)
#we note the r-squared is is .2075
#Then we simply calculate the VIF:  1/(1-.2075) = 1.26

#altenatively, we can use the vif function from the car package
vif(lm2) #remember you want to use this with your original model
#here we get the exact same results as when we calcualted it by hand
#above

