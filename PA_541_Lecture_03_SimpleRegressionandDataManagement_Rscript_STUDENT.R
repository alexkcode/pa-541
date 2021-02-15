#####################################################
# PA 541 - Week 3 - Simple Regression
####################################################

#SET YOUR WORKING DIRECTORY


#LOAD YOUR LIBRARIES
library(MASS)
library(faraway)
library(haven)
library(tidyverse)

#Demonstration of significance by chance alone
set.seed(1827)
a.random = rnorm(50)

b.data = data.frame(replicate(100, rnorm(50)))

cor.size = vector()
for (i in 1: 100)
  cor.size[i] = cor.test(a.random, b.data[,i])$estimate

cor.results = vector()
for (i in 1: 100)
  cor.results[i] = cor.test(a.random, b.data[,i])$p.value

trial = seq(1:100)

c.data = data.frame(trial, cor.size, cor.results)
c.data = c.data %>%
  mutate(sig = ifelse(cor.results <=.05, "significant", "not significant"))

#let's plot the size of the correlation
ggplot(c.data, aes (x = cor.size)) + geom_histogram(bins = 15)
ggplot(c.data, aes (x = cor.size, fill = sig)) + geom_dotplot()

#--------------------------------------

load("pima.RData")
pima = as_tibble(pima)

#create another file to be merged with pima
educ=sample(1:20, 768, replace=T)
respondent.id=1:768

pima2=cbind(educ, respondent.id)
pima2=as_tibble(pima2)


#Info on dataset - from Faraway 2005
#The National Institute of Diabetes and Digestive and Kidney Diseases
# conducted a study on 768 adult female Pima Indians living near Phoenix. 
# The following variables were recorded: Number of times pregnant, Plasma glucose concentration a 2 hours in an oral glucose tolerance
# test, Diastolic blood pressure (mm Hg), Triceps skin fold thickness (mm), 2-Hour serum insulin (mu U/ml),
# Body mass index (weight in kg/(height in m2)), Diabetes pedigree function, Age (years) and a test whether
# the patient shows signs of diabetes (coded 0 if negative, 1 if positive).

#As an initial pass over your data to check for any signs of issues or things 
#that look at out of place you can call summary('nameofdataset')
#is the data distributed as you would expect?  Are there any numbers that are not plausible?
summary(pima)

glimpse(pima)

#A few things might jump out at us:
#1. Pregnant has a value of 17, high but possible.
#2. diastolic, insulin, bmi all have zero values...probably not
#possible if the survey subjects were alive.

#Let's look into this a little further:

pima %>% arrange(diastolic)
#perhaps zero was used to indicate missing data - would want to 
#try and verify this with the original researchers.  

#Faraway 2005, p. 4: For one reason or another, the researchers did 
#not obtain the blood pressures of 35 patients.  In a real investigation, 
#one would likely be able to  question the researchers about what really 
#happened.  Nevertheless, this does illustrate the kind of misunderstanding 
#that can easily occur.  A careless statistician might overlook these 
#presumed missing values and complete the analysis as if they were real observed zeros.  


#Can also look at some plots and correlations with the data:
ggplot(pima, aes(x = pregnant)) + geom_bar() 

cor(pima)
#Note, this command will only work if all of your data is numeric.  
#If you want to look at only a subset of columns you can subset the data.
cor(pima[,c(2,3,6)])



#----------MERGING DATA FRAMES

# We often have a situation where there are two datasets we want to merge
# For example, if you are working with census data, you find different census 
# track level information from a variety of sources.  You can then link across 
# those sources based on the common identifier, the census track number.

# Let's look at an example with our pima data
pima
pima2
#The common column name 'respondent.id' allows us to merge the data together
#using the merge() function.

#we will create a new dataset and call it pima 3
pima3 = left_join(pima, pima2, by ="respondent.id")
pima3

#If you want to read more (and see other functions) you
#can go to  https://r4ds.had.co.nz/relational-data.html
#here we use left_join, which keeps all of the observations in the
#first dataframe and adds the variables in the second (that match on the id in the first)


#--------CREATING NEW VARIABLES

#new variables can be easily created in R using '$'
#for instance, if we wanted to create a variable that 
#measured a persons age in terms of months we could:

pima3$age.months = pima3$age*12
#note, this formula says to create a new variable in dataframe pima3
#that is called 'age.months' and set that variable equal to 
#the original age variable in pima3 * 12.
pima3

#if you are going to be creating a bunch of variables it may be
#for efficient to use 'mutate' as we saw in week 1.  But for 
#quick variable creation, the dollar sign method is simple and
#straightforward



#---------RECODING VARIABLES

# Recoding involves creating new values of a variable conditional on the 
# existing values of the same and/or other variables.  
# For example you may want to:
#   -Change a continuous variable into a set of categories
#   -Replace miscoded values with correct values
#   -Create a pass/fail variable based on a set of cutoff scores


# Assume we want to change our ages of pima respondents into young, middle age, 
# and old. The statement:
#   variable[condition] = expression 
# will only make the assignment condition when the condition is true.

pima3$agecat[pima3$age > 70] = "Elder"
pima3$agecat[pima3$age >= 40 & pima3$age <= 70] = "Middle"
pima3$agecat[pima3$age < 40] = "Young"


pima3$agecat.b <- cut(pima$age,
                      breaks=c(-Inf, 39, 70, Inf),
                      labels=c("Young","Middle","Elder"))
#by default, the values on the low end of the range are not included but those on the
#high end are.  Thus, for 39 and 70, it means we get the range from 40 - 69 and then from
#70 and above

#A third method is to use the dplyr term case_when()
pima3 = pima3 %>% 
  mutate(agecat.c = case_when( age > 70 ~ 'Elder',
                               age >= 40 & age <= 70 ~ 'Middle',
                               age < 40 ~ 'Young'))


#---------CREATING A FACTOR VARIABLE

class(pima3$agecat)#note this is a character vector
#given that it is a character vector it won't behave
#as we would like in some statistical analyses. 

#let's turn agecat into a factor

pima3$agecat.f=factor(pima3$agecat)
class(pima3$agecat.f)
pima3$agecat.f[1:20]#take a look at the first 20 observations

#note that by default the levels of a factor are ordered in
#alphabetical order.  

ggplot(pima3, aes(x = agecat.f)) + geom_bar()

#You can change this using the following command:
pima3$agecat.f2=factor(pima3$agecat, levels = c('Young', 'Middle', 'Elder') )
pima3$agecat.f[1:20] 
ggplot(pima3, aes(x = agecat.f2)) + geom_bar()

#what if the variable is numeric...such as pima3$test, which indicates
#whether or not the individual shows signs of diabetes.  We can convert this into
#a factor as well.

#Let's turn the numeric variable test into a factor
pima3$test.f=factor(pima3$test)
pima3$test.f[1:20]

#we can also add labels to these numeric indicators to improve
#the tables and graphics we create:

pima3$test.f2=factor(pima3$test, labels=c('negative', 'positive'))
pima3$test.f2[1:20]
#note that the labels get assigned based on the numeric order of the
#original variable, thus 0=negative, and 1=positive.  So make sure levels line
#up with your labels

#FOR TABLES
table(pima3$agecat.f, pima3$test.f)#non-labeled factor variable for test
table(pima3$agecat.f, pima3$test.f2)#labeled factor variable for test


#-------------DEALING WITH MISSING VALUES

# In R, missing values are represented by the symbol NA.
# Unlike other programs, R uses the same missing values for character and numeric data.

some.data = c(1,2,NA,7,0,10)
#we can use the is.na () function to locate the missing variable in
#our example data

is.na(some.data)
#we see that is.na returns an object of the same size as our original object
#and the logical value of TRUE is placed in the corresponding element of the
#missing value

#As Kabacoff 2011 discusses: 
# NOTE Missing values are considered noncomparable, even to themselves.
# This means that you can't use comparison operators to test for the presence
# of missing values. For example, the logical test myvar == NA is never TRUE.
# Instead, you have to use missing values functions, like is.na() to
# identify the missing values in R data objects.

# Recoding missing values
# As we saw with our Pima dataset, there are some values that we assume
# should be labeled as missing - for example the zero values for blood pressure
# If we read in a dataset from SPSS, missing might be labeled as '99' and need
# recoded as well

pima3$diastolic[pima3$diastolic == 0] = NA
#this code uses assignment to recode to missing just as we did 
#above to break down a continuous variable into a categorical one.
#Any value of 0 in the column diastolic, will be changed to NA

pima3$diastolic#you now see the NAs scattered in the data

#Excluding missing values from Analyses
# these missing values will need to be eliminated from your data analysis
x = c(1,2,3,NA, 5)
y = sum(x)
y #note we get a value of NA.

# Thus, we need to tell R how we want to handle this:
# most numeric functions have an na.rm=TRUE option that 
# removes the missing values prior to calculation and applies
# the function only to the remaining values.  Other functions
# like cor() for correlation have a different way to deal
# with missing values so it is helpful to look at the help files
x = c(1,2,3,NA, 5)
y = sum(x, na.rm=TRUE)
y

#We can also omit the rows of a dataframe that have NAs and
#only look at complete cases

newpima = na.omit(pima3)
#note the number of observations has dropped by 35...those
#were the rows for which diastolic contained NA values
dim(pima3)
dim(newpima)


############################################################
#
#Simple or Bivariate Regression
#
###########################################################

load("ceosal1.RData")#note it saves the file as 'data'
#let's rename this to avoid confusion in the future

ceo=as_tibble(data)

ceo

#Let's make a quick plot
ggplot(ceo, aes(x=roe, y=salary)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)

#Let's run a simple regression
lm1=lm(salary ~ roe, data=ceo)
summary(lm1)


#---------Coefficient of Determination
#R-squared is the ratio of explained variation to the total variation.  
#In other words it is the fraction of the sample variation in Y (salary) 
#that can be explained by X (roe). 

#SST or the total variation in salary is:

SST = sum ( (ceo$salary - mean(ceo$salary))^2 )
SST

#SSR or the residual sum of squares, concerns the difference between
#the predicted values from the model, and the observed values

pred.y=predict(lm1)#predicted values from the model
SSR=sum((ceo$salary - pred.y)^2)
SSR

#SSM - is what is explained by the model

SSM = SST - SSR
SSM
#or alternatively

SSMb = sum ( (pred.y - mean(ceo$salary))^2 )
SSMb

#of course R calculates all of these for you
#and you can see the break down by using the anova fucntion
#this gives us the same results as those we calculated "by hand"
anova(lm1)


#---------Regression standard error or residual standard error


#the residual standard error is the standard deviation of the y
#values around the regression line #sqrt(deviance(fm)/df.residual(fm))

#so the variance of the y values around the regression line is simply
#the sum of the squared residuals divided by degrees of freedom
#of the residual.  The degrees of freedom of the residual is n-p
#where n is the number of observations and p is the number of
#parameters (which includes the intercept)

lm1=lm(salary ~ roe, data=ceo)
summary(lm1)

pred.y=predict(lm1)#predicted values from the model
SSR=sum((ceo$salary - pred.y)^2)

#we can use this sum of squares residual (SSR) to say how
#far on average the actual observed response values, Y, are
#from the fitted values by calculating the regression standard
#error.  The regression standard error is:

sqrt(SSR/df.residual(lm1))
#note the df.residual(lm1) gives us n-p or 207 for our simple model.
#the value of 1366.55 is an estimate of the standard deviation of the
#random errors in the simple linear regression model.  The unit of
#measurement for this value is the same as our dependent variable.
#thus we can talk about the average CEO's salary with a particular ROE,
#being $1,367 away from the predicted value.  Roughly 2X this value
#would give us a 95% CI for the salary.

#As a side note, the top of the lm summary output shows 'Residuals',
#this is simply the distribution of the actual residuals, thus

lm1.resids = ceo$salary - pred.y
summary(lm1.resids)
#there is also a function in R to get the residuals:

resid(lm1)
summary(resid(lm1))


#-----------Slope parameter
#See Slides

#####################################
# VISUALLY INSPECTING REGRESSION ASSUMPTIONS
#####################################
lm1=lm(salary ~ roe, data=ceo)
pred.y=predict(lm1) #obtain the predicted values or fitted values
resids.lm1 = resid(lm1) #obtain the residuals

a.data = tibble(
  preds = pred.y,
  resids = resids.lm1
)

#this plot allows us to assess linearity, homoskedasticity, and independence
ggplot(a.data, aes(x = preds, y = resids)) + geom_point() #plot resids versus predicted values

#to check for normality we can look at two different plots:
ggplot(a.data, aes(x = resids)) + geom_histogram()
#again our outliers are problematic here...data could
#potentially be normally distributed if they were removed.

qqnorm(resids.lm1)
qqline(resids.lm1, col=2)#col = 2 just makes the line red


############################################
#In Class Exercise
############################################

# Open the data set ceosal2.RData.  
# The variable salary is the annual compensation of CEOs in 
# thousands of dollars and ceoten is the prior number of years a 
# CEO has been with the firm.

load("ceosal2.RData")
sal2=as_tibble(data)

sal2 #quick look at the data



# 1. Find the average salary and the average tenure (years with firm) 
# in the sample.

mean(sal2$salary)

mean(sal2$ceoten)

# 2. How many CEOs are in their first year as CEO (that is ceoten = 0)?

sum( sal2$ceoten == 0 )


# 3. What is the longest tenure?

max(sal2$ceoten)


# 4. Estimate the simple regression model: Salary = B0 + B1(ceoten) + e
#What is the approximate increase in salary given one more year as a CEO.





# 5. What is the r-squared for this model - how do you interpret it?





# 6. What is the predicted salary of an individual who has 0 years 
# with a  firm; what is the predicted salary for an individual 
# who has been with a firm for 10 years?




# 7. Create a residual versus predicted values plot - do you have 
# any concerns regarding the regression assumptions?

