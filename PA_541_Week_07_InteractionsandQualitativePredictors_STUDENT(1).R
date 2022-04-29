############################################################
#
#PA 541 ADVANCED DATA ANALYSIS
#MICHAEL D. SICILIANO
#WEEK 7: INTERACTIONS & QUALITATIVE PREDICTORS
#
############################################################

#Set WD

#add libraries
library(car)
library(tidyverse)
library(haven)
library(texreg) #useful package for formatting your output


###############################################################


wage1=read_spss("wage1_wooldridge.sav")
wage1 #check out the dataset
#Data Description
# 1. wage                     average hourly earnings
# 2. educ                     years of education
# 3. exper                    years potential experience
# 4. tenure                   years with current employer
# 5. nonwhite                 =1 if nonwhite
# 6. female                   =1 if female
# 7. married                  =1 if married
# 8. numdep                   number of dependents
# 9. smsa                     =1 if live in SMSA
# 10. northcen                 =1 if live in north central U.S
# 11. south                    =1 if live in southern region
# 12. west                     =1 if live in western region
# 13. construc                 =1 if work in construc. indus.
# 14. ndurman                  =1 if in nondur. manuf. indus.
# 15. trcommpu                 =1 if in trans, commun, pub ut
# 16. trade                    =1 if in wholesale or retail
# 17. services                 =1 if in services indus.
# 18. profserv                 =1 if in prof. serv. indus.
# 19. profocc                  =1 if in profess. occupation
# 20. clerocc                  =1 if in clerical occupation
# 21. servocc                  =1 if in service occupation
# 22. lwage                    log(wage)
# 23. expersq                  exper^2
# 24. tenursq                  tenure^2

#Let's start with a null model
mod1 = lm (wage ~ 1, data=wage1)
summary(mod1)

mean(wage1$wage)#note this matches the intercept term
sd(wage1$wage)#note this matches the residual standard error (average error of
#our prediction, and right now our prediction is just the mean)

#Let's add a predictor to our model
mod2 = lm(wage ~ educ, data=wage1)
summary(mod2)

#Let's add a secod predictor
mod3 = lm(wage ~ educ + exper, data=wage1)
summary(mod3) #note we see the size of the coefficient on education has changed
#it changes due to its correlation with exper.
cor(wage1$educ, wage1$exper)

#here is a nice tool for formatting your regression tables
#can use htmlreg if you want to export it to place in a word 
#document, but can use screenreg to format it in our console output
# htmlreg(list(mod1, mod2, mod3), single.row=TRUE, digits=3, 
#         stars = c(0.01, 0.05, 0.1), file="mod123.doc")
screenreg(list(mod1, mod2, mod3), single.row=TRUE, digits=3, 
        stars = c(0.01, 0.05, 0.1))

#Let's center our predictors and rerun the models
wage1$educ12 = wage1$educ - 12
wage1$exper10 = wage1$exper - 10

mod1c = lm (wage ~ 1, data=wage1)
summary(mod1c)

mod2c = lm(wage ~ educ12, data=wage1)
summary(mod2c)

mod3c = lm(wage ~ educ12 + exper10, data=wage1)
summary(mod3c) 

screenreg(list(mod1c, mod2c, mod3c), single.row=TRUE, digits=3, 
        stars = c(0.01, 0.05, 0.1))

#as an aside, you can create new variables in your model syntax
modx = lm(wage ~ I(educ-12), data=wage1)
summary(modx)

#Lets explore an interaction term.  I'm going to use tenure now instead
#of experience, because the interaction is more pronounced.
mod4 = lm(wage ~ educ12 + tenure + educ12*tenure, data=wage1)
summary(mod4) 


#---Graphically displaying interaction effects between two continuous variables

#Because we have a continuous variable it is useful to look at high, medium, and low
#levels of each.  often researchers will take -1sd, the mean, and +1sd to obtain
#these levels.  Becuase we are using education, we will look at those
#with no education, those with a high school degree, and those with a college degree

newdata2 = expand.grid(educ12= c(-12, 0, 4),
                       tenure = c(0:25) )

#expand grid is nice function to create a new dataset.  What we have done is 
#develop a dataset for the particular set of individuals we want to examine
#the effect of tenure given the interaction in the model. Because education
#is doing the moderating, education should be used as the grouping
#variable in the visualization (as we will see below)
head(newdata2)

newdata2$preds = predict(mod4, new=newdata2, type="response")

head(newdata2)

ggplot(newdata2, aes(x=tenure, y=preds, color=factor(educ12))) + 
  geom_point(size = 3) 


##-----------INTERACTIONS USING THE GGEFFECTS PACKAGE

#read more about the package here: https://strengejacke.github.io/ggeffects/articles/ggeffects.html

mydf.mod4 = ggpredict(mod4, terms = c("tenure","educ12 [-12, 0, 4]"))
mydf.mod4

#now you can plot aas you normally would and have access to all ggplot
#features
ggplot(mydf.mod4, aes(x = x, y = predicted, colour = group)) +
  geom_line()




#################################################################
# QUALITATIVE PREDICTORS
#################################################################


summary(wage1$wage)
summary(wage1$Male)

wreg1 = lm(wage1$wage ~ Male, data=wage1)
summary(wreg1)


#Visual representation of the model

ggplot(wage1, aes(x = factor(Male), y = wage, fill = factor(Male))) + 
  geom_violin() + 
  stat_summary(fun = mean, geom="point", shape=18,
               size=4)

wreg2 = lm(wage1$wage ~ Male + exper, data=wage1)
summary(wreg2)

 
#Dummy Variable Trap
wreg3 = lm(wage1$wage ~ Male + female, data=wage1)
summary(wreg3)

#----Using Qualitative Variables with More than Two Categories

wreg4 = lm(wage ~ south + west + northcen + exper, data=wage1)
summary(wreg4)



#-----Varying slopes models
wreg5 = lm(wage ~ Male + exper + Male*exper, data=wage1)
summary(wreg5)


#visualization of the raw data (which suggests the need for an
#interaction effect)

ggplot(data=wage1, aes(x=exper, y=wage, colour=factor(Male))) + 
  geom_point() + stat_smooth(method="lm")

#Two categorical variables in one model
wreg8 = lm(wage ~ Male + white + exper, data=wage1)
summary(wreg8)

#Interaction with two categorical variables
wreg8 = lm(wage ~ Male + white + exper + Male*white, data=wage1)
summary(wreg8)



#####################################################################
#IN-Class Exercise
#####################################################################

# Using the ‘wages’ dataset we used above, run a model predicting wage, 
# by educ, exper, white, male, and tenure.

wreg9 = lm(wage ~ Male + white + exper + educ + tenure, data=wage1)
summary(wreg9)

# Interpret the results.


# What is the expected wage of a white, female, with 8 years of educ, 
# and 0 years of exper and tenure?


# What is the expected wage of a white, female, with 9 years of educ, 
# and 0 years of exper and tenure?


# Create an interaction effect between tenure and white.


# Interpret the results.


# Draw a diagram by hand displaying the results of the interaction 
# (regardless of whether or not the interaction is significant).