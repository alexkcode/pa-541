######################################
# In Class Exercises - Week 4 Solutions
#####################################

#Be sure to set your wd and read in the libraries you need


#Problem 1: Using the prestige dataset regress prestige on education, income, and women.
# Prove to yourself that the coefficient on education is the partialled out effect of education on prestige. 
# Two steps needed to do this:
#   1) Regress the explanatory variable on all other explanatory variables
# 2) Regress  Y on the residuals from this regression

lm.part = lm(prestige ~ education + income + women, data = prest)
summary(lm.part) #note the coefficient is 4.187

lm.educ = lm(education ~ income + women, data=prest)
prest$educ.resid = resid(lm.educ) #calculate the residuals and at them as a new
#variable to the dataframe prest

lm.prest = lm(prestige ~ educ.resid, data = prest)
summary(lm.prest) #note we get the same coefficient

lm.prest = lm(prestige ~ educ.resid + income + women, data = prest)
summary(lm.prest)#if we add these in, notice what happens to income and women,
#their coefficients and significance are different.  This is because th parts
#of these variables that were correlated with education, and hence before not
#getting credit for its association with income now are.  Because we 'cleaned'
#education of its association with those predictors, they now get credit for 
#more predictive area than they did before.  Notice however, the overall 
#significance of the model and its R-squared do not change.

#----Wooldridge Computer Exercise 1:
#only part three requires us to use R

load('bwght.RData')
bwght = data #remember the wooldridge files all enter with the name 'data'
#so we want to rename them to avoid any confusion

lm.with = lm (bwght ~ cigs + faminc, data=bwght)
summary(lm.with)
lm.without = lm (bwght ~ cigs, data=bwght)
summary(lm.without)

# The effect of cigarette smoking is slightly smaller when faminc is added 
# to the regression, but the difference is not great. This is due to the fact
# that cigs and faminc are not very correlated, and the coefficient on faminc
# is practically small. (The variable faminc is measured in thousands, so $10,000 
# more in 1988 income increases predicted birth weight byonly .93 ounces.)


#----Wooldridge Computer Exercise 2:

load("hprice1.RData")
house = data

lm.house = lm(price ~ sqrft + bdrms, data = house)
summary(lm.house)


#(ii) Holding square footage constant,  change in price = 15.20 X change in bdrms, 
#and so price increases  by 15.20 which means $15,200.

# (iii) Now change in change in price = .128 X change in sqrft + 15.20 X change in bdrms
# = .128(140) + 15.20 = 33.12, or $33,120. 
# Because the size of the house is increasing, this is a much larger effect than in (ii).

# (iv) About 63.2%.

# (v) The predicted price is -19.32 + .128(2,438) + 15.20(4) = 353.544, or $353,544.

# (vi) From part (v), the estimated value of the home based only on square footage and
# number of bedrooms is $353,544. The actual selling price was $300,000, which suggests 
# the buyer underpaid by some margin. But, of course, there are many other features of a 
# house (some that we cannot even measure) that affect price, and we have not controlled 
# for these.
