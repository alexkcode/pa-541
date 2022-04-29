#set working directory
setwd("~/Desktop/UIC/PhD/Course Material/Spring 2021/TA - PA 541/Optional Lab 4")

#libraries
library(tidyverse)
library(haven)
library(texreg) #useful package for formatting your output

#read in dataset
load("bwght.RData")

weight = as_tibble(data)

weight

#####INTERACTIONS#####

###continuous-continuous variables###
mod1 = lm(bwght ~ motheduc + faminc, data=weight)
summary(mod1)

mod2 = lm(bwght ~ motheduc + faminc + motheduc*faminc, data=weight)
summary(mod2)
#assume education is the moderator here.

#interpretations
#intercept: It is the birth weight of a child whose mother has no education and family income is $0.

#motheduc: Holding family income at $0, for each additional year of mother education, the birth weight of child increases by 1.37 ounces. This is conditional effect.

#faminc: Holding mother education at 0, for each additional $1000 increase in income, the birth weight of child increases by 0.61. This is conditional effect.

#motheduc*faminc: For each additional year of mother education, the effect of family income decreases by 0.037 ounces. For higher levels of education, each additional $1000 in family income decreases birth weight by 0.037 ounces.

###continuous-qualitative variables###
mod3 = lm(bwght ~ faminc + white, data=weight)
summary(mod3)

mod4 = lm(bwght ~ faminc + white + faminc*white, data=weight)
summary(mod4)
#assume race moderates the effect of famincome on bwght.

#interpretations
#intercept: It is the birth weight of a non-white child with family income $0.

#faminc: When family income increases by $1000, the birth weight of non-white child increases by 0.02 ounces. This is conditional effect.

#white: For a white child with family income of $0, the birth weight is 3.60 ounces more than the weight of a non-white child with family income of $0. This is conditional effect.

#faminc*white: For a white child, increase in family income additionally increases the birth weight by 0.075 ounces. Incremental change in the slope of family income for white children.


###qualitative-qualitative variables###
mod5 = lm(bwght ~ faminc + white + male, data=weight)
summary(mod5)

mod6 = lm(bwght ~ faminc + white + male + white*male, data=weight)
summary(mod6)
#assume race is the moderator here. 


#interpretations
#intercept: It is the birth weight of female, non-white child with family income $0.

#faminc: When family income increases by $1000, the birth weight of increases by 0.09 ounces. This is not a conditional effect because it is not involved in the interaction. 

#white: For a white female child, the birth weight is 5.476 ounces more than the weight of a non-white female child. 

#male: For a male, non-white child the birth weight is 3.65 ounces more than the female, non-white child. 

#white*male: The interaction tells us if there is a different effect of male on birth weight between white and nonwhites. 
#We find the effect of being male on the birth weight is 0.59 ounces less if the male is white compared to nonwhite. 

#One way to see this is to create two observations that differ only in race.  We can set income
#to 20k here, but it could be anything.

#White-male
obs.a = c(1,20,1,1,1)
obs.a.weight = crossprod(obs.a, mod6$coefficients)
obs.a.weight

#nonwhite-male
obs.b = c(1,20,0,1,0)
obs.b.weight = crossprod(obs.b, mod6$coefficients)
obs.b.weight

obs.a.weight - obs.b.weight # 4.877

#Where does that value come from?  Well it is the impact of being white for males.
#that impact is the simple effect of white plus the interaction
5.46269 + -0.58546
