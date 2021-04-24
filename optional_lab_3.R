# we will work with two datasets today: the gapminder which is within R and 
# ceosal1 from the book 
setwd("C:/Users/Evgenia/Desktop/TA - 541/optional labs")



library(gapminder)


gap <-  gapminder

gap


# Normally, we would start with plots between the dependent variable and the focal variables
# Today we will skip this part 

# Here the dependent will be life expectancy 
# We will start with a simple model with just the dependent variable
# What does the intercept mean here? 

mod1 <- lm(lifeExp ~ 1, data = gap)
summary(mod1)

# Intercept is the mean of life expectancy 
mean(gap$lifeExp)


# Let's go to a regression with one independent variable 
# We will add year, which a continuous variable

mod2 <- lm(lifeExp ~ year, data = gap)
summary(mod2)

# Coefficient for year: For each additional year, life expectancy increase by .32 years. The relationship is significant (p < .001)
# Intercept: life expectancy when the year is 0
# It is negative, which does not make sense because we have data starting at 1952
# If we make year a categorical variable the intercept would be meaningful 

gap$year_cat <- factor(gap$year)

mod3 <- lm(lifeExp ~ year_cat, data = gap)
summary(mod3)

# Now the reference group is 1952
# Intercept: In 1952 on average life expectancy was 49 years
# year_cat1957: The life expectancy in 1957 was 2.5 years more compared to 1952. The effect is not significant at .05 level, but it is significant at .01 level
# year_cat1962: The life expectancy in 1962 was 4.5 years more compared to 1952. The effect is significant at .001 level
# year_cat1967: The life expectancy in 1967 was 6.6 years more compared to 1952. The effect is significant at .001 level and so on

# alternatively, if you can recode the variable year so 0 would be meaningful
# in this case, 0 would be 2007. Note that compared to mod2 the intecept has changed
# and it is meaningful but the coefficient for the year has not changed. 
gap$year_c <- gap$year - 2007
mod <- lm(lifeExp ~ year_c, data = gap)
summary(mod)

# Now let's build on mod2 and add population 
mod4 <- lm(lifeExp ~ year + pop, data = gap)
summary(mod4)

# Intercept: the expected life expectancy in year 0 and population 0 
# year: for every additional year, life expectancy increases by 0.32 years. The effect is significant (p<.001)
# population: for every additional person in the population of a country, the life expectancy goes up by 0.00000003. The effect is significant (p<.001)
# for the population it would make more sense to recode by 100k or a million 

gap$pop_100k <- gap$pop / 100000


mod5 <- lm(lifeExp ~ year + pop_100k, data = gap)
summary(mod5)

# As you can see for this model, the intercept and the coefficient did not change
# The coefficient for population moved five decimal points

# Now let's add a categorical variable

mod6 <- lm(lifeExp ~ year + pop_100k + continent, data = gap)
summary(mod6)

# categorical variables always have a reference group
# If you do not specify the reference group, it will be the one in alphabetical order
# If you want to change the reference group, you can do so with the command relevel
# here is an example of how you can make Americas the reference group
# gap$continent_new <- relevel(gap$continent, ref = "Americas")

# Intercept: the expected life expectancy for our reference group (0 year, 0 population, in Africa)
# continent Americas: In America, the life expectancy is 15.8 years higher compared to Africa. The effect is significant (p<.001)
# continent Asia: In Asia, the life expectancy is 11.2 years higher compared to Africa. The effect is significant (p<.001) and so on 



# let's go to a different dataset
load("ceosal1.RData")#note it saves the file as 'data'
#let's rename this to avoid confusion in the future

ceo=as_tibble(data)

ceo

# salary: 1990 salary, thousands $
# pcsalary: percent change salary, 89-90
# sales: 1990 firm sales, millions $
# roe: return on equity, 88-90 avg
# pcroe: percent change roe, 88-90
# ros: return on firm's stock, 88-90
# indus: =1 if industrial firm
# finance: =1 if financial firm
# consprod: =1 if consumer product firm
# utility: =1 if transport. or utilties
# lsalary: natural log of salary
# lsales: natural log of sales


#Let's run a simple regression
mod7=lm(salary ~ sales, data=ceo)
summary(mod7)

# Intercept: The expected salary of a ceo when the sales are 0 
# sales: for a one million increase on sales, the salary increases by $1,547

# Let's add some more independent variables 

mod8=lm(salary ~ sales + pcsalary + finance, data=ceo)
summary(mod8)

# Intercept: the expected salary when the sales are 0, the percent change on salary is 0, 
# and it is not an industrial firm 

#sales: for a one million increase on sales, the salary increases by $15.6. 
# The effect is not significant at .05 level (p < .05)
# pcsalary: 1% change on salary, increases the salary by $400
# The effect is not significant at .05 level (p < .05)
# finance: The salary of a ceo at an financial firm is $106,400 more compared to nonfinancial firms 
# The effect is not significant at .05 level (p < .05)


# Let's add some more variables 
mod9=lm(salary ~ sales + pcsalary + finance + roe + pcroe, data=ceo)
summary(mod9)

# here we can see the roe and pcroe (return on equity and percent change roe)
# do not have a significant effect. 
# But do they have a joint significant effect? 

# mod8 is the restricted model and mod9 the unrestricted

# we need to examine the F statistic 
# there are two ways to examine this: we will show the ANOVA tests
# the null hypothesis is there is no significant difference between the two models 
# We will test the hypothesis

anova(mod8, mod9)

# In general, a small F value indicates that we fail to reject the null hypothesis
# However, we need to be more precise and find the critical value
# When CV < F we reject teh null hypothesis
# In this case, there are not significant differences so the two variables do not have a joint effect

