######################################################
#
#  PA 541 Week 14 - Panel Data, Fixed Effects Models
#  Michael D. Siciliano
#
#####################################################

library(tidyverse)
library(sandwich)
library(lmtest)
library(plm)
library(pscl)

#############################################################
# WIDE TO LONG DATA
#############################################################

# Before jumping into the models, let me provide a quick tutorial on how to
# manage panel data.  You will want to be able to transform your data from
# wide to long and back again (depending on your task).  Generally, data
# for panel data analysis should be in long format. 

# We will use two functions from tidyr pivot_wider and pivot_longer

# For a full tutorial you can go here: https://tidyr.tidyverse.org/articles/pivot.html
# For some other advanced use, see: https://dcl-wrangle.stanford.edu/pivot_advanced.html


# Here is a wide dataset
nlsy=read_dta(file="nlsy_inclass.dta")
head(nlsy)
# let's also add a id number here so we can track our data as we change its structure
nlsy$id = 1:length(nlsy$momage)
nlsy

# let's also put id as the first column
nlsy = nlsy %>% select(id, everything())
nlsy

# note that we have measure for anti, self, and pov for two different years but
# those values are all listed in same row.  Ideally we want a unique row for
# each year.  Here is how you can do this.

#let's start with a smaller dataset first

nlsy.samp = nlsy %>% dplyr::select(id, momage, anti90, anti94, childage)
nlsy.samp

nlsy.samp %>% pivot_longer(cols = c(anti90, anti94),
                           names_to = "year",
                           values_to = "anti")

# basically, what we want, but I would like to fix the year column so it
# just shows the year

nlsy.samp %>% pivot_longer(cols = c(anti90, anti94),
                           names_to = "year",
                           names_prefix = "anti",
                           values_to = "anti")

# year is stored as a character here...so let's do one more thing
# and indicate that year should be numeric
final = nlsy.samp %>% pivot_longer(cols = c(anti90, anti94),
                                   names_to = "year",
                                   names_prefix = "anti",
                                   values_to = "anti") %>%
  mutate(year = as.numeric(year))

final


# Ok, now that we did that with a subset of the dataset, lets try to
# do the same thing but now with all variables that have two observations
# so will transform, anti, self, and pov

# We'll need to make the following two changes to our pivot_longer() call:
#   
# Change names_to to be a vector of two names: c("measure", "year"). 
# This will tell pivot_longer() to create two variables from the column names instead of one.
# Use the names_sep argument to tell pivot_longer() what separates an measure value 
# from a year in each of the column names.  You could also add the prefix and ptypes
# here...I left them off to keep the code cleaner and focus on the main functions.

nlsy.l = nlsy %>% pivot_longer(cols = c(anti90, anti94, self90, self94, pov90, pov94),
                               names_to = c("measure", "year"),
                               names_sep =  "(?<=[A-Za-z])(?=[0-9])",
                               values_to = "value")
nlsy.l

# what we are left with now is close, but we don't want each measure to be in a separate row
# rather we want a year column, and then have the variable names for anti, self, and pov

# to fix this, we need to pivot_wider

full = nlsy.l %>% pivot_wider(names_from = measure, values_from = value)
full

# Whew...it took me awhile to figure this out for this dataset. The approach
# to making tidy datasets has changed a lot in the past five years
# The verbs pivot_longer() and pivot_wider() seem like the best approach.
# You may identify better solutions using these verbs than my approach above
# ...if so please share with me.
# Thankfully, most datasets we encounter will look like nlsy so you
# adopt the strategies for other contexts. 

###############################################################################
# PANEL DATA                  
# Differencing Methods with two time periods
###############################################################################

#Effect of crime on unemployment
# CRIME2.DES
# 
# pop       crimes    unem      officers  pcinc     west      nrtheast  south    
# year      area      d87       popden    crmrte    offarea   lawexpc   polpc    
# lpop      loffic    lpcinc    llawexpc  lpopden   lcrimes   larea     lcrmrte  
# clcrimes  clpop     clcrmrte  lpolpc    clpolpc   cllawexp  cunem     
# clpopden 
# lcrmrt_1  ccrmrte   
# 
# Obs:    92
# 
# 1. pop                      population
# 2. crimes                   total number index crimes
# 3. unem                     unemployment rate
# 4. officers                 number police officers
# 5. pcinc                    per capita income
# 6. west                     =1 if city in west
# 7. nrtheast                 =1 if city in NE
# 8. south                    =1 if city in south
# 9. year                     82 or 87
# 10. area                     land area, square miles
# 11. d87                      =1 if year = 87
# 12. popden                   people per sq mile
# 13. crmrte                   crimes per 1000 people
# 14. offarea                  officers per sq mile
# 15. lawexpc                  law enforce. expend. pc, $
# 16. polpc                    police per 1000 people
# 17. lpop                     log(pop)
# 18. loffic                   log(officers)
# 19. lpcinc                   log(pcinc)
# 20. llawexpc                 log(lawexpc)
# 21. lpopden                  log(popden)
# 22. lcrimes                  log(crimes)
# 23. larea                    log(area)
# 24. lcrmrte                  log(crmrte)
# 25. clcrimes                 change in lcrimes
# 26. clpop                    change in lpop
# 27. clcrmrte                 change in lcrmrte
# 28. lpolpc                   log(polpc)
# 29. clpolpc                  change in lpolpc
# 30. cllawexp                 change in llawexp
# 31. cunem                    change in unem
# 32. clpopden                 change in lpopden
# 33. lcrmrt_1                 lcrmrte lagged
# 34. ccrmrte                  change in crmrte

crime=read_csv("crime2.csv")
crime

#let's look at the relationship in just 1987
crime2=filter(crime, year==87)#subset the data into a new file with just 1987 data
crime2
crm1=lm(crmrte ~ unem, data=crime2)
summary(crm1)

#let's look at the relationship in just 82
crime2b=filter(crime, year==82)#subset the data into a new file with just 1982 data
crime2b
crm1b=lm(crmrte ~ unem, data=crime2b)
summary(crm1b)

#Let's now simply pool the data together and use standard OLS
crime$d87.2=ifelse (crime$year==87,1,0)#though it was already made in the dataset, here
#is how I would create a simple year dummy variable

crm2=lm(crmrte ~ d87 + unem, data=crime)
summary(crm2)


#Run a first differenced model
#two delta variables were stored as characters...need to change to numeric as shown below
#the first time period under differencing has no value, so it is na.  these values were
#stored originally as '.' and the warning is indicating those were forced to NAs. 
crime$cunem=as.numeric(crime$cunem)
crime$ccrmrte=as.numeric(crime$ccrmrte)
crm3=lm(ccrmrte ~ cunem, data=crime)#note only half the observations get used because there is only
#a delta variable for the rows for 1987.  Note the intercept in this model is
#is simply the expected change in crime between 82 and 87.
summary(crm3)

###############################################################################################
#   ADVANCED PANEL DATA METHODS -
# - Wooldridge Chapter 14 - FIXED EFFECTS 
###############################################################################################
##FIXED EFFECTS ON NCDATA

nccrime=read_dta("CRIME4.dta")

nccrime


#Note it is a good habit to always set your unit and and time variables
#when working with panel data data and planning to use the plm package
#let's do this now

plm.nc=pdata.frame(nccrime, index=c("county","year"))

#fixed effects with year dummies
fem1b=plm(lcrmrte ~ d82 + d83 + d84 + d85 + d86+ d87 + lprbarr + lprbconv +
            lprbpris + lavgsen + lpolpc, data=plm.nc, model="within")
summary(fem1b)

#twoways approach; no year dummies (as it accounts for them)
fem1b2=plm(lcrmrte ~  lprbarr + lprbconv +
             lprbpris + lavgsen + lpolpc, data=plm.nc, model="within", effect = "twoways")
summary(fem1b2)

#LSDV approach
fem1b3=lm(lcrmrte ~  as.factor(county) + as.factor(year) + lprbarr + lprbconv +
            lprbpris + lavgsen + lpolpc, data = nccrime)
summary(fem1b3)

#In light of the new results in Stock and Watson,
#"Heteroskedasticity-robust standard errors for fixed-effects
#panel-data regression," Econometrica 76 (2008): 155-174.
#It is generally a good idea to use cluster robust standard errors for fe
#xtreg, fe in Stata now uses vce(cluster id) when vce(robust) is specified


fem.cl = vcovHC(fem1b, type="HC1", cluster = "group") #cluster errors by group; note
#we told R that the dataset pnccrime is indexed by county and year and thus
#clustering by group means that we are clustering by county
#Note, if you don't enter a 'type', the default is HCO, and thus if you just specify
#the cluster, you will get the cluster-robust standard errors in R.  You can use
#'type' to specify other versions of HC standard errors.  Again, stata uses HC1
coeftest(fem1b, fem.cl) 



##################################################################
# RECREATING CAMERON AND TRIVEDI CHAPTER 21
##################################################################

load("mom.RData")
mom

#using this data we want to look at the relationship between lnhr and lnwg
#through a variety of methods:
#1. POLS
#2. Within
#3. Between
#4. FD



#1. -----Pooled OLS model------------------

#let's use the plm package to do this

#For PLM, as noted above, it is best to create a panel data frame using
#pdata.frame and indicate your unit and time indices
p.mom = pdata.frame(mom, index=c("id", "year"))

mod1b = plm(lnhr ~ lnwg, data=p.mom, model="pooling")
summary(mod1b)#matches our OLS model
#now if we want cluster-robust standard errors in PLM we 
#can use the following (this controls for serial correlation
#that is likely present because group is the indivdial and we 
#observe that individual over time)
coeftest(mod1b,vcov=vcovHC(mod1b,cluster="group"))
#note, there are several vcovHC functions in r, the one we
#used in a prior lecture to deal with heteroskedasticit only
#came from the sandwich package, in PLM the same function is used
#to get cluster-robust estimates.  Because PLM uses the same function
#and PLM was called in second from our library, it masks the sandwich
#function and thus the vcovHC is the one from plm.  You need to state
#sandwich::vcovHC to get the one from sandwich now.

#--note time constant predictors in pooled ols are ok.
#we cannot do this for fd or fe models though
mod1c = lm(lnhr ~ lnwg + kids + disab + as.factor(year), data=mom)
summary(mod1c)


#2. ------Between Estimate-----------------------
#you could produce these results by taking the average of each
#measure for each person, and then regressing the averages on each other
#because each person has 10 observations, this means you would have
#532 total observations (just 1 for each person now)  We will use the
#PLM package to estimate this:

mod2 = plm(lnhr ~ lnwg, data=p.mom, model="between")
summary(mod2)
#if we wanted to deal with heteroskedasticity you would
#need to run the model using the lm function and then
#as for the heteroskedastic-consistent errors from the sandwich package


#3.--------Within Estimate-----------------------
mod3 = plm(lnhr ~ lnwg, data=p.mom, model="within")
summary(mod3)
#now if we want cluster-robust standard errors in PLM we 
#can use their following
coeftest(mod3,vcov=vcovHC(mod3,cluster="group"))#matches Cameron and Trivedi


#a note about the intercept or constant term:
# following is from: http://r.789695.n4.nabble.com/I-need-intercept-in-plm-model-td4654212.html
# a fixed effects (within) model does not have a single intercept: it has
# one for each individual
# 
# Some confusion often stems from Stata (misleadingly, IMHO) reporting an
# "intercept" which is actually the average of the individual intercepts,
# which you can recover in R as 'mean(fixef(<yourmodel>))'. Remember,
# though, that this isn't "the intercept" of the estimated model in the
# usual sense of the term. If you "need" an intercept, then you must
# review your specification. 

mean(fixef(mod3)) #this matches Cameron and Trivedi 



#4.--------First Difference-----------------------
mod4 = plm(lnhr ~ lnwg, data=p.mom, model="fd")
summary(mod4)
#now if we want cluster-robust standard errors in PLM we 
#can use their following
coeftest(mod4,vcov=vcovHC(mod4,cluster="group"))#matches Cameron and Trivedi




###############################################################################
# TRAFFIC FATALITIES AND BEER TAX - EXAMPLE
###############################################################################

fatality <- read_dta("fatality.dta")
fatality$y <- fatality$mrall*10000
fatality$state_fip = factor(fatality$state)
summary(fatality$y)
fatality

#let's plot the data
ggplot(fatality, aes(x = year, y = y, color = state_fip)) + geom_point() + geom_line() +
  facet_wrap(~as.factor(state)) + theme(legend.position = "none")

###Let's begin with a simple model, fitting only a state-specific mean to each state's fatality rate

m1=lm(y ~ -1 + state_fip, data=fatality)
summary(m1)$r.squared #so state predicts a good deal of variation in the fatality rate

###Naive estimate of the effect of beer tax
m2=lm(y ~ beertax, data=fatality)
summary(m2) #get odd result that higher beer tax increases fatalities
#We can plot this relationship on an unconditional scatterplot. Again, ignoring the grouping by state

ggplot(fatality, aes(x = beertax, y = y) ) + geom_point() + geom_smooth(method = "lm", se = F) + 
  labs(title = "Beer Taxes and Traffic Fatalities",
       subtitle = "1982 to 1988",
       x = "Beer Tax",
       y = "Vehicle Fatality Rate per 10,000 people", 
       caption = "fatality data set")

#What is going here? We are not accounting for any unobserved heterogeneity.  The fixed effect of
#a state may be particularly relevant if the data display a good deal of cross-sectional or between
#variation, and the Xit available for analysis do a poor job of soaking up the cross-sectional variation.
#We might ignore the fixed state effects if such things were assigned randomly or we can assume they
#are not correlated with our predictors.

###Fixed Effects Estimate
plm.fatality=pdata.frame(fatality, index=c("state", "year")) 

m3=plm(y ~ beertax, data=plm.fatality, method="within")
summary(m3)
#Note the striking change in the estimated effect of beer tax; in the presence of fixed effects for
#state, each dollar-per-case increase in beer-tax is estimated to reduce traffic fatalities by about
#.66 per million residents.

#We can plot the relationship between the fixed effects and the average beer tax in the state
#To see why we need a fixed effects model

#First we will calculate the average beer tax
fatality = fatality %>%
  group_by(state) %>%
  mutate(beerTaxAverage = mean(beertax)) %>%
  ungroup()

#Then we will calculate the state fixed effect and put 
#those values in a new data frame
fixed1 <- lm(y ~ -1 + factor(state) + beertax,
             data=fatality)

stateFE = tibble(
  state_fip = unique(fatality$state_fip),
  state_fixed_effects = coef(fixed1)[1:48],
  state_abbr = names(attr(fatality$state, "labels"))
)

stateFE
#merge the state fixed effects with the original fatality data
fatality2 = left_join(fatality, stateFE, by = "state_fip")

#make a plot of the state fixed effects by average beer taxes
#instead of using geom_point(), we will use the state_abbr as
#the point through the use of geom_text()
fatality2 %>% distinct(state_fip, .keep_all = T) %>%
  ggplot(aes(x = beerTaxAverage, y = state_fixed_effects)) +
  geom_text(aes(label=state_abbr))

