###########################################################
#
#PA 541 ADVANCED DATA ANALYSIS
#MICHAEL D. SICILIANO
#WEEK 10 - Specification, Outliers, Missing Data
#
###########################################################


library("norm") #will use for expectation maximization when dealing with missing data
library("Amelia")#will use for multiple imputation when dealing with missing data
library("Tidyverse")
library("texreg")

#############################
#Log Models
#############################
load("crime.RData")

#LOG-LOG Model
logm1 = lm(lcrime ~ lenroll, data=crime)
summary(logm1)

#Let's calculate the exact percentage change
#what we need to do now is look at a 1% change in your level of
#enrollment.  So let's say we are currently at 5000 students, a 
#1% increase would bring us to 5050 students
a=-6.6279 + 1.2693*log(5000)
b=-6.6279 + 1.2693*log(5000+ .01*5000)
#Now both a and b are in log units, we need to exponentiate to get
#them back to original units.
e.a = exp(a) #= 65.56
e.b = exp(b) # = 66.39
(e.b-e.a)/e.a 
#If we multiply by 100 we get the percent increase in the outcome 
#variable (on the scale of the outcome variable).  So we say that a 
#1% change in enrollment leads to a 1.27% change in crime rate
#Again, we need to multiply by 100, simply because this is how
#we calculate a percentage change.  It is (new-old / old) * 100 


#LOG-Lin Model
logm2 = lm(lcrime ~ enroll, data=crime)
summary(logm2)
#Let's calculate the exact percentage change
a = 3.94 + .00008293*1000 #calculate the expected lcrime at 1000 students
e.a = exp(a) #= 55.86455
b = 3.94 + .00008293*1001 #calculate the expected lcrime at 1001 students
e.b = exp(b) # = 55.86918
(e.b-e.a)/e.a # calculate the percentage change between those two values
#if we multiply by 100 we get the percent increase in the outcome 
#variable (on the scale of the outcome variable)
#So here we would say a one UNIT increase in enrollment leads to a 
#.00829% increase in crime.  A really really small amount, but then again
#we are talking about increasing enrollment by only 1 person.


#LIN-LOG Model
logm3 = lm(crime ~ lenroll, data=crime)
summary(logm3)

#Let's calculate the exact percentage change
#what we need to do now is look at a 1% change in your level of
#enrollment.  So let's say we are currently at 5000 students, a 
#1% increase would bring us to 5050 students
a = -3358.53 + 400.38*log(5000)
b = -3358.53 + 400.38*log(5000 + .01*5000)
#Now in this model, we interpret the percentage change in the predictor
#on the unit change in crime.  Thus all we need to do is take the following:
b-a #and we get 3.98; approximately B1/100 unit change.

#simple linear model to compare 
logm4 = lm(crime ~ enroll, data = crime)

screenreg(list(logm4, logm1, logm2, logm3), 
          custom.model.names = c("Linear", "Log-Log", "Log-Lin", "Lin-Log"))



################################
# OUTLIERS
###############################

#Example 1 - Wooldridge

load("RDCHEM.RData")
chem = as_tibble(data)
chem


#Let's assume we want to predict r&d intensity (defined as the total r&d 
#expenditures as a percentage of sales) based on the sales (in millions) and 
#profits as a percentage of sales.

out1 = lm(rdintens ~ sales + profmarg, data=chem)
summary(out1)

#Take a look a the raw data to see if there are any potential flags
#in regards to outliers

ggplot(data = chem, aes(x=sales, y=rdintens)) + geom_point() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))

ggplot(data = chem, aes(x=profmarg, y=rdintens)) + geom_point() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))

#create a dummy for observation with largest sales
chem$dum = ifelse(chem$sales==max(chem$sales), 1, 0)

out2 = lm(rdintens ~ sales + profmarg + dum, data=chem)
summary(out2) 

#let's plot the studentized residuals
chem$rstudent = rstudent(out1)

ggplot(data = chem, aes(x=as.numeric(row.names(chem)), y=rstudent)) + geom_point()  +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))
#based on the plot it looks like perhaps only one observation is concerning, let's
#drop this as well as the extreme sales observation and rerun our model

chem2 = chem
chem2 = chem2[which(!chem2$rstudent>4),]#one way to to this if you know there
#is only one outlier with a value greater than 4...other methods are to use
#the max function along with abs (absolute value) function to identify the 
#largest outlier.  I take this approach in the second example below using
#the cars dataset
chem2 = chem2[which(!chem2$sales==max(chem2$sales)),]

out3 = lm(rdintens ~ sales + profmarg, data=chem2)
summary(out3) 


#worth noting that Wooldridge found the best fit with a log transformation:

out3 = lm(lrd ~ lsales + profmarg, data=chem)
summary(out3)

#let's take a look at the relationship between the rd and sales
ggplot(data = chem, aes(x=sales, y=rdintens)) + geom_point()
ggplot(data = chem, aes(x=sales, y=lrd)) + geom_point()
ggplot(data = chem, aes(x=lsales, y=lrd)) + geom_point()#nice straight line relation here

#Example 2 - Cars data

cars = read_csv(file = "cars5.csv")
head(cars)

cars$Cgphm = 100/cars$Cmpg
hist(cars$Cgphm)

cm1 = lm(Cgphm  ~ Eng + Cyl + Vol, data = cars)
summary(cm1)

#let's check our residuals for any outliers
cars$rstudent = rstudent(cm1)

ggplot(data = cars, aes(x=as.numeric(row.names(cars)), y=rstudent)) + geom_point()  +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

cars.b = cars #create a duplicate dataset
cars.b = cars.b[which(!abs(cars.b$rstudent)==max(abs(cars.b$rstudent))),]

cm2 = lm(Cgphm  ~ Eng + Cyl + Vol, data = cars.b)
summary(cm2)

###############################
# MISSING DATA
###############################


#Let's examine some missing data techniques in the context
#of a basic regression analysis

#here will you use data from Cohen et al. (2003) classic text

cite = read.table("Cohen_ch11_data.txt", header=T, na.strings="NA")
head(cite)
tail(cite)

#-----Listwise Deletion
#note, most authors state that pairwise deletion is never a good
#idea, some call it 'unwise' deletion.
#In the presence of missing data, most statistical packages
#use listwise deletion which removes any row that contains a missing
#value from the analysis.
#by default, R engages in Listwise deletion
lm1 = lm(SALARY ~ PUB + CITM, data=cite)
summary(lm1) #here we see 7 observations deleted due to missingness

#take a look at a scatterplot
ggplot(data = cite, aes(x=CITM, y=SALARY)) + geom_point()

#----Mean imputation
#let's impute the missing data based on the mean
countNAs <- function(x) {
  sum(is.na(x))
}

missing=list()
for (i in 1:length(colnames(cite)))
missing[[i]] = countNAs(cite[,i])
names(missing)=colnames(cite)
missing#here we se we have 7 missing values in column 6

fmiss=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =mean(x, na.rm=TRUE) #convert the item with NA to mean value from the column
  x #display the column
}

cite.mean = cite#make a duplicate dataset
cite.mean[,6]=fmiss(cite.mean[,6])


cite.mean
mean(cite$CITM, na.rm=T) #note, this is the value that was used to
#replace all of the NAs in that column, thus, the mean of Relig.Inv will
#be the exact same in the fund.mean dataset.

#Let's see what happens to our regression
lm2 = lm(SALARY ~ PUB + CITM, data=cite.mean)
summary(lm2) #

ggplot(data = cite.mean, aes(x=CITM, y=SALARY)) + geom_point() + 
  geom_point(data=cite.mean[63:69, ], aes(x=CITM, y=SALARY), colour="red", size=5)



#----Regression based imputation
#let's go back to our original data set with missing data
#now we want to build a model to predict the variable with missing data
lm3 = lm(CITM ~ PUB , data=cite)
summary(lm3) 

newdata=cite[is.na(cite$CITM),] #create a dataset that has just observations
#with missing data

pred.miss = predict(lm3, newdata=newdata)

cite.reg = cite #create another duplicate dataset

cite.reg$CITM = replace(cite.reg$CITM, is.na(cite.reg$CITM), pred.miss)

lm4 = lm(SALARY ~ PUB + CITM , data=cite.reg)
summary(lm4)

ggplot(data = cite.reg, aes(x=CITM, y=SALARY)) + geom_point() +
  geom_point(data=cite.reg[63:69, ], aes(x=CITM, y=SALARY), colour="red", size=5)
#note if we had a simple regression (i.e. bivariate), the red points would 
#follow a straight line, they do not here because we also used PUB as a predictor.

#Let's investigate what happens when the DV has the missing data
#DV
#Note this approach makes little sense if the DV has missing data
#because we are essentially predicted the missing the values
#by our intended regression equation and thus they will simply
#follow the model results when calculated on the complete cases
#hence, the only effect will be to reduce the standard errors
#in the model on the imputed dataset.

#let's assume CITM is our outcome, and PUB and SALARY are the IVs
#we would first want to impute the missing data:
lma = lm(CITM ~ PUB + SALARY, data=cite)
summary(lma) 

newdata=cite[is.na(cite$CITM),] #create a dataset that has just observations
#with missing data

pred.miss = predict(lma, newdata=newdata)

cite.reg2 = cite #create another duplicate dataset

cite.reg2$CITM = replace(cite.reg2$CITM, is.na(cite.reg2$CITM), pred.miss)
lmb = lm(CITM ~ PUB + SALARY, data=cite)
summary(lmb)

lmc = lm(CITM ~ PUB + SALARY, data=cite.reg2)
summary(lmc)

#----Expectation Maximization

#We will use functions in the 'norm' package for this
mcite = as.matrix(cite) #need a data matrix not a dataframe for the functions in norm
pcite = prelim.norm(mcite)
#Description in package of this function:
#Sorts rows of x by missingness patterns, and centers/scales columns of x. 
#Calculates various bookkeeping quantities needed for input to other functions, 
#such as em.norm and da.norm.

emcite = em.norm(pcite)

rngseed(1827) #need to set a random seed for this to work
cite.em = imp.norm(pcite, emcite, mcite)
#Performs maximum-likelihood estimation on the matrix of incomplete 
#data using the EM algorithm.

cite.em = as.data.frame(cite.em)


lm5 = lm(SALARY ~ PUB + CITM, data=cite.em)
summary(lm5)

ggplot(data = cite.em, aes(x=CITM, y=SALARY)) + geom_point() +
  geom_point(data=cite.em[63:69, ], aes(x=CITM, y=SALARY), colour="red", size=5)

#-------Multiple Imputation

#See the Amelia Package Documentation for more info

#First we need to identify the variables to include in the imputation
#model.  It is crucial to include at least as much info as will be used
#in the analysis model.  In other words, any variable that will be in
#your regression model, should also be in your imputation model.  This
#includes any transformations or interactions.  In fact, it is often
#useful to add more information to the imputation model than will be
#present in the analysis model.

#note, if you have categorical variables in your data with missing
#values, then you need to indicat them in the function by stating noms
#for example the code might be a.out = amelia (cite, m=5, noms = "SEX")
#we do not have this in our data, so we can enter the following:
a.out = amelia(cite, m=20)
a.out
#here we imputed 20 new datasets.  Each imputed dataset is now stored
#in a list, a.out$imputations.  Thus we could plot a histogram
#of CITM forthe 2nd imputation.  Since CITM is the only variable
#with missind data, the histograms will be different for each imputation
#but for the other variables, they will be identical as there was
#nothing to impute

hist(a.out$imputations[[3]]$CITM)

ggplot(data = a.out$imputations[[3]], aes(x=CITM, y=SALARY)) + geom_point() +
  geom_point(data=a.out$imputations[[3]][63:69, ], aes(x=CITM, y=SALARY), colour="red", size=5)

#One useful tool in Amelia is the missingness maps.  This is a
#map tht visualizes the dataset on a grid and colors the grid
#by missingness status.  the columns of the grid are the varaibles 
#and the rows are the observations.  This tool allows for a quick
#summary of the patterns of missing data
missmap(a.out)#not that intesting since we only have 1 variable
#with missing data in this example
#this map can be used to show missingness by group and time slice as well.

#To use our imputed datasets to calculate our regression coefficients
#and standard errors we need to run a loop over the number of imputed
#datasets. [Note: one other option is to use the package Zelig]

b.out = NULL
se.out = NULL
for (i in 1:a.out$m) {
  ols.out = lm(SALARY ~ CITM + PUB, data=a.out$imputations[[i]])
  b.out = rbind(b.out, ols.out$coef)
  se.out = rbind(se.out, coef(summary(ols.out))[,2])
}

combined.results = mi.meld(q=b.out, se = se.out)
combined.results

#-------------------Compare results across all models

#this will export a file into your working folder
htmlreg(list(lm1, lm2, lm4, lm5), single.row=TRUE, digits=3, file="mymiss.doc")




