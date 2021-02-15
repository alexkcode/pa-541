#############################################
# PA 541 - Advanced Data Analysis
# Intro to R and the Tidyverse
#############################################


#-----Set the working directory for the session
#the working directory is the folder that contains the data you will be using and/or where you want
#to save the output of your work.




# -----Load the libraries/packages you need for this session 
#if a package is not installed, you can use the packages tab in RStudio to search for it and 
#install it.  Once installed, it is available for use, but not active.  Once installed, you can
#make a package (and all of its functions) available to by using the library() function.

#Think about packages like books.  You can own (install) a bunch of books, but you can't use them
#until you take them off the shelf (library)

library(tidyverse)
#notice which functions are in conflict with other packages
#meaning the same function name is used in both.  To avoid
#confusion with these functions you can type dplyr::filter
library(skimr)

library(gapminder) #has the gapminder dataset of country life expectency, population and gdp
#a useful dataset to explore various functions

############################################################################
# ------Bring in a dataset and take a quick look at it

gap <-  gapminder

gap #take a look at the dataset.
#note, this object is a tibble not a dataframe.  This is the tidyverse way of
#showing the data.  If it was a dataframe, R would show all of the rows and columns, 
#which can be problematic for large datasets as it fills up your entire console.  A
#tibble only shows you part of the data nd gives you information on the number of 
#rows and the col names of variables not show.  A useful function with a tibble is
 
glimpse(gap) #which is very similar to str()

#if you prefer working in a dataframe you can simply use as.data.frame()

skim(gap) #more detailed glimpse of the data; info on missing, quartiles, means


#------ A motivating example

#we will learn all of the tools do to this below 

#what if you wanted to plot the average GDP Per Capita for each continent in the year
#1952.

gap #again, the data lists country, continent, year, etc...

gap_avg = gap %>%
  group_by(continent) %>%
  filter(year == 1952) %>%
  summarize(meanGDP = mean(gdpPercap))

ggplot(gap_avg, aes(x = continent, y = meanGDP, fill = continent)) + geom_col()


################################################################################
# 1. ------R Basics
################################################################################


#R as simple calculator

7 + sqrt(144)

#Object oriented programming
#everything we create in R is an object. Let's look at some of the main object types

x = 7 + sqrt(144)
x

ls() #lists all of the objects in your current R session; can also use 'objects()'



#----VECTORS
myv = c ( 10 , 2.5 , 38 , 4.6 , 51 , 16 , 27)
#'c' is a function in R. 'c' combines the arguments you
#pass it to form a vector.  

myv #here we can say our new vector
sqrt (myv) #the function sqrt, is applied to each element in the vector

#we can easily find the mean and standard deviation of our newly created vector
mean(myv)
sd(myv)

#If for some reason we wanted to save the mean of myv to be used in later calculations
#we can do the following

mean.myv = mean(myv)
mean.myv

#We can access different elements of the object, the exact manner of access depends on
#the type of object you are working with

myv[2]#returns the second element in the vector

#----MATRICES
x = rnorm(16)
mat1 = matrix(x, nrow = 4)

mat1 #here is the result, as with vectors we can call specific items

mat1[1,2] #the first number is for the row, the second is for the column

mat1[,3] #here I call for all the rows, but only the third column

mat1[1:2,] #here all the columns, but just for the first 2 rows


#----DATAFRAMES
#create some variables
x1 = c(11,22,33,44,55,66,77)
x2 = c(71,52,62,31,22,16,17)
y = 2*x1 + -5*x2

xdat = data.frame(x1,x2, y)
class(xdat)

#can work with it like a matrix
xdat[,1] #get just the first column

xdat$x1 #can also call columns by their names

#create new variables and add them as an additional column in the dataframe
xdat$x3 = log(xdat$x1)
xdat

summary(xdat$x3)#get summary statistics on the variables
skim(xdat)

ls() #take a look at what exists in our current environment now

#apply family allows you to pass functions across columns or rows

apply(xdat, MARGIN=2, FUN=mean) #margin 2 means columns, 1 would indicate rows

#let's save our dataframe

save(xdat, file="xdat.RData")#to save it as an R dataframe

write_csv(xdat, file.path("xdat.csv")) #create a csv file

#if at a later point you wanted to read in your dataframe you would...

load("xdat.RData") #to read in a saved R datafile

read_csv(file="xdat.csv") #you will want to assign a name to this though...such as

xdat2 = read_csv(file="xdat.csv")

#------------Logical statements

xdat$x1 > 20

which(xdat$x1 > 20)

#ifelse statements
ifelse (xdat$x1 > 20, xdat$x1*2, xdat$x1)

#for loops
poisdist=vector()
for (i in 1:1000) poisdist[i]= mean(rpois(100, 1.5))

#------------Functions in R

#Base R and the many packages that exist provide the user with a number of functions
#These built-in R functions have the following format:
#  function_name(arg1 = val1, arg2 = val2, ...)
#Each function has one or more arguments. 

#take mean() for instance, it has three arguments (type mean in the help tab on the bottom right quadrant)
#many arguments do not need to specified each time you use the function, as the default values are often what 
#you want.

mean(xdat$x2)
#but if you want to add in addition arguments...
mean(xdat$x2, trim = .4) #which will trim the largest and smallest 40% of the values



##############################################################################
# 2. ----- Data manipulation
###############################################################################


#There are many components to data wrangling, importing and cleaning data, getting
#data into a usable format, summarizing data, etc...

#Today we will look at five key verbs from the dplyr package

# The five verbs are:
# filter(), that is able to return a subset of the rows,
# select(), which returns a subset of the columns,
# arrange(), that reorders the rows according to single or multiple variables,
# mutate(), used to add columns from existing data,
# summarise(), which reduces each group to a single row by calculating aggregate measures.

#See Wickman and Gorelund 2017 Book - R for Data Science for more in-depth treatment on
#these verbs and a range of other data wrangling tools

#To explore these verbs we will use the gapminder dataset, which is in the gapminder package

# For each of 142 countries, the package provides values for life expectancy, GDP per capita, 
#and population, every five years, from 1952 to 2007. 



#-----FILTER: allows you to pick observations by their value; in other words
#pull out specific observations (i.e., rows) in your data.  You do this
#by passing the function filter to your dataset and then one or more logical
#statements.

#so let's say we want to only look at observations from the year 1952
gap_1952 = filter(gap, year == 1952)

gap_1952

#you can filter on more than one variable, and use logical operators
#so maybe you want to find all counties prior to 1954 that had a life
#expectancy greater than 65 years.  

gap_2 = filter(gap, year <1954, lifeExp >65)
gap_2


######  you try: create a dataset that contains information on China in the year 2007







###### you try: create a dataset that contains data for all years in the 1960s,
#and only in Asia






#following gives you same result as above
gap_tryb = filter(gap, year <1970, year >1959, continent == "Asia")
gap_tryb

# Here is a list of logical operators in R that you can use with filter() as well as 
# with other functions or purposes:
# x < y, TRUE if x is less than y
# x <= y, TRUE if x is less than or equal to y
# x == y, TRUE if x equals y
# x != y, TRUE if x does not equal y
# x >= y, TRUE if x is greater than or equal to y
# x > y, TRUE if x is greater than y
# x %in% c(a, b, c), TRUE if x is in the vector c(a, b, c)



#-----ARRANGE: order rows
#like, filter, you pass arrange column names.  If you pass more than one
#each additional column will be used to break ties in the values in the
#preceding columns

arrange(gap, gdpPercap)

#note, we didn't assign the results of the arrange function.  We will just
#see the first few rows.  If you want to assign them, then simply do as we did above
#for the filter examples

#if you want in descending order

arrange(gap, desc(gdpPercap))



#-----SELECT: allows you to narrow down the variables in a dataset you are working with.
#this can be helpful if you have a dataset with thousands of variables and you are only
#interested in a small subset. 

select(gap, country, year)

#we can also select in a variety of ways, other than the specific column name

#For instance, maybe you want all columns between 2 other columns:
select(gap, year:pop)

# dplyr comes with a set of helper functions that can help you select groups of variables inside a select() call:
# starts_with("X"): every name that starts with "X",
# ends_with("X"): every name that ends with "X",
# contains("X"): every name that contains "X",
# matches("X"): every name that matches "X", where "X" can be a regular expression,
# num_range("x", 1:5): the variables named x01, x02, x03, x04 and x05,
# one_of(x): every name that appears in x, which should be a character vector.


#Lastly, if you want to rename a variable use the function rename ()
#rename(gap, CONT = continent)



#-----MUTUATE: allow you to create new variables from existing variables
#let's create a lifeExp in terms of decades

gap_4a = mutate (gap, lifeExpDec = lifeExp/10)
gap_4a

#this is the tidyverse way of creating new variables; we saw the base
#R way above.  That was to:

# gap$lifeExpDec = gap$lifeExp/10

#What if we wanted lifeExp in decades and gdpPerCap in 1,000s of dollars
gap_4b = mutate (gap,
                 lifeExpDec = lifeExp/10,
                 gdpPercap1000 = gdpPercap/1000 #we can calculate several variables at once
)

gap_4b

#-----SUMMARISE: calculate summaries of variables.  R has several functions to aggregate data,
#here are a few common ones:

# min(x) - minimum value of vector x.
# max(x) - maximum value of vector x.
# mean(x) - mean value of vector x.
# median(x) - median value of vector x.
# quantile(x, p) - pth quantile of vector x.
# sd(x) - standard deviation of vector x.
# var(x) - variance of vector x.
# IQR(x) - Inter Quartile Range (IQR) of vector x.
# diff(range(x)) - total range of vector x.
#other  dplyr specific aggregate functions are n_distinct() which counts the number
#of unique elements in a vector and n() which simply counts the number of observations.

#Note: count() is a function that performs both group_by and n() at the same time)


#The summarize verb is most useful in combination with the
#group_by function.  This is likely one of the techniques you will use most commonly.
#An additional technique that facilitates this approach is is the 'pipe' operater.  
#Piping allows us to combine multiple functions together.

#say you want to know the average life expectancy by continent, we could

gap_avg = gap %>%
  group_by(continent) %>%
  summarize(avgLifeExp = mean(lifeExp))

gap_avg

#or you might want to know the average gdpPercap in each country over all the years in the
#the dataset

gap_avg2 = gap %>%
  group_by(country) %>%
  summarize(gdpPercap = mean(gdpPercap))

gap_avg2


#### you try: create a dataset of the average lifeExp by year






#### you try: create a dataset of the average lifeExp for each continent in 2002





#note if you have any missing values and use a function like mean(), you get NA as result.  So
#if for some of the years gdpPercap was missing, it would return an NA for the average even though
#there are values for all the other years. To deal with this, you can add the na.rm = TRUE.
#For example:

gap_avg2 = gap %>%
  group_by(country) %>%
  summarize(gdpPercap = mean(gdpPercap, na.rm = TRUE))
#note, this is unnecessary here as there are no NAs in the gapminder data

#you can also group by multiple variables
#in our data, maybe we want to group by year and continent

gap_avg3 = gap %>%
  group_by(continent, year) %>%
  summarize(med_gdp = median(gdpPercap))

gap_avg3

#Finally, this is really helpful for creating plots of the summarized data
gap_avg4	=	gap	%>%		
  group_by(year)	%>%		
  summarize(meanGDP	=	mean(gdpPercap)) 

ggplot(gap_avg4,	aes(x	=	year,	y	=	meanGDP))	+		
  geom_point() 

gap_avg5	=	gap	%>%		
  group_by(year,	continent)	%>%		
  summarize(meanGDP	=	mean(gdpPercap)) 

ggplot(gap_avg5,	aes(x	=	year,	y	=	meanGDP,	color	=	continent))	+		
  geom_point() 

#what if you wanted to plot the life expectancy for each continent in the year
#1952.  What would you do?  Seems like a difficult task, but if you walk through
#the necessary steps, you should be able to figure it out:

gap_avg6 = gapminder %>%
  group_by(continent) %>%
  filter(year == 1952) %>%
  summarize(meanGDP = mean(gdpPercap))

ggplot(gap_avg6, aes(x = continent, y = meanGDP)) + geom_col()

#NOTE: There are two types of bar charts: geom_bar makes the height of the bar 
#proportional to the number of cases in each group (or if the weight aesthetic is 
#supplied, the sum of the weights). If you want the heights of the bars to represent 
#values in the data, use geom_col instead. Which is what we did above, as your new
#data set, gap_avg6, contained the average gdpPerCap in each continent in 1952, thus
#there are only 5 rows of data, and you want bar height to equal the mean value.



########################################################################
# 3. ------ Basic data visualization
########################################################################

#we will use the package ggplot2, which is part of the tidyverse for visualizng data
#base R has options for graphcis, but ggplot is more flexible and prodcues better
#looking graphics. 

#some good reference materials: 
# https://ggplot2.tidyverse.org/reference/
# http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
# https://uc-r.github.io/ggplot_intro

# create canvas
ggplot(gap)

# variables of interest mapped
ggplot(gap, aes(x = year, y =  gdpPercap))

# data plotted
ggplot(gap, aes(x = year, y =  gdpPercap)) +
  geom_point()

#We will cover data visualization later in the semester

###################################################################
# 4. -----------Some other useful tools and packages
###################################################################

#Janitor package - help rename all of your variables 

#Tidyr package to reshape data (part of the tidyverse)

#in dplyr, there are tools for relational data (joining and merging datasets)

#texreg package  - formats your model output into near publication ready tables

#and a whole bunch of other things we will explore this semester...
