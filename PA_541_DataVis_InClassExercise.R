################################
#
# PA 541 ADVANCED DATA ANALYSIS
# DATA VISUALIZATION PRACTICE
#
################################

#let's load the tidyverse, which has the ggplot2 library
#and also bring in the midwest data, which comes with ggplot2
library(tidyverse)
data(midwest)

midwest

glimpse(midwest) #look at all the variables and their type

# The plan is to have you work on a number of different plots.
# I am going to give you minimal information and have you try to
# develop an appropriate figure.  For Part One, do not
# worry about labels, titles, or other finer aspects.  Let's just
# try to get a useful figure plotted that helps tell a story.
# In Part Two, you will work on the refinements. 

###################
### PART ONE ###
###################

## FIGURE ONE
# Create a plot to show the relationship between the percent of college educated (percollege)
# and the percent living below the poverty line (percbelowpoverty). You can add any another
# aesthetic mappings to the plot you find interesting or useful.

fig1 <- midwest %>%
  ggplot(aes(x = percollege, y = percbelowpoverty)) +
  geom_jitter() +
  geom_smooth(method = "lm")
fig1

## FIGURE TWO
# Create a plot to show the total number of American Indians in each state.  Note, you will
# will need to create a new variable and then a new dataset before plotting.

midwest_AmInd <- midwest %>%
  group_by(state) %>%
  summarize(AmInd = sum(popamerindian))

midwest_AmInd %>%
  left_join(tigris::states(cb = T) %>% sf::st_transform(sf::st_crs(26971)),
            by = c("state" = "STUSPS")) %>%
  ggplot(aes(fill = AmInd)) + 
  geom_sf(aes(geometry = geometry)) + 
  ggtitle("American Indians in the Midwest")

## FIGURE THREE
# Create a plot to show the distribution of population density (popdensity) across the different counties in
# in Illinois.

midwest %>% 
  filter(state == "IL") %>%
  ggplot(aes(popdensity)) + geom_histogram() + ggtitle("Population Density of Illinois Counties")
  
## FIGURE FOUR
# Recreate Figure Three, but do it for each state.  Hint: use facet_wrap() to create a subplot for
# each state.

midwest %>% ggplot(aes(popdensity)) + 
  geom_histogram() + 
  facet_wrap(~state) + 
  aes(fill = as.factor(state)) + 
  theme(legend.position = "none")

## FIGURE FIVE
# Create a boxplot showing the distribution of population totals (poptotal) across the counties
# for each state.  Color the different boxplots by state.

# midwest %>% ggplot(aes(y = log10(popdensity), fill = state)) + geom_boxplot()

# better than transforming y since it properly labels the y-axis instead of 
# showing the exponent number
midwest %>% ggplot(aes(y = popdensity, fill = state)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  annotation_logticks(sides = "l") 

#####################
### PART TWO ###
#####################

# For each of the plots above, add labels and titles. As needed
# update the font size, color, etc.. See the end of the script for
# this week for more information


