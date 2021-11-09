# checking directory
getwd()

# loading data and converting the blanks into NA in order to remove them later (used for all the questions).
ny <- read.csv("new_york_city.csv", header=T, na.strings=c("","NA"))
wash = read.csv("washington.csv", header=T, na.strings=c("","NA"))
chi = read.csv("chicago.csv", header=T, na.strings=c("","NA"))

#ny = read.csv('new_york_city.csv')
#wash = read.csv('washington.csv')
#chi = read.csv('chicago.csv')

# preview data
head(ny)
head(chi)
head(wash)
summary(ny)

# load libraries
library(ggplot2) #for the plots
library(ggpubr) #to adjust multiple plots in one page


##########################
#       QUESTION 1: Show the statistical summary (quartiles, median and outliers) of the trip duration per user type in each city. 
##########################

#Boxplot of New-York using qplot. 
box_ny = qplot(x = User.Type, y = Trip.Duration, data = subset(ny, !is.na(User.Type)), geom = 'boxplot') + 
  coord_cartesian(ylim = c(0,3000))  + labs(x = "User Type", y = "Trip Duration") +
  ggtitle('New-York')

#Boxplot of Chicago using qplot
box_chi = qplot(x = User.Type, y = Trip.Duration, data = subset(chi, !is.na(User.Type)), geom = 'boxplot') + 
  coord_cartesian(ylim = c(0,3000)) + labs(x = "User Type", y = "Trip Duration")+
  ggtitle('Chicago')

#Boxplot of Washington using qplot
box_wash = qplot(x = User.Type, y = Trip.Duration, data = subset(wash, !is.na(User.Type)), geom = 'boxplot') + 
  coord_cartesian(ylim = c(0,3000)) + labs(x = "User Type", y = "Trip Duration") +
  ggtitle('Washington')

#combining the 3 graphs in one page
ggarrange( box_ny, box_chi, box_wash,
          ncol = 3, nrow = 1) 

#Results: Washington shows more customers than other cities, and in general, all the cities records a higher number of customers than subscribers. Only Chicago shows dependent users. 


##########################
#       QUESTION 2: Show type of users per gender in New-York and Chicago. 
##########################

#Function that takes two arguments:
#dataset to be processed
#dname, dataset's name of the current city
histogram_creator  = function(dataset, dname)
{
  dataset = na.omit(dataset)
  
  #concatenation of dname with the title
  title = paste('Histogram of Number of User Type per gender in', dname, sep = ' ')
  
  ggplot(aes(x=User.Type), data= dataset) +
    geom_histogram(binwidth=100 , stat = "count") + 
    ggtitle(title) +
    labs(x = "User Type", y = "Gender") + 
    facet_wrap(~Gender) +
    theme(plot.title = element_text(size=12))
}

histo_ny = histogram_creator(ny, "New-York")
histo_chi = histogram_creator(ny, "Chicago")

#combining the 2 graphs in one page
ggarrange(histo_ny, histo_chi, 
          ncol = 2, nrow = 1)

#Results: In both cities, we have more male users than female users, especially the subscribers. 

##########################
#       QUESTION 3: Show the average trip duration according to the birth year. 
##########################

#Plot of New-York

age_ny = ggplot(aes(x=Birth.Year, y=Trip.Duration), data= subset(ny, !is.na(Birth.Year))) +
  coord_trans(y='sqrt') +
  geom_line(stat = 'summary', fun.y=mean) +
  labs(x = "Users year of birth", y = "Average of trip duration") + 
  ylim(100,2000)

#Plot of Chicago

age_chi = ggplot(aes(x=Birth.Year, y=Trip.Duration), data= subset(chi, !is.na(Birth.Year))) +
  coord_trans(y='sqrt') +
  geom_line(stat = 'summary', fun.y=mean) +
  labs(x = "Users year of birth", y = "Average of trip duration") + 
  ylim(100,2000)

#combining the 2 graphs in one page
ggarrange(age_ny, age_chi, 
          labels = c("Average trip duration per age in New-York", "Average trip duration per age in Chicago"),
          ncol = 2, nrow = 1)

#Results: In New-York, the average duration is higher, especially for the oldest users, however the average of trip duration is increasing among the young users in Users.
