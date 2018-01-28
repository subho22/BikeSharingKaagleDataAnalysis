###########################################################################################
###########################################################################################
#1. Check current directory

getwd()

#2. Set current directory as the folder where Bike Sharing data is downloaded

setwd("G:\\Rworkspace\\Bikesharing")

###########################################################################################
###########################################################################################
#3. import train data

train<-read.csv("train.csv")

#4.Import test data

test<-read.csv("test.csv")

#5. Check structure of train data

str(train)


#6. Check structure of test data

str(test)


#7. Check the summary of train data

summary(train)

#8. Check the summary of test data

summary(test)

###########################################################################################
###########################################################################################
#Combine both train and test data to undetstand the distribution of independent variable

#9. Create new blank columns in test data which are not present there (casual, registered, count)

test$casual<-0
test$registered<-0
test$count<-0

#10. Check before combining train and test data set whether the structure is similar for both.

str(test)
str(train)
#11. Create a new dataframe by combining train and test dataset -"BikeData" 
BikeData <- rbind(train,test)


###########################################################################################
###########################################################################################
#12. Check the structure of BikeData

str(BikeData)

#13. Check the summary of BikeData

summary(BikeData)

###########################################################################################
###########################################################################################
#14. Find Missing value (if any)

table(is.na(BikeData))
sapply(BikeData,function(x)sum(x==""))

###########################################################################################
###########################################################################################
#Uni-Variate Analysis. 

#15. Create histigram chart for all the numeric columns

hist(BikeData$season) #season should be factor
hist(BikeData$holiday)#holiday should be factor
hist(BikeData$workingday)#workingday should be a factor
hist(BikeData$weather)#weather should be factor
hist(BikeData$humidity)
hist(BikeData$temp)
hist(BikeData$atemp)
hist(BikeData$windspeed)


str(BikeData)




#16. Change the layout of plot using par function. Then again plot the histograms

par(mfrow=c(2,2))


#17. Write insights as comment for each of the histogram


###########################################################################################
###########################################################################################
#18. Convert discrete variable into factor (season, weather, holiday, working day)
BikeData$season<- as.factor(BikeData$season)
BikeData$weather<-as.factor(BikeData$weather)
BikeData$holiday<-as.factor(BikeData$holiday)
BikeData$workingday<-as.factor(BikeData$workingday)
str(BikeData)


#19. Create table for all the factors  

table(BikeData$season)
table(BikeData$weather)
table(BikeData$holiday)
table(BikeData$workingday)


#20. Create proportion for all the factors and write insight as comment

prop.table(table(BikeData$workingday))
prop.table(table(BikeData$holiday))
prop.table(table(BikeData$weather))
prop.table(table(BikeData$season))




par(mfrow=c(2,2))

barplot(table(BikeData$season),main = "season",xlab = "no. of seasons",ylab = "no. of users")
barplot(table(BikeData$workingday),main = "workingday",xlab = "no. of workingday",ylab = "no. of users")
barplot(table(BikeData$holiday),main = "holiday",xlab = "no. of holiday",ylab = "no. of users")


# Feature Engineering


#Create the Year column from datetime column


#convert that to factor
#Check the unique data in BikeData$year

#Year
BikeData$year<-substr(BikeData$datetime,1,4)
BikeData$year=as.factor(BikeData$year)
#year<-substr(BikeData$datetime,1,4)

unique(BikeData$year)

#Month
BikeData$month<-substr(BikeData$datetime,6,7)
#Create the Month column from datetime column
#convert that to factor
BikeData$month=as.integer(BikeData$month)
#Check the unique data in BikeData$Month
unique(BikeData$month)


#Create the days of week column from datetime column

#convert that to factor
#Check the unique data in BikeData$day


#Date
date<-substr(BikeData$datetime,1,10)

days<-weekdays(as.Date(date))
BikeData$day<-days
unique(BikeData$day)




#Hour
BikeData$hour = substr(BikeData$datetime, 12,13)
BikeData$hour = as.factor(BikeData$hour)
unique(BikeData$hour)


#Bivariate analysis


#Bi-Variate Analysis

#6. Yearly trend
boxplot(BikeData$count~BikeData$year,xlab="year", ylab="Total users") 


boxplot(BikeData$registered~BikeData$year,xlab="year", ylab="registered users") 
# Demand with registered users increases over year

boxplot(BikeData$casual~BikeData$year,xlab="year", ylab="casual users") 
# Demand with casual users also increases over year


#1.Trend by Hour 
# Find total count of User Trend by Hour
boxplot(BikeData$count~BikeData$hour, xlab ="Hour", ylab = "Count of User")
#High User:7 to 9 and 17-19 
#Average User: 10-16; 
#Low User:0-6 and 20-24 hours


boxplot(BikeData$casual~BikeData$hour, xlab ="Hour", ylab = "Casual User") 
#More casual user between 10 to 19


# Find Registered and Casual User Trend by Hour
boxplot(BikeData$registered~BikeData$hour, xlab ="Hour", ylab = "Registered User")
#High User:7 to 9 and 17-19 
#Average User: 10-16; 
#Low User:0-6 and 20-24 hours



#2.Trend by Day of Week
# Find Registered and Casual User Trend by Day of the week
boxplot(BikeData$casual~BikeData$day, xlab ="Day", ylab = "Casual User")
# Sat and Sun there is high in demand for casual user

# Find Registered and Casual User Trend by Day of the week
boxplot(BikeData$registered~BikeData$day, xlab ="Day", ylab = "Registered User")
# Sat and Sun there is less demand for registered user


#3. Trend by Weather
# For Casual user because of rain demand goes down drastically for Rainy day
boxplot(BikeData$casual~BikeData$weather, xlab ="Weather", ylab = "Casual User")

#4. Trend by Humidity
boxplot(BikeData$casual~BikeData$humidity, xlab ="Weather", ylab = "Casual User")
boxplot(BikeData$registered~BikeData$humidity, xlab ="Weather", ylab = "Registered User")

#5. For all the numerical variable - Correlation
sub = data.frame(train$registered,train$casual,train$count,train$temp,train$humidity,train$atemp,train$windspeed)

library(corrplot)

corrplot(cor(sub[,sapply(sub,is.numeric)]))
