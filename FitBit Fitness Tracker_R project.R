#loading packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# Import files:
activity <- read.csv("dailyActivity_merged.csv")
sleep <- read.csv("sleepDay_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")

# Observe the first rows of the data:
head(activity)
head(sleep)
head(weight)

# Check the column names in the files:
colnames(activity)
colnames(sleep)
colnames(weight)

# How many unique participants are there in the data frames?
n_distinct(activity$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

# Remove unnecessary columns:
activity <- activity %>%
  select(-c("TrackerDistance" , "LoggedActivitiesDistance", "SedentaryActiveDistance"))

sleep <- sleep %>%
  select(-c("TotalSleepRecords"))

# Rename columns in the datasets (to make them consistant before merging):
activity <- rename(activity, Date = ActivityDate)
sleep <- rename(sleep, Date = SleepDay)

# Check the data types in the datasets: 
str(activity)
str(sleep)

# Change the data type in the "Date" columns in our datasets: 
activity$Date=as.Date(activity$Date, format="%m/%d/%y")
sleep$Date=as.Date(sleep$Date, format="%m/%d/%y")

# Check the number of observations in the datasets:
nrow(activity)
nrow(sleep)

# Check the duplicated rows in the datasets:
nrow(activity[duplicated(activity), ])
nrow(sleep[duplicated(sleep), ])

# Remove duplicate rows in the dataset 'sleep':
sleep <- sleep %>% distinct()

# Check the result:
nrow(sleep)

# Merge the datasets:
merged_data <- merge(sleep, activity, by=c('Id', 'Date'))

# Observe the first rows of the merged data:
head(merged_data)

# Observe some general trends in the data:
hist(merged_data$TotalSteps)
hist(merged_data$VeryActiveMinutes)
hist(merged_data$SedentaryMinutes)
hist(merged_data$Calories)

# Check the statistical summary:
merged_data %>%
  select(TotalSteps, TotalDistance, VeryActiveDistance, ModeratelyActiveDistance, 
         LightActiveDistance) %>%
  summary()

merged_data %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, 
         SedentaryMinutes, Calories) %>%
  summary()

merged_data %>%
  select(TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

#Observations:
  
#Average sedentary minutes were at 955.2 minutes or 16 hours.
#Average very active minutes and fairly active minutes were at 23.04 minutes 14.79 minutes and respectively.
#Average lightly active minutes were at 210.3 minutes or 3.5 hours.
#The average amount of calories burnt per day was around 2362 kcal.

#Conclusions:
  
#Participants were largely inactive throughout the day.
#Participants spent a low amount of time exercising.
#Participants are unlikely to take part in vigorous activities.

# Visualization
ggplot(data=activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title="Total Steps vs. Calories")
# There is positive correlation between Total Steps and Calories, which is obvious - the more active we are, the more calories we burn.

ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point() + 
  labs(title="Total Minutes Asleep vs. Total Time in Bed")
# the relationship between Total Minutes Asleep and Total Time in Bed looks linear. So if the Bellabeat users want to improve their sleep, they should consider using notification to go to sleep.

ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='darkblue') + 
  geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")

# Conclusion:

# Here we can clearly see the negative relationship between Sedentary Minutes and Sleep time.
# As an idea: if Bellabeat users want to improve their sleep, Bellabeat app can recommend reducing sedentary time.
# Keep in mind that we need to support this insights with more data, because correlation between some data doesn’t mean causation.
