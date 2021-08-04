----
## Bellabeat Case Study
## Bellabeat is a successful small high-tech company that manufactures health-focused smart products for women. They have the potential to become a larger player in the global smart device market.

## Business task: Analyze smart device usage data to gain insight into user habits and inform Bellabeat marketing strategy.

## Project:
##  • Processed data by formatting, aggregation, and data exploration in R.
##  • Identified major customer group and developed action plans and recommendations.
----

## Installing and loading common packages and libraries
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("janitor")

library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(janitor)

## Loading your CSV files
Activity_Day <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
Intensities_hourly <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
Sleep_Day <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
Sleep_minute <- read_csv("Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")

## Exploring a few key tables
colnames(Activity_Day)
colnames(Intensities_hourly)
colnames(Sleep_Day)
colnames(Sleep_minute)

## Understanding some summary statistics
### How many unique participants are there in each data frame
n_distinct(Activity_Day$Id)
n_distinct(Intensities_hourly$Id)
n_distinct(Sleep_Day$Id)
n_distinct(Sleep_minute$Id)

### What are some quick summary statistics we'd want to know about each data frame?
### For the daily activity data frame:
Activity_Day %>%
  select(TotalSteps, TotalDistance, SedentaryMinutes,
         VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

#### The users of this data set are mainly lightly active (average 192.8 minutes, approximately 3.2 hours per day)
#### The average number of steps per day is 7638, and the distance is 5.5 miles.
#### The average minutes of sedentary per day is 991.2 minutes, approximately 16.5 hours)

### For the daily sleep data frame:
Sleep_Day %>%
  select(TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
#### The average minutes of sleep per day is 419.5 minutes, approximately 7 hours)
#### The average minutes of time in bed per day is 458.6 minutes, approximately 7 hours 38.6 minutes)

## Transforming data for the intensities hourly data frame:
Intensities_hourly$ActivityHour=as.POSIXct(Intensities_hourly$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
Intensities_hourly$time <- format(Intensities_hourly$ActivityHour, format = "%H")
Intensities_hourly$date <- format(Intensities_hourly$ActivityHour, format = "%m/%d/%y")
Intensities_hourly$Day <-  weekdays(Intensities_hourly$ActivityHour)

### review the updated intensities hourly data frame:
head(Intensities_hourly)

## Create a new daily intensity data frame:
Intensities_Day <- Intensities_hourly %>%
  group_by(Id,date) %>%
  drop_na() %>%
  summarise(sum_TotalIntensity = sum(TotalIntensity)) 

## Cleaning the format
Intensities_Day$datev2=as.POSIXct(Intensities_Day$date, format="%m/%d/%Y", tz=Sys.timezone())
Intensities_Day$Day <-  weekdays(Intensities_Day$datev2)
Intensities_Day_group$Day <- factor(Intensities_Day_group$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

### review the Daily grouped intensities data frame:
Intensities_Day %>%
  select(sum_TotalIntensity) %>%
  summary()
#### mean of intensity of a day is 284.8 intensity

### Grouping to by Day of the weeks
Intensities_Day_group <- Intensities_Day %>%
  group_by(Day) %>%
  drop_na() %>%
  summarise(mean_sum_TotalIntensity = mean(sum_TotalIntensity))

# get the start date and end date of intensity hourly data frame
mindate <- min(Intensities_hourly$date)
maxdate <- max(Intensities_hourly$date)
TotalID <- n_distinct(Intensities_hourly$Id)

## Plotting a few explorations
### relationship between days of the week and average intensity
ggplot(data=Intensities_Day_group) +
  geom_col(mapping = aes(x = Day, y=mean_sum_TotalIntensity, fill=mean_sum_TotalIntensity)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Intensity of Each Day of the Week",
       caption=paste0("Date from ", mindate," to ",maxdate, "; ", TotalID, " users"),
       x="Day of the week",
       y="Average intensity",
       fill="Intensity") +
  coord_cartesian(ylim=c(200,310))
### The graph shows that Saturday has the highest intensity, followed by Tuesday


#### grouping by hour for the intensities hourly data frame, create a new data frame:
#Intensities_hourly_group <- Intensities_hourly %>%
#  group_by(time) %>%
#  drop_na() %>%
#  summarise(mean_TotalIntensity = mean(TotalIntensity))

#### review the hourly grouped intensities data frame:
#Intensities_hourly_group %>%
#  select(mean_TotalIntensity) %>%
#  summary()
##### mean of intensity of a hour is about 12 intensity

### Group the intensity hourly data frame by day and hour to create a new data frame:
Intensities_hourly_G <- Intensities_hourly %>%
  group_by(Day, time) %>%
  drop_na() %>%
  summarise(mean_TotalIntensity = mean(TotalIntensity))

### set the level for day in order to show Monday thru Sunday
Intensities_hourly_G$Day <- factor(Intensities_hourly_G$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

### What time does the user usually exercise?
ggplot(data=Intensities_hourly_G) +
  geom_col(mapping = aes(x = time, y=mean_TotalIntensity)) +
  theme(axis.text.x = element_text(size = 6, angle = 45)) +
  labs(title="Hourly Intensity of a Day",
       caption=paste0("Date from ", mindate," to ",maxdate, "; ", TotalID, " users"),
       x="Time(hour)",
       y="Average intensity")
### Users usually exercise more between 5pm~7pm, followed by exercise between 12pm~2 pm

### deep dive into day of week level
ggplot(data=Intensities_hourly_G) +
  geom_col(mapping = aes(x = time, y=mean_TotalIntensity, fill=mean_TotalIntensity)) +
  facet_wrap(~Day) +
  theme(axis.text.x = element_text(size = 6, angle = 45)) +
  labs(title="Intensity of Each Hour of Each Day of the Week",
       caption=paste0("Date from ", mindate," to ",maxdate, "; ", TotalID, " users"),
       x="Time(hour)",
       y="Average intensity",
       fill="Intensity")

### The graph shows some interesting result. 
### We see that users usually exercise between 5pm and 7pm on weekdays. 
### On Saturdays, they are very active around 1pm and are pretty active until 7pm. 
### On Sunday, the activity level dropped significantly.

### Based on the data, my hypothesis is that the main customer base of the 
### health tracker is office workers. They tend to have longer periods of 
### sedentary time, exercise around lunchtime and after work time during 
### weekdays, active on Saturdays and rest on Sunday to prepare for the next week.



### scrap analysis material
### Let's explore sleep data

## Transforming data for the Sleep_Day data frame:
Sleep_Day$SleepDay=as.POSIXct(Sleep_Day$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
Sleep_Day$SleepDay_Day <- format(Sleep_Day$SleepDay, format = "%m/%d/%y")

### Combine id and day to create a key for merge
Sleep_Day <- Sleep_Day %>%
  unite("Id_date", Id, SleepDay_Day, remove = FALSE)

Intensities_Day %>%
  unite("Id_date", Id, date, remove = FALSE)

## Merging Intensities_Day and Sleep Day together
combined_data <- merge(Sleep_Day, Intensities_Day, by="Id_date")
colnames(combined_data)

### How many unique participants are there in combined_data?
n_distinct(combined_data$Id.x)
### We have fewer IDs because not all users have sleep data

### --- concern of fewer IDs --- ###

## Plotting a few explorations
### let's see if there is any relationship between sleep length vs intensity
### Whether the stronger the intensity, the longer the sleep?
ggplot(data=combined_data, aes(x=TotalMinutesAsleep, y=sum_TotalIntensity)) + geom_smooth() + geom_point()
cor(combined_data$TotalMinutesAsleep,combined_data$sum_TotalIntensity)
### The graph does not show linearity, there is no obvious correlation

### how about the sleep length vs total time in bed
ggplot(data=combined_data, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_smooth() + geom_point()
cor(combined_data$TotalMinutesAsleep,combined_data$TotalTimeInBed)
### Correlation is pretty high between total time in bed vs sleep length 
### Need to investigate further, after all correlation is not causation

###  1 = a strong positive correlation
###  0 = no correlation
### -1 = strong negative correlation.

### there is value in the minute sleep data frame, 
### 1 = "asleep", 2 = "restless", and 3 ="awake"
### Let's transform the data to include the percentage of each sleep value

### take a look of the date frame
head(Sleep_minute)

## Transforming data for the Sleep minute data frame:
Sleep_minute$date=as.POSIXct(Sleep_minute$date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
Sleep_minute$Dateonly <- format(Sleep_minute$date, format = "%m/%d/%y")

Sleep_minute <- Sleep_minute %>%
  unite("Id_date", Id, Dateonly, remove = FALSE)

### The convenient janitor package’s tabyl() function is used.
Sleep_value <- tabyl(Sleep_minute, Id_date, value) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1)
  
colnames(Sleep_value)[2] <- "value_1_asleep"
colnames(Sleep_value)[3] <- "value_2_restless"
colnames(Sleep_value)[4] <- "value_3_awake"

Sleep_value$value_1_asleep <- as.numeric(sub("%","",Sleep_value$value_1_asleep))/100
Sleep_value$value_2_restless <- as.numeric(sub("%","",Sleep_value$value_2_restless))/100
Sleep_value$value_3_awake <- as.numeric(sub("%","",Sleep_value$value_3_awake))/100

glimpse(Sleep_value)

colnames(Sleep_value)

### Merge in sleep value
combined_data <- merge(combined_data, Sleep_value, by="Id_date")

### let's see if there is any relationship between Restless percentage vs intensity
### Whether the stronger the intensity, the lower the percentage of restlessness in bed?
ggplot(data=combined_data, aes(x=value_2_restless, y=sum_TotalIntensity)) + geom_smooth() + geom_point()
cor(combined_data$value_2_restless, combined_data$sum_TotalIntensity)
### there is no obvious correlation

### ggplot to see relationship between Restless total vs time in bed
ggplot(data=combined_data, aes(x=value_2_restless, y=TotalTimeInBed)) + geom_smooth() + geom_point()
cor(combined_data$value_2_restless,combined_data$TotalTimeInBed)
### there is no obvious correlation



