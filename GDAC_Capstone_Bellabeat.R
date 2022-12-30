library("tidyverse")
library("here")
library("skimr")
library("janitor")
library("dplyr")
library("palmerpenguins")
library("lubridate")
library("rmarkdown")
library("magrittr")

dailyActivity <- read.csv("dailyActivity_merged.csv")
dailyCalories <- read.csv("dailyCalories_merged.csv")
dailyIntensities <- read.csv("dailyIntensities_merged.csv")
dailySteps <- read.csv("dailySteps_merged.csv")
sleepDay <- read.csv("sleepDay_merged.csv")

head(dailyActivity)
head(dailyCalories)
head(dailyIntensities)
head(dailySteps)
head(sleepDay)

n_distinct(dailyActivity$Id)
n_distinct(dailyCalories$Id)
n_distinct(dailyIntensities$Id)
n_distinct(dailySteps$Id)
n_distinct(sleepDay$Id)

sum(is.na(dailyActivity))
sum(is.na(dailyCalories))
sum(is.na(dailyIntensities))
sum(is.na(dailySteps))
sum(is.na(sleepDay))

sum(duplicated(dailyActivity))
sum(duplicated(dailyCalories))
sum(duplicated(dailyIntensities))
sum(duplicated(dailySteps))
sum(duplicated(sleepDay))

sleepDay <- sleepDay[!duplicated(sleepDay), ]
sum(duplicated(sleepDay))

glimpse(dailyActivity)
glimpse(dailyCalories)
glimpse(dailyIntensities)
glimpse(dailySteps)
glimpse(sleepDay)

dailyActivity$ActivityDate <- mdy(dailyActivity$ActivityDate)
dailyCalories$ActivityDay <- mdy(dailyCalories$ActivityDay)
dailyIntensities$ActivityDay <- mdy(dailyIntensities$ActivityDay)
dailySteps$ActivityDay <- mdy(dailySteps$ActivityDay)
sleepDay$SleepDay = as.Date(sleepDay$SleepDay, format = "%m/%d/%Y")

sleepDay <- sleepDay %>% 
  rename("Date" = SleepDay)

dailyActivity$TotalMinutes <- dailyActivity$VeryActiveMinutes + dailyActivity$FairlyActiveMinutes + dailyActivity$LightlyActiveMinutes + dailyActivity$SedentaryMinutes

sum(dailyActivity$TotalMinutes > 1440)

sum(dailyActivity$TotalMinutes < 1440)


usageCountPerDay <- dailyActivity %>% 
  group_by(ActivityDate) %>% 
  summarise(NumberOfTimesSmartDeviceIsUsed = n())

glimpse(usageCountPerDay)

usageCountPerDay$DayOfWeek <- weekdays(usageCountPerDay$ActivityDate)

AvgDailyUse <- usageCountPerDay %>% 
  group_by(DayOfWeek) %>% 
  summarise(AvgNumOfTimesDeviceIsUsed = mean(NumberOfTimesSmartDeviceIsUsed))

glimpse(AvgDailyUse)

dailyActivity %>% 
  select(SedentaryMinutes,
         Calories,
         TotalSteps) %>% 
  summary()

sleepDay %>% 
  select(TotalMinutesAsleep,
         TotalTimeInBed,
         TotalSleepRecords) %>% 
  summary()


sleepDailyActivity <- merge(dailyActivity, sleepDay, by = "Id")
n_distinct(sleepDailyActivity)
unique(sleepDailyActivity$Id)

dailyCalories$DayOfWeek <- weekdays(dailyCalories$ActivityDay)

dailyCalories %>% 
  select(Calories,
         DayOfWeek) %>% 
  group_by(DayOfWeek) %>% 
  summarise(mean(Calories))

dailyCaloriesAVG <- dailyCalories %>% 
  group_by(Id) %>%
  summarize(AvgCalories = mean(Calories))

dailyCaloriesDaysAVG <- dailyCalories %>% 
  group_by(DayOfWeek) %>% 
  summarize(AvgCalories = mean(Calories))


dailyStepsAverage <- dailySteps %>% 
  group_by(Id) %>% 
  summarize(AvgStepTotal = mean(StepTotal))

dailyStepsDays <- dailySteps
dailyStepsDays$DayOfWeek <- weekdays(dailyStepsDays$ActivityDay)

dailyStepsDaysAVG <- dailyStepsDays %>% 
  group_by(DayOfWeek) %>%
  summarize(AvgStepTotal = mean(StepTotal))


AvgStepsAndCalories <- merge(dailyStepsAverage, dailyCaloriesAVG, by="Id")

AvgDays_StepsXCalories <- merge(dailyCaloriesDaysAVG, dailyStepsDaysAVG, by="DayOfWeek")

View(AvgDailyUse)

ggplot(data = sleepDay, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point()+
  labs(title = "Total time in bed Vs. Total minutes asleep")

ggplot(data = dailyActivity, aes(x=Calories, y=TotalSteps)) + geom_point()+
  labs(title = "Total steps Vs. Calories burnt")

ggplot(data = sleepDailyActivity, aes(x=TotalMinutesAsleep, y=TotalSteps))+ geom_smooth()+
  labs(title = "Total steps Vs. Total minutes asleep")

ggplot(data = AvgStepsAndCalories, aes(x=AvgStepTotal, y=AvgCalories))+ geom_smooth()+
  labs(title = "Average calories burnt per day Vs. Average steps per day")

ggplot(data = AvgDays_StepsXCalories, aes(x=AvgCalories, y=reorder(DayOfWeek, +AvgCalories), fill=AvgStepTotal))+
  geom_bar(stat = "identity", alpha = 0.8)+
  labs(y= "Day of the week", x= "Average calories burnt", title = "Day of the week Vs Avg calories burnt") 

ggplot(data = AvgDailyUse, aes(x=AvgNumOfTimesDeviceIsUsed, y=reorder(DayOfWeek, +AvgNumOfTimesDeviceIsUsed)))+
  geom_bar(stat = "identity", alpha = 0.8)+
  labs(y= "Day of the week", x= "Avg number of time device is used", title = "Day of the week Vs. Avg number of time device is used")
