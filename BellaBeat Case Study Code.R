library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

setwd("~/R Data Science")

# Prepping Data

# Daily
dailyActivity_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
sleepDay_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weightLogInfo_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

#Hourly
hourlyCalories_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourlyIntensities_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourlySteps_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
#Minute
minuteCaloriesNarrow_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv")
minuteIntensitiesNarrow_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv")
minuteMETsNarrow_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv")
minuteSleep_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")
minuteStepsNarrow_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/minuteStepsNarrow_merged.csv")

#second
heartrate_seconds_merged <- read_csv("Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")


# Joining hourly Activity
hourlyActivity_merged <- hourlyCalories_merged %>%
  merge(hourlyIntensities_merged,by = c('Id','ActivityHour')) %>%
  merge(hourlySteps_merged,by = c('Id','ActivityHour'))

# Joining minute Activity (Excluding Sleep. As this doesn't have enough)

minuteActivity_merged <- minuteCaloriesNarrow_merged %>%
  merge(minuteIntensitiesNarrow_merged,by = c('Id','ActivityMinute')) %>%
  merge(minuteStepsNarrow_merged,by = c('Id','ActivityMinute')) %>%
  merge(minuteMETsNarrow_merged,by = c('Id','ActivityMinute'))


# Begin Analysis
datasets <- c("dailyActivity","hourlyActivity","minuteActivity", "sleepDay", "secHeartrate","weightLog") 
distinctIds_tables <- c(n_distinct(dailyActivity_merged$Id),n_distinct(hourlyActivity_merged$Id),
                        n_distinct(minuteActivity_merged$Id),n_distinct(sleepDay_merged$Id),
                        n_distinct(heartrate_seconds_merged$Id),n_distinct(weightLogInfo_merged$Id))

completeness <- paste(round((distinctIds_tables/33)*100,2), "%")

View(data.frame(datasets, distinctIds_tables, completeness))

# checking how much people actual use certain things

a <- as.data.frame(table(sleepDay_merged$Id))
nrow(filter(a, Freq >= 16)) / nrow(a)

ggplot(data=a, aes(x=Freq)) + geom_bar(color = "#FF6666", fill = "#FF6666") + geom_vline(xintercept=mean(a$Freq), color = "grey", size = 1.5)
 
b <- as.data.frame(table(heartrate_seconds_merged$Id))
mean(b$Freq) /12 /60 /24

c <- as.data.frame(table(weightLogInfo_merged$Id))
nrow(filter(c,Freq > 5))


# Figureing Out sleep shit


dailyActivity_merged$ActivityDate=as.POSIXct(dailyActivity_merged$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
hourlyActivity_merged$ActivityHour=as.POSIXct(hourlyActivity_merged$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
minuteActivity_merged$ActivityMinute=as.POSIXct(minuteActivity_merged$ActivityMinute, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleepDay_merged$SleepDay=as.POSIXct(sleepDay_merged$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())



sleepDay_merged$DayofWeek <- wday(as_date(as.POSIXct(sleepDay_merged$SleepDay, format="%m/%d/%Y %H:%M:%S %p")))

sleepDay_merged$DayofWeek = case_when(
  sleepDay_merged$DayofWeek == 1 ~ 'Sunday',
  sleepDay_merged$DayofWeek == 2 ~ 'Monday',
  sleepDay_merged$DayofWeek == 3 ~ 'Tuesday',
  sleepDay_merged$DayofWeek == 4 ~ 'Wednesday',
  sleepDay_merged$DayofWeek == 5 ~ 'Thursday',
  sleepDay_merged$DayofWeek == 6 ~ 'Friday',
  sleepDay_merged$DayofWeek == 7 ~ 'Saturday',
)


sleepDay_merged %>%
  group_by(DayofWeek) %>%
  summarise(MeanSleep = mean(TotalMinutesAsleep)/60)
  
sleepDay_merged %>%
  group_by(Id) %>%
  summarise(TimeToSleep = mean(TotalTimeInBed)-mean(TotalMinutesAsleep)) %>%
  ggplot(aes(x=Id,y=TimeToSleep)) +geom_bar(stat='identity')
  




#Playing around with Activity

user_hours_total <- dailyActivity_merged %>%
  rowwise() %>%
  group_by(Id, ActivityDate) %>%
  summarize(TotalHours = sum(VeryActiveMinutes, FairlyActiveMinutes,
                               LightlyActiveMinutes, SedentaryMinutes))

user_hours_total$TotalHours <- user_hours_total$TotalHours/60


user_hours_mean <- user_hours_total %>%
  group_by(Id) %>%
  summarise(MeanHours = mean(TotalHours))


ggplot(user_hours_mean) + geom_histogram(mapping = aes(x=MeanHours),color="black", fill="lightblue") +
  geom_vline(xintercept = mean(user_hours_mean$MeanHours),linetype='dotted', color = "red", size=1.5) +
  annotate(geom = 'text', x = mean(user_hours_mean$MeanHours)-0.2, y = 4,color='Red', label = 'Mean',angle=90,size=7)+
  labs(x='Hours Wearing Fitbit',y='Number of Users',title='How Often Users Wear Their Fitbit') +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data=user_hours_total) + geom_histogram(mapping = aes(x=TotalHours,fill='Red'))


## Sleep Stuff

sleepDay_merged %>%
  group_by(Id) %>%
  summarise(MeanSleep = mean(TotalMinutesAsleep)/60) %>%
  ggplot(aes(x=Id,y=MeanSleep,label=Id))+geom_point() +geom_text(aes(label=Id),hjust=-0.1, vjust=-0.1) +
  labs(x='Ids',y='Mean Hours Slept',title='Average Amount of Sleep For Users') +
  geom_hline(yintercept = 7,linetype='dotted', color = "red", size=0.5) +
  theme(plot.title = element_text(hjust = 0.5))

sleepDay_merged %>%
  group_by(Id) %>%
  summarise(MeanSleep = mean(TotalMinutesAsleep)/60) %>%
  filter(MeanSleep < 7) %>%
  nrow()

sleepDay_merged %>%
  group_by(DayofWeek) %>%
  summarise(MeanSleep = mean(TotalMinutesAsleep)/60,TotalMinutesAsleep) %>%
  ggplot(aes(x=DayofWeek,y=TotalMinutesAsleep,fill=DayofWeek)) + geom_boxplot() +
  annotate("text", x = 4, y = 500, label = "Max Sleep") +
  annotate("text", x = 1, y = 420, label = "Min Sleep")
  

  
# Steps Over the Day

hourlyActivity_merged$DayofWeek <- wday(as_date(hourlyActivity_merged$ActivityHour))
hourlyActivity_merged$DayofWeek = case_when(
  hourlyActivity_merged$DayofWeek == 1 ~ 'Sunday',
  hourlyActivity_merged$DayofWeek == 2 ~ 'Monday',
  hourlyActivity_merged$DayofWeek == 3 ~ 'Tuesday',
  hourlyActivity_merged$DayofWeek == 4 ~ 'Wednesday',
  hourlyActivity_merged$DayofWeek == 5 ~ 'Thursday',
  hourlyActivity_merged$DayofWeek == 6 ~ 'Friday',
  hourlyActivity_merged$DayofWeek == 7 ~ 'Saturday',
)

dailyActivity_merged$DayofWeek <- wday(as_date(dailyActivity_merged$ActivityDate))
dailyActivity_merged$DayofWeek = case_when(
  dailyActivity_merged$DayofWeek == 1 ~ 'Sunday',
  dailyActivity_merged$DayofWeek == 2 ~ 'Monday',
  dailyActivity_merged$DayofWeek == 3 ~ 'Tuesday',
  dailyActivity_merged$DayofWeek == 4 ~ 'Wednesday',
  dailyActivity_merged$DayofWeek == 5 ~ 'Thursday',
  dailyActivity_merged$DayofWeek == 6 ~ 'Friday',
  dailyActivity_merged$DayofWeek == 7 ~ 'Saturday',
)


hourlyActivity_merged$HourofDay <- hour(hourlyActivity_merged$ActivityHour)

ggplot(data = hourlyActivity_merged, aes(x=HourofDay,y=StepTotal,color=c('red'))) + geom_point() +geom_smooth(color='blue')

hourlyActivity_merged %>%
  group_by(DayofWeek) %>%
  summarise(Steps = mean(StepTotal)) %>%
  ggplot() + geom_point(aes(x=DayofWeek,y=Steps,color=DayofWeek,size=1.5))+theme(legend.position = "none")

ggplot(data = hourlyActivity_merged, aes(x=HourofDay,y=StepTotal,color=c('red'))) + geom_point() +geom_smooth(color='blue')


#Steps per day using hours
hourlyActivity_merged %>%
  group_by(ActivityHour) %>%
  summarise(meanSteps = mean(StepTotal),DayofWeek) %>%
  ggplot(aes(x=ActivityHour,y=meanSteps,fill=DayofWeek)) + geom_bar(stat = 'identity')


StepsByDay <- dailyActivity_merged %>%
  group_by(ActivityDate,DayofWeek) %>%
  summarise(meanSteps = mean(TotalSteps))
min(StepsByDay$ActivityDate)
ggplot(StepsByDay, aes(x=ActivityDate,y=meanSteps,fill=DayofWeek)) +geom_bar(stat = 'identity')+ 
  geom_rect(inherit.aes=FALSE,aes(xmin=min(StepsByDay$ActivityDate)-100000, xmax=max(StepsByDay$ActivityDate)+100000, ymin=7500, ymax=10000), alpha=0.01, fill="red") +
  annotate('text',x=median(StepsByDay$ActivityDate),y=9000,label='Recommended Steps',color='red')

StepsByDay %>%
  group_by(DayofWeek) %>%
  summarise(meanSteps2 = mean(meanSteps)) %>%
  ggplot(aes(x=DayofWeek,y=meanSteps2,fill=DayofWeek)) +geom_bar(stat = 'identity')+ ylim(0,10500)+
  annotate('text',x=median(StepsByDay$DayofWeek),y=9000,label='Recommended Steps',color='black') +
  geom_hline(yintercept = 7500,linetype='dotted', color = "black", size=1) +
  geom_hline(yintercept = 10000,linetype='dotted', color = "black", size=1)
  

StepsByHour <- hourlyActivity_merged %>%
  group_by(HourofDay,DayofWeek) %>%
  summarise(meanSteps = mean(StepTotal))


ggplot(data = StepsByHour, aes(x=HourofDay,y=meanSteps,color=DayofWeek)) +
  geom_smooth() +
  geom_line() +
  facet_wrap(~DayofWeek)+
  labs(x='Hour of Day',y='Mean Steps',title='Average Steps per Weekday')



typeof(dailyActivity_merged$ActivityDate)
typeof(hourlyActivity_merged$ActivityHour)

ggplot(data = hourlyActivity_merged,aes(x=ActivityHour, y=StepTotal,color=DayofWeek)) + geom_point() +
  geom_hline(yintercept = 450,linetype='dotted', color = "black", size=1) +
  geom_hline(yintercept = 670,linetype='dotted', color = "black", size=1)
  




# Plotting Some Data
ggplot(data=sleepDay_merged) + geom_boxplot(mapping = aes(x = SleepDay,y = TotalMinutesAsleep, color=DayofWeek, fill=DayofWeek))+ coord_flip()

ggplot(sleepDay_merged, aes(x=SleepDay, y=TotalMinutesAsleep)) + 
  geom_boxplot()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), 
       title=element_text(size=25,face="bold"), legend.text=element_text(size=20), 
       axis.text.x = element_text(angle = -90)) + guides(fill=guide_legend(title="Type")) + coord_flip()


aggregate(dailyActivity_merged[,c('VeryActiveMinutes','FairlyActiveMinutes','LightlyActiveMinutes','SedentaryMinutes')], list(dailyActivity_merged$Id), mean)

ggplot(data = hourlyActivity_merged, aes(x=HourofDay,y=StepTotal)) + geom_histogram(stat = 'identity',fill='red') +
  geom_vline(xintercept = 9,linetype='dotted',color='Blue',size=1.5) +
  geom_vline(xintercept = 17,linetype='dotted',color='Blue',size=1.5)


StepsByDay$meanSteps
nrow(filter(StepsByDay, meanSteps>7500))/length(StepsByDay$meanSteps)


UserActiveType <- dailyActivity_merged[,c('Id','VeryActiveMinutes','FairlyActiveMinutes','LightlyActiveMinutes','SedentaryMinutes')] %>%
  group_by(Id) %>%
  mean()

ggplot(data = dailyActivity_merged, aes(x=SedentaryMinutes,y=Calories,color =Id))+
  geom_point() +geom_smooth()

ggplot(data = hourlyActivity_merged, aes(x=StepTotal,y=DayofWeek)) + geom_point() +geom_smooth()
