#Contains information licensed under the Open Government Licence - Nova Scotia
#Accessed May 31st, 2017 8:58pm MST
#License https://novascotia.ca/opendata/licence.asp
#Dataset and further information: data.novascotia.ca/Public-Service/Nova-Scotia-Government-Employee-Absenteeism/3kpf-veux
setwd("G:/Personal/R coding/RScripts/Govtabsetism")

library(RSocrata); library(tidyverse); library(lubridate);library(fpp2);library(forecast);
library(urca); library(tseries)

url <- "https://data.novascotia.ca/resource/dy4w-s8zb.json"

# rawdata <- read.socrata(url)
# write.csv(rawdata,file = "NSgovAbsentData.csv")

############################ Data Cleaning
rawdata <- read.csv("NSgovAbsentData.csv",stringsAsFactors = FALSE)
tidydata <- rawdata[-1]
tidydata$absence_hours <- as.numeric(tidydata$absence_hours)
tidydata$age_cohort_on_absence_date <- as.factor(tidydata$age_cohort_on_absence_date)
tidydata$absence_type_category <- gsub("_"," ",tidydata$absence_type_category)

############################ EDA

#Absent days by gender
absHoursGender <- tidydata %>%
    group_by(gender) %>%
    summarise(
        count = n()) %>%
    ggplot(mapping = aes(x = gender, y = count)) +
    geom_bar(stat = "identity")+
    theme_classic() + 
    geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
    labs(x = "Gender", y="")+
    ggtitle("Days Absent by Gender")+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

#Days absent by age group
absAgeRange <- tidydata %>%
    group_by(age_cohort_on_absence_date) %>%
    summarise(
        count = n()) %>%
    ggplot(mapping = aes(x = age_cohort_on_absence_date, y = count)) +
    geom_bar(stat = "identity")+
    geom_text(aes(label = count), hjust = 1.6, color = "white", size = 3.5)+
    theme_classic()+
    labs(x = "Age Group", y = "", title = "Days Absent by Age group")+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x=element_blank())+
    coord_flip()

#Days absent by type cateogry and gender
absTypeCategory <- tidydata %>%
    group_by(absence_type_category, gender) %>%
    summarize(
        count = n()) %>%
    ggplot(mapping=aes(x=absence_type_category, y = count, fill = gender))+
    geom_bar(position = "dodge",stat = "identity")+
    theme_classic()+
    labs(x = "Absence Category", y = "", title = "Days Absent by Type Category and Gender")+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())+
    coord_flip()
               
#Days absent by Age group and type category
absAgeType <- tidydata %>%
    group_by(age_cohort_on_absence_date, absence_type_category) %>%
    summarize(
        count = n()) %>%
    ggplot(mapping = aes(x = age_cohort_on_absence_date, y = count, fill = absence_type_category))+
    geom_bar(stat = "identity", colour = "black")+
    guides(fill= guide_legend(reverse = TRUE))+
    theme_classic()+
    labs(x = "Age Group", y = "", title = "Days Absent by Age Group and Type Category", fill = "Type Category")
absAgeType

#Plot absences over time
absTime <- tidydata %>%
    group_by(absence_date) %>%
    summarize(
        count = n()) %>%
    ggplot(mapping = aes(x = absence_date, y = count))+
    geom_point()+
    labs(x = "Date", y = "", title = "Absences Over Time")+
    theme_classic()
absTime    

#Take a closer look at the absenses over time, by looking at the distributions over each month
absMonth <- tidydata %>%
    mutate(
        month = month(absence_date, label = TRUE)) %>%
    group_by(month) %>%
    summarize(
        count= n()) %>%
    ggplot(mapping = aes(x = month, y=as.factor(count)))+
    geom_bar(stat = "identity")+
    labs(x = "Month", y = "", title = "Absences by Month")+
    theme_classic()+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
absMonth

#Days absent by employee type
absEmpType <- tidydata %>%
    group_by(employee_type) %>%
    summarise(
        count = n()) %>%
    ggplot(mapping = aes(x = employee_type, y = count))+
    geom_bar(stat= "identity")+
    labs(y = "", x = "Employee Type", title = "Absences by Employee Type")+
    coord_flip()+
    theme_classic()+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
absEmpType


#Absensces by Day of the Week
absDayWeek <- tidydata %>%
  mutate(
    dayOfWeek = wday(tidydata$absence_date,label = TRUE, abbr = TRUE)) %>%
  select(dayOfWeek) %>%
  group_by(dayOfWeek) %>%
  summarize(
    count = n()) %>%
  ggplot(mapping= aes(x=dayOfWeek, y= count)) +
  geom_bar(stat = "identity")+
  labs(y="",x = "Day of Week", title = "Absences by Day of Week")+
  theme_classic()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
absDayWeek

############################ Time Series Modeling
timeseries <- tidydata %>%
    select(absence_date,absence_hours) %>%
    group_by(absence_date) 

timeseries <- ts(tidydata[,2], start = c(2014,1,1),end = c(2017,3,31),frequency = 365)
autoplot(timeseries)+
    ggtitle("Absentee Hours by Day")+
    xlab("Time")+ ylab("Hours")

#Decompose into compontents, and view it
plot(decompose(timeseries))

#Check for stationarity - which is needed to model in most functions.  p-value < 0.05 = stationary
#Stationarity means mean and variance not changing over time
adf.test(timeseries)

#Check for autocorrelations.  Used to identify the MA part of Arima model
acf(timeseries) #plot = FALSE to just get number, lag.max = 20, gives you correlation between first 20 time points

#Check for partial autocorrelations.  Used to identify the AR part of arima model
pacf(timeseries)

#Put both acf and pacf into one chart. From forecast pkg
tsdisplay(timeseries)

#Doesn't look like white noise
acfplot <- ggAcf(timeseries)
acfplot

#Forecast Model 1 - Arima
myarima <- auto.arima(timeseries, ic = "aic", trace = TRUE)
fcst <- forecast(myarima,h = 100)
plot(fcst)
acf(fcst$residuals) #Model can't be improved because no significant autocorrelation in residuals

#Forecast Model 2 - HoltsWinters - Seems like a far more reasonable model
myhw <- HoltWinters((timeseries))
fcsthw <- forecast(myhw, h=365)
plot(fcsthw)
plot(fcsthw$residuals)

