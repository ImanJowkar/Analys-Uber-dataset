# Clear work directory--------------------
rm(list = ls())

# Set Working Directory------------------
setwd('K:/Hoshmandan/R/1- Introduction to R/final project/')


# Load Data ----------------------------

Jul_data <- read.csv("uber-raw-data-jul14.csv", header = T)
Aug_data <- read.csv("uber-raw-data-aug14.csv", header = T)
Sep_data <- read.csv("uber-raw-data-sep14.csv", header = T)
View(Jul_data)
str(Jul_data)
dim(Jul_data)
dim(Aug_data)
dim(Sep_data)



data = rbind(Jul_data, Aug_data, Sep_data)
View(data)
data$Date.Time <- as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
class(data$Date.Time)


data$Time <- format(data$Date.Time, format = "%H:%M:%S")
View(data)

library(lubridate)


data$day = factor(day(data$Date.Time))
data$month = factor(month(data$Date.Time, label = T))
# data$year = factor(year(April_data$Date.Time))
data$dayofweek = factor(wday(data$Date.Time, label = T, abbr = F))
data$hour = factor(hour(hms(data$Time)))
# View(data)
head(data)



# Hourly Demand
hour_data = as.data.frame(table(data$hour))
head(hour_data)
names(hour_data) = c('Hour', 'Demand')
head(hour_data)


library(ggplot2)
library(ggthemes)
library(scales)

ggplot(hour_data, aes(x = Hour, y = Demand, group = 1)) + 
  geom_bar(stat = 'identity', fill = 'blue',col = 'white') +
  geom_point() +
  scale_y_continuous(labels = comma) + 
  annotate("text", label = "Pick Demand", x = 17, y = 198000, color = "red", size = 6) +
  ylab('Number of Demand') +
  theme_clean() + 
  ggtitle('Hourly Demand')



hour_month_demand = table(data$hour, data$month)
hour_month_demand


hour_month_demand = as.data.frame(hour_month_demand)
# View(hour_month_demand)
head(hour_month_demand)
names(hour_month_demand) = c('Hour','Month','Demand')

ggplot(hour_month_demand, aes(x = Hour, y = Demand, fill = Month)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_continuous(labels = comma) + 
  annotate("text", label = "Pick Demand", x = 17, y = 198000, color = "red", size = 6) +
  theme_base() + 
  ggtitle('Hourly and monthly Demand')




# weekly demand

Weekly_demand = as.data.frame(table(data$dayofweek))
head(Weekly_demand)
names(Weekly_demand) = c('Day','Demand')
Weekly_demand

red.bold.italic.text <- element_text(face = "bold.italic", color = "red", size = 16)
black.bold.italic.16.text <- element_text(face = "bold.italic", color = "black", size = 10)
ggplot(data = Weekly_demand, aes(x = Day, y = Demand)) + 
  geom_bar(stat = 'identity', fill = 'blue', col = 'red') + 
  ggtitle('Weekly Demand') + 
  theme_bw() + 
  theme(title = red.bold.italic.text, axis.title = red.bold.italic.text) + 
  theme(axis.text = black.bold.italic.16.text)



Montly_Demand = as.data.frame(table(data$month))
head(Montly_Demand)


names(Montly_Demand) = c('Month','Demand')

p = ggplot(data = Montly_Demand, aes(x = Month, y = Demand)) + 
  geom_bar(stat = 'identity', col = 'red', fill = 'blue') + 
  theme_bw()

p
