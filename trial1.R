library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(data.table)

data <- read.csv("fifa_ranking.csv")
dates <- as.Date(data$rank_date, 
                 format = '%Y-%m-%d')

data2 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("1993-08-08"), 
                 as.Date("1993-12-23")))
data2

#filtering out confederations for 1993
#filtering for UEFA confederation 
UEFA <- subset(data2,confederation == "UEFA")
UEFA

data_sorted <-ddply(UEFA,.(country_full),
                    summarize,sum=sum(previous_points))

newdata <- data_sorted[order(-data_sorted$sum), ]
newdata
# plotting the data 
barplot(as.matrix(newdata), beside=T , legend.text=T, 
        col=c("red" , "green", 
             "blue"), ylim=newdata$sum, ylab="height")
