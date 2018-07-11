library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(data.table)
library(sugrrants)

data<-read.csv(file.choose(),header = TRUE)
dates <- as.Date(data$rank_date, 
                 format = '%Y-%m-%d')
data2 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("2018-01-18"), 
                 as.Date("2018-06-07")))
data2

#filtering out confederations for 2018

#filtering for UEFA confederation 
UEFA <- subset(data2,confederation == "UEFA")
UEFA

data_sorted <-ddply(UEFA,.(country_full),
                    summarize,sum=sum(previous_points))

newdata <- data_sorted[order(-data_sorted$sum), ]
newdata

#filtering for AFC confederation 
AFC <- subset(data2,confederation == "AFC")
AFC

data_sorted <-ddply(AFC,.(country_full),
                    summarize,sum=sum(previous_points))

newdata1 <- data_sorted[order(-data_sorted$sum), ]
newdata1
#filtering for OFC confederation 
OFC <- subset(data2,confederation == "OFC")
OFC

data_sorted <-ddply(OFC,.(country_full),
                    summarize,sum=sum(previous_points))

newdata2 <- data_sorted[order(-data_sorted$sum), ]
newdata2

