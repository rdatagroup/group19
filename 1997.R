library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(data.table)

data<-read.csv(file.choose(),header = TRUE)
dates <- as.Date(data$rank_date, 
                 format = '%Y-%m-%d')

data2 <-data  %>%
  
  select(rank,country_full,
         previous_points,rank_change,confederation,rank_date)%>%
  filter(between(dates, 
                 as.Date("1997-02-27"), 
                 as.Date("1997-12-23")))
data2

#filtering out confederations for 1997
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
                    summarize,Points_OFC=sum(previous_points))

newdata2 <- data_sorted[order(-data_sorted$Points_OFC), ]
newdata2
ggplot(newdata2,aes(x=country_full,y=Points_OFC))+geom_col()+coord_flip()


#filtering for CAF confederation 
CAF <- subset(data2,confederation == "CAF")
CAF

data_sorted <-ddply(CAF,.(country_full),
                    summarize,sum=sum(previous_points))

newdata3 <- data_sorted[order(-data_sorted$sum), ]
newdata3
ggplot(newdata3,aes(x=country_full,y=sum))+geom_col()+coord_flip()
#filtering for  CONCACAFconfederation 
CONCACAF <- subset(data2,confederation == "CONCACAF")
CONCACAF

data_sorted <-ddply( CONCACAF,.(country_full),
                     summarize,sum=sum(previous_points))

newdata4 <- data_sorted[order(-data_sorted$sum), ]
newdata4
#filtering for  CONMEBOL confederation 
CONMEBOL <- subset(data2,confederation == "CONMEBOL")
CONMEBOL
#total perfomnce in Year 1997

data_sorted_1997 <-ddply(data2,.(country_full),
                         summarize,sum2=sum(previous_points))
data_sorted_1997
#sort in descending order
desc <- data_sorted_1997[order(-data_sorted_1997$sum2), ]
desc