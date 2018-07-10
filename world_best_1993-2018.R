library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(data.table)

data<-read.csv(file.choose(),header = TRUE)

#total perfomnce in Year 2018

data<-ddply(data,.(country_full),
                        summarize,sum=sum(previous_points))
data
#sort in descending order
desc <- data[order(-data$sum), ]
desc
