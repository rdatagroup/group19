library(dplyr)
library(ggplot2)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
#selecting country's data from 1993
datae2<-
country_data<-fifa%>%filter(filtered_date<-substring(rank_date,6)=="1993")%>%select(rank,previous_points,rank_date,country_abrv)

#substring(date_data,6)
#graph for Germany team
