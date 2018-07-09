library(dplyr)
library(ggplot2)
library(formattable)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
counrty_select<-fifa%>%filter(country_abrv=="BRA")%>%select(rank_date,rank,total_points,rank_change)%>%arrange(desc(total_points))%>%slice(1:15)
#selecting the columns to display 
slect_reduce<-counrty_select[,c(1,2,3,4)]
#change column names 
colnames(slect_reduce)<-c("DATE","RANK","POINTS","CHANGE")
#draw table
widget_formattable<-formattable(slect_reduce)
