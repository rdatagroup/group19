library(dplyr)
library(ggplot2)
library(formattable)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
world_data<-fifa%>%filter(rank<=15,rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))%>%slice(1:15)
#selecting the columns to display
best_data<-world_data[,c(1,2,3)]
#The display the table

#change column names 
colnames(best_data)<-c("RANK","COUNTRY","POINTS")
widget_formattable<-formattable(best_data)
widget_formattable