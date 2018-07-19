library(dplyr)
library(ggplot2)
library(formattable)
library(DT)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
world_data<-fifa%>%filter(rank<=15,rank_date=="6/7/2018")%>%select(rank,country_full,rank_change,total_points)%>%arrange(desc(total_points))%>%slice(1:15)
#selecting the columns to display

widget_formattable<-formattable(world_data,list(rank=color_tile("red","green")))
  
#widget_formattable