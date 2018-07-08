library(dplyr)
library(ggplot2)
library(formattable)
data<-read.csv(file.choose(),header = TRUE)
fifa<-as.data.frame(data)
tabl_select<-fifa%>%filter(confederation=="CAF",rank_date=="6/7/2018")%>%select(rank,country_full,total_points)%>%arrange(desc(total_points))%>%slice(1:15)
#selecting the columns to display
choosen_data<-tabl_select[,c(1,2,3)]
#The display the table

#change column names 
colnames(choosen_data)<-c("RANK","COUNTRY","POINTS")
widget_formattable<-formattable(choosen_data)